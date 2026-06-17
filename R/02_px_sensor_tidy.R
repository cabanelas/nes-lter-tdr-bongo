###############################################################
##  NES-LTER Bongo PX Sensor: Tidy & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_px_sensor_tidy.R
##  Author:  Alexandra Cabanelas
##
##  SIMRAD PX MultiSensor 
##  Purpose: Read raw PX Multisensor (SR15) measurements CSV files,
##           assign cruise/station/cast metadata via elog,
##           filter out Isaacs-Kidd Midwater Trawl (IKMT/MWT) PX data,
##           trim to elog deploy/recover windows, label downcast/upcast,
##           and export cleaned bongo-only PX sensor data.
##
##  Input: data/raw/  
##          px_sensor/{cruise}_px_sensor/*.csv
##          elog_zoop_tows_thruAR99_2026-04-14.csv 
##            (from nes-lter-api-pulls.Rproj; 01_elog_pull.R)
##          all-nes-lter-bongologs-20260526.csv
##            (from nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R)
##  NES-LTER API2 (https://github.com/WHOIGit/nes-lter-api-2/wiki) for MWT elog
##
##  Output:  data/processed/px_data_bongo_YYYY-MM-DD.rds
##           data/processed/nes-lter-bongo-px.csv
##           data/processed/px-column-headers.csv
##           figures/px_sensor_profiles_check.pdf
##           figures/px_sensor_profiles_trimmed.pdf
##           figures/px_sensor_profiles_labeled.pdf           
###############################################################

## ------------------------------------------ ##
# PxSensor data available starting AE2426
# 2024 = AE2426
# 2025 = EN727, AR88, AR92, AR95
# 2026 = AR99, ***need to add HRS2601***
## ------------------------------------------ ##
# PxSensor is generally not used at MVCO nor L1 -- too shallow 

# AE2426 L2, L4, L6, L7, L8                          
# AR88   L2, L3, L4, L5, L6, L7, L8, L9, L10, L11    
# AR92   L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11
# AR95   L1, L2, L3, L4, L5, L7, L8, L9, L10, L11   
# AR99   L3, L4, L5, L6, L7, L8, L10                 
# EN727  L3, L4, L5, L6, L7, L8, L10, L11   

## AR99 L11 B9: PX sensor did not record; PX sensor logsheet max depth ~199m

## ------------------------------------------ ##
##  Packages               ----
## ------------------------------------------ ##
library(tidyverse)
library(here)
library(xml2)

source(here("R", "00_helpers.R"))

## ------------------------------------------ ##
##  Read PX sensor data     ----
## ------------------------------------------ ##
px_files <- list.files(
  here("data", "raw", "px_sensor"),
  recursive = TRUE,
  full.names = TRUE
)

px_csv_files <- px_files[grepl("measurements\\.csv$", px_files)]

## ------------------------------------------ ##
##  File structure             ----
## ------------------------------------------ ##
message("Total PX files found: ", length(px_files))
message("  CSV (measurements): ", length(px_csv_files))
message("  XML (telemetry):    ", sum(grepl("\\.xml$", px_files)))

## folder structure summary
tibble(path = px_csv_files) %>%
  mutate(
    cruise = path %>%
      str_remove(fixed(here("data", "raw", "px_sensor"))) %>%
      str_remove("^[\\/]") %>%
      str_extract("^[^\\/]+") %>%
      str_remove("_px_sensor$")
  ) %>%
  count(cruise, name = "n_csv_files") %>%
  print()

## full folder structure
tibble(path = px_csv_files) %>%
  mutate(
    rel_path = path %>%
      str_remove(fixed(here("data", "raw", "px_sensor"))) %>%
      str_remove("^[\\/]") %>%
      dirname()  # just the folder, not the filename
  ) %>%
  distinct(rel_path) %>%
  arrange(rel_path) %>%
  pull(rel_path) %>%
  walk(~message("  ", .))

## --- read files --- ##
# skip only if file has 2 columns (truly no sensor data) [2 files]
px_data <- map_dfr(px_csv_files, function(f) {
  cruise <- f %>%
    str_remove(fixed(here("data", "raw", "px_sensor"))) %>%
    str_remove("^[\\/]") %>%
    str_extract("^[^\\/]+") %>%
    str_remove("_px_sensor$")
  
  cast_start <- basename(f) %>%
    str_extract("^\\d{8}_\\d{6}") %>%
    ymd_hms(tz = "UTC")
  
  first_line <- readLines(f, n = 1)
  has_header <- str_detect(first_line, "Date|date")
  n_cols_file <- length(str_split(first_line, ";")[[1]])
  
  ## skip truly empty files (only date + time columns)
  if (n_cols_file <= 2) {
    message("  skipping (no sensor columns): ", basename(f))
    return(NULL)
  }
  
  if (has_header) {
    df <- read_delim(f, delim = ";", show_col_types = FALSE,
                     col_types = cols(.default = "c")) %>%
      rename_with(~case_when(
        str_detect(., regex("depth", ignore_case = TRUE))  ~ "depth_m",
        str_detect(., regex("temp",  ignore_case = TRUE))  ~ "temp_C",
        str_detect(., regex("date",  ignore_case = TRUE))  ~ "date",
        str_detect(., regex("time",  ignore_case = TRUE))  ~ "time",
        TRUE ~ .
      ))
  } else {
    ## no header - file 19 pattern: date;time;temp;depth
    df <- read_delim(f, delim = ";", show_col_types = FALSE,
                     col_names = c("date", "time", "temp_C", "depth_m"),
                     col_types = cols(.default = "c"))
  }
  
  df %>%
    mutate(
      depth_m    = as.numeric(depth_m),
      temp_C     = as.numeric(temp_C),
      cruise     = cruise,
      cast_start = cast_start,
      date_time  = ymd_hms(paste(date, time), tz = "UTC"),
      ## fix 12-hour clock parsing: sensor records without AM/PM
      ## afternoon casts (12:00-23:59) get parsed as AM (00:00-11:59)
      ## detected by comparing to cast_start from filename (always 24hr correct)
      date_time  = if_else(
        date_time < cast_start - hours(6),
        date_time + hours(12),
        date_time
      )
    ) %>%
    filter(!is.na(depth_m)) %>%   # drop rows before sensor started recording
    select(cruise, cast_start, date_time, depth_m, temp_C)
})
# checked warning; can ignore

glimpse(px_data)

px_data %>%
  group_by(cruise, cast_start) %>%
  summarise(
    n_obs     = n(),
    max_depth = max(depth_m, na.rm = TRUE),
    duration  = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    .groups   = "drop"
  ) %>%
  arrange(cruise, cast_start) %>%
  print(n = Inf)
# some MidWaterTrawl data in here (deep depths)

## ------------------------------------------ ##
##  Check px sensor metadata     ----
## ------------------------------------------ ##
# these are in the telemetry xml files
xml_files <- px_files[grepl("\\.xml$", px_files)]

xml_info <- map_dfr(xml_files, function(f) {
  x <- read_xml(f)
  tibble(
    file        = basename(f),
    cruise      = f %>%
      str_remove(fixed(here("data", "raw", "px_sensor"))) %>%
      str_remove("^[\\/]") %>%
      str_extract("^[^\\/]+") %>%
      str_remove("_px_sensor$"),
    source_name = xml_find_first(x, ".//source_name") %>% xml_text(),
    sensor_type = xml_find_first(x, ".//type")        %>% xml_text(),
    update_rate = xml_find_first(x, ".//update_rate") %>% xml_text(),
    offset_1    = xml_find_first(x, ".//offset_1")    %>% xml_text(),
    offset_2    = xml_find_first(x, ".//offset_2")    %>% xml_text(),
    offset_3    = xml_find_first(x, ".//offset_3")    %>% xml_text()
  )
})

## check unique values
xml_info %>% count(source_name, sensor_type, update_rate)
# these are the same in all cruises

## check if any hardware offsets set
xml_info %>%
  filter(offset_1 != "" | offset_2 != "" | offset_3 != "") %>%
  select(cruise, file, offset_1, offset_2, offset_3)
# no offsets 

read_xml(xml_files[1]) %>% {
  tibble(
    system_name    = xml_find_first(., ".//system_name")       %>% xml_text(),
    user           = xml_find_first(., ".//user")              %>% xml_text(),
    date           = xml_find_first(., ".//date")              %>% xml_text(),
    source_name    = xml_find_first(., ".//source_name")       %>% xml_text(),
    source_type    = xml_find_first(., ".//source_type")       %>% xml_text(),
    sensor_type    = xml_find_first(., ".//type")              %>% xml_text(),
    update_rate    = xml_find_first(., ".//update_rate")       %>% xml_text(),
    basic_type_1   = xml_find_first(., ".//basic_type_sensor_1") %>% xml_text(),
    basic_type_2   = xml_find_first(., ".//basic_type_sensor_2") %>% xml_text(),
    basic_type_3   = xml_find_first(., ".//basic_type_sensor_3") %>% xml_text(),
    variable_ch1   = xml_find_first(., ".//variable_channel_1") %>% xml_text(),
    variable_ch2   = xml_find_first(., ".//variable_channel_2") %>% xml_text(),
    variable_ch3   = xml_find_first(., ".//variable_channel_3") %>% xml_text()
  )
}
rm(xml_info, xml_files)

## ------------------------------------------ ##
##  Add station and cast info      ----
## ------------------------------------------ ##
# px sensor has datetime but no cast or station 

### --- Load elog data --- ###
# elog data fixed and created in nes-lter-api-pulls.Rproj
# 01_elog_pull.R
# https://github.com/cabanelas/nes-lter-api-pulls
elog <- read_csv(file.path("data", "raw",
                           "elog_zoop_tows_thruAR99_2026-04-14.csv"))

## px cast_start must fall within elog deploy-recover window
## 2-min grace period allows for px logger starting slightly before elog deploy

# these are bongo only elog entries
elog_bongo_px_window <- elog %>%
  filter(cruise %in% unique(px_data$cruise)) %>%
  select(cruise, station, cast, action, elog_time = datetime8601) %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  pivot_wider(names_from = action, values_from = elog_time,
              names_prefix = "elog_") %>%
  select(cruise, station, cast, elog_deploy, elog_recover) %>%
  filter(!is.na(elog_deploy), !is.na(elog_recover))

## cruise-specific grace periods (in minutes)
elog_bongo_px_window <- elog_bongo_px_window %>%
  mutate(grace = if_else(cruise == "EN727", 40, 2))
# EN727: px sensor started 11-34 min before elog deploy = using 40 min buffer
# other cruises get 2 min buffer

px_cast_meta <- px_data %>%
  distinct(cruise, cast_start) %>%
  left_join(elog_bongo_px_window, by = "cruise", relationship = "many-to-many") %>%
  filter(cast_start >= elog_deploy - minutes(grace),
         cast_start <= elog_recover) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(as.numeric(difftime(cast_start, elog_deploy, units = "mins"))), 
            n = 1) %>%
  ungroup() %>%
  select(cruise, cast_start, station, cast, elog_deploy, elog_recover)

## check coverage
cat("Total px cast_starts: ", n_distinct(paste(px_data$cruise, px_data$cast_start)), "\n")
cat("Matched to bongo:     ", nrow(px_cast_meta), "\n")

## unmatched; likely MWT or truly unidentifiable
px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start")) %>%
  arrange(cruise, cast_start) %>%
  print(n = Inf)

rm(elog_bongo_px_window, elog)

## ------------------------------------------ ##
##  Filter out Isaacs-Kidd Midwater Trawl  ----
## ------------------------------------------ ##
## there are Isaacs-Kidd Midwater Trawl px sensor data in here!
# delete those that arent bongo tows
# get elog from API2 to get Isaacs-Kidd Midwater Trawl 

##  QC: verify unmatched are MWT     ----
elog_mwt_window <- map_dfr(tolower(unique(px_data$cruise)), function(cr) {
  url <- paste0("https://nes-lter-api.whoi.edu/api/events/", cr, ".csv")
  message("  fetching: ", cr)
  df <- safe_read_csv(url)
  if (is.null(df)) return(NULL)
  df %>% mutate(cruise = toupper(cr))
}) %>%
  filter(Instrument == "Isaacs-Kidd Midwater Trawl",
         Action %in% c("deploy", "recover")) %>%
  select(cruise, station = Station, cast = Cast,
         action = Action, mwt_time = dateTime8601) %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  pivot_wider(names_from = action, values_from = mwt_time,
              names_prefix = "mwt_") %>%
  select(cruise, station, cast, mwt_deploy, mwt_recover) %>%
  filter(!is.na(mwt_deploy), !is.na(mwt_recover))

## classify unmatched as MWT using containment
px_mwt <- px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start")) %>%
  left_join(
    elog_mwt_window %>%
      # AR92 MWT: px started ~10 min before MWT deploy
      # giving it 15 min buffer; the rest get 2 min buffer
      mutate(grace = if_else(cruise == "AR92", 15, 2)),
    by = "cruise", relationship = "many-to-many"
  ) %>%
  filter(cast_start >= mwt_deploy - minutes(grace),
         cast_start <= mwt_recover) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(as.numeric(difftime(cast_start, mwt_deploy, units = "mins"))), 
            n = 1) %>%
  ungroup() %>%
  mutate(instrument = "MWT") %>%
  select(cruise, cast_start, station, cast, instrument)

## anything that matched neither bongo nor MWT
truly_unmatched <- px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start")) %>%
  anti_join(px_mwt, by = c("cruise", "cast_start"))

cat("Unmatched (not bongo):  ", nrow(px_data %>% distinct(cruise, cast_start) %>%
                                       anti_join(px_cast_meta, 
                                                 by = c("cruise", "cast_start"))), "\n")
cat("Truly unidentified:     ", nrow(truly_unmatched), "\n")
cat("Total px cast_starts:   ", n_distinct(paste(px_data$cruise, px_data$cast_start)), "\n")
cat("Matched to bongo:       ", nrow(px_cast_meta), "\n")
cat("Matched to MWT:         ", nrow(px_mwt), "\n")

if (nrow(truly_unmatched) > 0) {
  cat("\n--- truly unidentified (not bongo, not MWT) ---\n")
  print(truly_unmatched)
}

## check what was happening in elog around these unidentified cast_starts
elog_all_instruments <- map_dfr(tolower(unique(truly_unmatched$cruise)), 
                                function(cr) {
  url <- paste0("https://nes-lter-api.whoi.edu/api/events/", cr, ".csv")
  message("  fetching: ", cr)
  df <- safe_read_csv(url)
  if (is.null(df)) return(NULL)
  df %>% mutate(cruise = toupper(cr))
})

truly_unmatched %>%
  left_join(elog_all_instruments, by = "cruise", 
            relationship = "many-to-many") %>%
  mutate(diff_mins = as.numeric(difftime(cast_start, dateTime8601, 
                                         units = "mins"))) %>%
  filter(abs(diff_mins) <= 60) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(diff_mins), n = 5) %>%
  ungroup() %>%
  select(cruise, cast_start, Instrument, Action, Station, Cast, 
         dateTime8601, diff_mins) %>%
  arrange(cruise, cast_start, abs(diff_mins)) %>%
  print(n = Inf)

truly_unmatched
rm(elog_mwt_window, truly_unmatched)

## ------------------------------------------ ##
##  Manual exclusions & assignments         ----
## ------------------------------------------ ##
## AR92 2025-08-18 11:30:24  = IKMWT Tow7; missing recover in elog
## AR99 2026-01-15 00:00:00  = bad
## EN727 2025-01-27 00:00:01 = bongo cast L7 B14
##   cast_start (00:00) is misleading; deploy was 04:43 UTC within file range
px_manual_mwt <- tibble(
  cruise     = "AR92", cast_start = as.POSIXct("2025-08-18 11:30:24", tz = "UTC"),
  station    = "L7", cast = "Tow7"
)

px_manual_bongo <- tribble(
  ~cruise, ~cast_start,                                 ~station, ~cast, ~elog_deploy,                                 ~elog_recover,
  "EN727", as.POSIXct("2025-01-27 00:00:01", tz = "UTC"), "L7",   "B14", as.POSIXct("2025-01-27 04:43:44", tz = "UTC"), as.POSIXct("2025-01-27 05:00:52", tz = "UTC"),
  "AR88",  as.POSIXct("2025-04-25 03:00:45", tz = "UTC"), "L4",   "B2",  as.POSIXct("2025-04-25 04:44:00", tz = "UTC"), as.POSIXct("2025-04-25 04:51:17", tz = "UTC"),
  "EN727", as.POSIXct("2025-01-26 02:16:17", tz = "UTC"), "L11",  "B6",  as.POSIXct("2025-01-26 05:01:30", tz = "UTC"), as.POSIXct("2025-01-26 05:29:41", tz = "UTC")
)

## build final bongo metadata: auto-matched + manual bongo
px_bongo_cast_meta_final <- bind_rows(
  px_cast_meta,
  px_manual_bongo
)

## ------------------------------------------ ##
##  Filter to bongo only + add metadata     ----
## ------------------------------------------ ##
px_data_bongo <- px_data %>%
  ## drop AR99 bad file
  filter(!(cruise == "AR99" & cast_start == as.POSIXct("2026-01-15 00:00:00", 
                                                       tz = "UTC"))) %>%
  ## keep only bongo cast_starts
  semi_join(px_bongo_cast_meta_final, by = c("cruise", "cast_start")) %>%
  ## add station/cast/elog times
  left_join(
    px_bongo_cast_meta_final %>% 
      select(cruise, cast_start, station, cast, elog_deploy, elog_recover),
    by = c("cruise", "cast_start")
  )

cat("Total px cast_starts:      ", n_distinct(paste(px_data$cruise, px_data$cast_start)), "\n")
cat("Matched to bongo (auto):   ", nrow(px_cast_meta), "\n")
cat("Matched to bongo (manual): ", nrow(px_manual_bongo), "\n")
cat("Matched to MWT (auto):     ", nrow(px_mwt), "\n")
cat("Matched to MWT (manual):   ", nrow(px_manual_mwt), "\n")
cat("px_data_bongo rows:        ", nrow(px_data_bongo), "\n")
cat("unique bongo casts:         ", n_distinct(paste(px_data_bongo$cruise, px_data_bongo$cast)), "\n")

rm(px_mwt, px_manual_mwt, px_manual_bongo)

## ------------------------------------------ ##
##   Plot all casts --- 
## ------------------------------------------ ##
## compute max depth label per cast
px_maxdepth <- px_data_bongo %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(station, " ", cast, "\n(", round(max_depth, 1), "m)"))

pdf(here("figures", "px_sensor_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(px_data_bongo$cruise))) {
  p <- px_data_bongo %>%
    filter(cruise == cr) %>%
    left_join(px_maxdepth %>% select(cruise, station, cast, label),
              by = c("cruise", "station", "cast")) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_point(size = 0.5, alpha = 0.6, color = "steelblue") +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("PX sensor profiles —", cr),
         x = NULL, y = "depth (m)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text  = element_text(size = 7))
  print(p)
}

dev.off()
rm(px_maxdepth)

# AE2426 good
# AR88
#  *  L4 B2 logsheet says PX depth 58 but no cast/data for this px? ^fixed above
#           px sensor cast mixed in with trawl so need to trim
#     L10 B6 incomplete upcast; correctly has max depth; upcast ends at ~150m deep
#     L11 B9 incomplete upcast; correctly has max depth; upcast ends at ~75m
#  *  L6 B10 need to cut off weird surface data point 
#     L8 B17 incomplete upcast; correctly has max depth; upcast ends at ~118m
# AR92 
#     L1 B1 weird cast shape but max depth there
#     L10 B8 incomplete upcast; correctly has max depth; upcast ends at ~200m
#     L11 B11 incomplete upcast; correctly has max depth; upcast ends at ~185m
#     L2 B2 incomplete upcast; correctly has max depth; upcast ends at ~25m
#  *  L6 B12 cast shape weird
#     L8 B17 incomplete upcast; correctly has max depth; upcast ends at ~80m
#     L9 B20 incomplete upcast; correctly has max depth; upcast ends at ~140m
#     L7 B21 incomplete upcast; correctly has max depth; upcast ends at ~80m
#     L3 B22 incomplete upcast; correctly has max depth; upcast ends at ~42m
# AR95
#     L1 B1 incomplete upcast; correctly has max depth; upcast ends at ~7.5m
#  *  L2 B2 need to cut off tail 
#  *  L9 B16 looks weird 
#     L4 B5 incomplete upcast; correctly has max depth; upcast ends at ~22m
#     L5 B6 incomplete upcast; correctly has max depth; upcast ends at ~50m
#     L10 B7 incomplete upcast; correctly has max depth; upcast ends at ~25m
#     L11 B10 incomplete upcast; correctly has max depth; upcast ends at ~125m
#  *  L6 B11 looks wrong; too shallow
#  *  L9 B16 looks weird; does it have 2 casts together?
#     L8 B17 incomplete upcast; correctly has max depth; upcast ends at ~52m
#  *  L3 B19 need to cut off tail 
# AR99
#  *  L2 B3 looks wrong; too shallow
#     L5 B4 incomplete upcast; correctly has max depth; upcast ends at ~20m
#  *  L9 B5 looks wrong; too shallow
#     L10 B6 incomplete upcast; correctly has max depth; upcast ends at ~25m
#     L11 B9 missing PX sensor data; logsheet says there is w max depth ~199m 
#             but with a note that PX sensor data didnt record
#     L6 B10 incomplete upcast; correctly has max depth; upcast ends at ~60m
#     L4 B17 incomplete upcast; correctly has max depth; upcast ends at ~50m
#     L3 B18 incomplete upcast; correctly has max depth; upcast ends at ~30m
#     L7 B19 incomplete upcast; correctly has max depth; upcast ends at ~100m
#     L8 B20 incomplete upcast; correctly has max depth; upcast ends at ~50m
# EN727
#     L4 B5 incomplete upcast; correctly has max depth; upcast ends at ~30m
#  *  L11 B6 logsheets say there should be Px data, but no cast identified?
#             ^fixed above in px_manual_bongo; this was w MWT data
#     L10 B9 incomplete upcast; correctly has max depth; upcast ends at ~175m
#  *  L8 B10 this looks bad; really deep; I think the real cast starts after
#             first super deep data must be surface noise
#  *  L9 B11 this looks bad; too shallow; logsheet say max depth ~200m 
#  *  L7 B14 need to cut off tail 
#     L6 B17 incomplete upcast; correctly has max depth; upcast ends at ~40m
#  *  L5 B19 this looks bad; really deep; I think the real cast starts after
#             first super deep data must be surface noise

## ------------------------------------------ ##
##  Trim to elog deploy/recover window      ----
## ------------------------------------------ ##
# this is to remove extra surface data
## px sensor starts recording once in water so no predeploy trimming needed
## trim using elog times already in px_data_bongo
## small buffer to avoid clipping first/last real data points

px_data_bongo2 <- px_data_bongo %>%
  filter(date_time >= elog_deploy - seconds(60),
         date_time <= elog_recover + seconds(120))

px_data_bongo %>%
  distinct(cruise, station, cast) %>%
  anti_join(
    px_data_bongo2 %>% distinct(cruise, station, cast),
    by = c("cruise", "station", "cast")
  )
# should be 0

## ------------------------------------------ ##
##  Check timestamps          ----
## ------------------------------------------ ## 
px_time_check <- px_data_bongo %>%
  group_by(cruise, station, cast) %>%
  summarise(
    px_start     = min(date_time, na.rm = TRUE),
    px_end       = max(date_time, na.rm = TRUE),
    px_duration  = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    .groups = "drop"
  ) %>%
  left_join(
    px_bongo_cast_meta_final %>% select(cruise, station, cast, elog_deploy, elog_recover),
    by = c("cruise", "station", "cast")
  ) %>%
  mutate(
    offset_deploy_min  = as.numeric(difftime(px_start, elog_deploy,  units = "mins")),
    offset_recover_min = as.numeric(difftime(px_end,   elog_recover, units = "mins")),
    elog_duration      = as.numeric(difftime(elog_recover, elog_deploy, units = "mins")),
    duration_diff_min  = px_duration - elog_duration,
    flag_large_offset  = abs(offset_deploy_min) > 30
  )

px_time_check %>%
  group_by(cruise) %>%
  summarise(
    n_casts              = n(),
    n_flag_large_offset  = sum(flag_large_offset, na.rm = TRUE),
    median_deploy_offset = median(offset_deploy_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(median_deploy_offset)))

px_time_check %>%
  filter(flag_large_offset) %>%
  select(cruise, station, cast, px_start, elog_deploy, offset_deploy_min,
         px_end, elog_recover, offset_recover_min) %>%
  print(n = Inf, width = Inf)

rm(px_time_check)

## ------------------------------------------ ##
##  Plot     ----
## ------------------------------------------ ##
px_maxdepth2 <- px_data_bongo2 %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(station, " ", cast, "\n(", round(max_depth, 1), "m)"))

pdf(here("figures", "px_sensor_profiles_trimmed.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(px_data_bongo2$cruise))) {
  p <- px_data_bongo2 %>%
    filter(cruise == cr) %>%
    left_join(px_maxdepth2 %>% select(cruise, station, cast, label),
              by = c("cruise", "station", "cast")) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_point(size = 0.5, alpha = 0.6, color = "steelblue") +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("PX sensor profiles (trimmed) —", cr),
         x = NULL, y = "depth (m)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text  = element_text(size = 7))
  print(p)
}

dev.off()

# AE2426 good
# AR88   good
# AR92
#     L1 B1 weird cast shape but max depth there (just note)
# AR95
#  *  L6 B11 looks wrong; too shallow = delete?
# AR99
#  *  L2 B3 looks wrong; too shallow = delete?
#  *  L9 B5 looks wrong; too shallow = delete?
# EN727
#    L8 B10 casts starts and needs trimmed to 19:30:05-19:44:27 (b4 this, deep bad data) 
#    L5 B19 casts starts and needs trimmed to 20:35:09-20:43:53 (b4 this, deep bad data)
#    L9 B11 this looks bad; too shallow; logsheet say max depth ~200m = delete 

## ------------------------------------------ ##
##  Delete bad px sensor casts              ----
## ------------------------------------------ ##
## confirmed bad: sensor malfunction
## cross-checked raw px_data max depth vs logsheet target depth
## AR95 L6  B11:  px max 17m,  logsheet target 90m
## AR99 L2  B3:   px max 7m,   logsheet target 39m
## AR99 L9  B5:   px max 14m,  logsheet target 200m
## EN727 L9 B11:  px max 21m,  logsheet target ~200m

px_data_bongo2 <- px_data_bongo2 %>%
  filter(!(cruise == "AR95"  & station == "L6"  & cast == "B11"),
         !(cruise == "AR99"  & station == "L2"  & cast == "B3"),
         !(cruise == "AR99"  & station == "L9"  & cast == "B5"),
         !(cruise == "EN727" & station == "L9"  & cast == "B11"))

## ------------------------------------------ ##
##  Manual time trims                       ----
## ------------------------------------------ ##
## EN727 L8 B10: deep noise before 19:30:05, real cast 19:30:05-19:44:27
## EN727 L5 B19: deep noise before 20:35:09, real cast 20:35:09-20:43:53

px_data_bongo2 <- px_data_bongo2 %>%
  mutate(date_time = date_time) %>%
  filter(
    !(cruise == "EN727" & station == "L8" & cast == "B10" &
        date_time < as.POSIXct("2025-01-26 19:30:05", tz = "UTC")),
    !(cruise == "EN727" & station == "L5" & cast == "B19" &
        date_time < as.POSIXct("2025-01-27 20:35:09", tz = "UTC"))
  )

## verify
px_data_bongo2 %>%
  filter(cruise == "EN727", station %in% c("L8", "L5"), 
         cast %in% c("B10", "B19")) %>%
  group_by(station, cast) %>%
  summarise(start = min(date_time), end = max(date_time),
            max_depth = max(depth_m), .groups = "drop")

## ------------------------------------------ ##
##  Label downcast / upcast                ----
## ------------------------------------------ ##
px_data_bongo_final <- px_data_bongo2 %>%
  group_by(cruise, station, cast) %>%
  do(label_down_up(.)) %>%
  ungroup()

px_data_bongo_final %>%
  count(down_up)
rm(px_data_bongo, px_data_bongo2)

## ------------------------------------------ ##
##  Plot labeled px sensor profiles         ----
## ------------------------------------------ ##
pdf(here("figures", "px_sensor_profiles_labeled.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(px_data_bongo_final$cruise))) {
  p <- px_data_bongo_final %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
    geom_point(size = 1, alpha = 0.6) +
    scale_y_reverse() +
    scale_color_manual(
      values = c(predeploy = "grey70",
                 downcast  = "steelblue",
                 upcast    = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("PX sensor labeled profiles —", cr),
         x = NULL, y = "depth (m)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text  = element_text(size = 6),
          legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()

## ------------------------------------------ ##
##   Add notes column           ----
## ------------------------------------------ ##
px_cast_notes <- tribble(
  ~cruise,  ~station, ~cast,  ~note_code,           ~note_detail,
  ## missing or deleted bad data:
  ## AR95 L6  B11:  px max 17m,  logsheet target 90m
  ## AR99 L2  B3:   px max 7m,   logsheet target 39m
  ## AR99 L9  B5:   px max 14m,  logsheet target 200m
  ## EN727 L9 B11:  px max 21m,  logsheet target ~200m
  ## AR99 L11 B9: No PX sensor data recorded; logsheet notes sensor did not record; logsheet max depth ~199m
  # --- manual time trims: deep noise at start ---
  # EN727 L8 B10 px_trimmed_start, Deep noise before 19:30:05 UTC trimmed; real cast 19:30:05 to 19:44:27",
  # EN727 L5 B19 px_trimmed_start, Deep noise before 20:35:09 UTC trimmed; real cast 20:35:09 to 20:43:53",
  # AR92  L1 B1 weird cast shape but max depth there 
  
  # --- incomplete upcast ---
  "AR88",   "L10",    "B6",   "early_end", "Upcast ends at ~150m; sensor stopped recording before recovery",
  "AR88",   "L11",    "B9",   "early_end", "Upcast ends at ~75m; sensor stopped recording before recovery",
  "AR88",   "L8",     "B17",  "early_end", "Upcast ends at ~118m; sensor stopped recording before recovery",
  "AR92",   "L10",    "B8",   "early_end", "Upcast ends at ~200m; sensor stopped recording before recovery",
  "AR92",   "L11",    "B11",  "early_end", "Upcast ends at ~185m; sensor stopped recording before recovery",
  "AR92",   "L2",     "B2",   "early_end", "Upcast ends at ~25m; sensor stopped recording before recovery",
  "AR92",   "L8",     "B17",  "early_end", "Upcast ends at ~80m; sensor stopped recording before recovery",
  "AR92",   "L9",     "B20",  "early_end", "Upcast ends at ~140m; sensor stopped recording before recovery",
  "AR92",   "L7",     "B21",  "early_end", "Upcast ends at ~80m; sensor stopped recording before recovery",
  "AR92",   "L3",     "B22",  "early_end", "Upcast ends at ~42m; sensor stopped recording before recovery",
  "AR92",   "L6",     "B12",  "early_end", "Upcast ends at ~48m; sensor stopped recording before recovery",
  "AR95",   "L1",     "B1",   "early_end", "Upcast ends at ~7.5m; sensor stopped recording before recovery",
  "AR95",   "L4",     "B5",   "early_end", "Upcast ends at ~22m; sensor stopped recording before recovery",
  "AR95",   "L5",     "B6",   "early_end", "Upcast ends at ~50m; sensor stopped recording before recovery",
  "AR95",   "L10",    "B7",   "early_end", "Upcast ends at ~25m; sensor stopped recording before recovery",
  "AR95",   "L11",    "B10",  "early_end", "Upcast ends at ~125m; sensor stopped recording before recovery",
  "AR95",   "L8",     "B17",  "early_end", "Upcast ends at ~52m; sensor stopped recording before recovery",
  "AR99",   "L5",     "B4",   "early_end", "Upcast ends at ~20m; sensor stopped recording before recovery",
  "AR99",   "L10",    "B6",   "early_end", "Upcast ends at ~25m; sensor stopped recording before recovery",
  "AR99",   "L6",     "B10",  "early_end", "Upcast ends at ~60m; sensor stopped recording before recovery",
  "AR99",   "L4",     "B17",  "early_end", "Upcast ends at ~50m; sensor stopped recording before recovery",
  "AR99",   "L3",     "B18",  "early_end", "Upcast ends at ~30m; sensor stopped recording before recovery",
  "AR99",   "L7",     "B19",  "early_end", "Upcast ends at ~100m; sensor stopped recording before recovery",
  "AR99",   "L8",     "B20",  "early_end", "Upcast ends at ~50m; sensor stopped recording before recovery",
  "EN727",  "L4",     "B5",   "early_end", "Upcast ends at ~30m; sensor stopped recording before recovery",
  "EN727",  "L10",    "B9",   "early_end", "Upcast ends at ~175m; sensor stopped recording before recovery",
  "EN727",  "L6",     "B17",  "early_end", "Upcast ends at ~40m; sensor stopped recording before recovery"
)

px_data_bongo_final <- px_data_bongo_final %>%
  left_join(px_cast_notes, by = c("cruise", "station", "cast"))

rm(px_cast_notes)

## ------------------------------------------ ##
##   Add recording interval column  ----
## ------------------------------------------ ##
# almost always 2 sec for PX sensor 
## add sampling interval column
px_intervals <- px_data_bongo_final %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  mutate(interval_sec = as.numeric(difftime(date_time,
                                            dplyr::lag(date_time),
                                            units = "secs"))) %>%
  summarise(
    px_sampling_interval_sec = round(median(interval_sec, na.rm = TRUE)),
    px_max_gap_sec           = round(max(interval_sec,    na.rm = TRUE)),
    px_n_obs                 = n(),
    .groups = "drop"
  )

px_data_bongo_final <- px_data_bongo_final %>%
  left_join(px_intervals, by = c("cruise", "station", "cast"))

rm(px_intervals)

## ------------------------------------------ ##
##  QC checks                              ----
## ------------------------------------------ ##
## ------------------------------------------ ##
##  a. Naming consistency checks       ----
## ------------------------------------------ ##
## All these should not print anything (tibble 0 x 1)

# cruise: should all be uppercase alphanumeric
px_data_bongo_final %>%
  filter(!grepl("^[A-Z]{2,3}[0-9]+[A-Z]?$", cruise)) %>%
  distinct(cruise)

# station: should be L + integer, MVCO
px_data_bongo_final %>%
  filter(!grepl("^L[0-9]+$", station)) %>%
  distinct(cruise, station)

# cast: should be B + integer
px_data_bongo_final %>%
  filter(!grepl("^B[0-9]+$", cast)) %>%
  distinct(cruise, station, cast)

# down_up: only valid labels
px_data_bongo_final %>%
  filter(!down_up %in% c("predeploy", "downcast", "upcast")) %>%
  distinct(down_up)

## ------------------------------------------ ##
##  b. Physical range checks (row-level) ----
## ------------------------------------------ ##

px_data_bongo_final %>%
  summarise(
    n_neg_depth  = sum(depth_m < 0, na.rm = TRUE),
    n_deep       = sum(depth_m > 300, na.rm = TRUE),
    n_temp_low   = sum(temp_C < -2, na.rm = TRUE),
    n_temp_high  = sum(temp_C > 32, na.rm = TRUE),
    n_na_depth   = sum(is.na(depth_m)),
    n_na_temp    = sum(is.na(temp_C))
  )

## cast-level summary
px_data_bongo_final %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_obs        = n(),
    max_depth    = max(depth_m, na.rm = TRUE),
    duration_min = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    has_downcast = any(down_up == "downcast"),
    has_upcast   = any(down_up == "upcast"),
    .groups = "drop"
  ) %>%
  filter(!has_downcast | !has_upcast | max_depth < 15 | duration_min < 3) %>%
  arrange(cruise, station)

## ------------------------------------------ ##
##  c. Temporal checks (cast-level)     ----
## ------------------------------------------ ##
cast_qc_px <- px_data_bongo_final %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_obs        = n(),
    t_start      = min(date_time, na.rm = TRUE),
    t_end        = max(date_time, na.rm = TRUE),
    duration_min = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    max_depth_m  = max(depth_m,  na.rm = TRUE),
    temp_min_C   = min(temp_C,   na.rm = TRUE),
    temp_max_C   = max(temp_C,   na.rm = TRUE),
    temp_range_C = temp_max_C - temp_min_C,
    n_time_reversal = sum(diff(as.numeric(date_time)) < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    flag_too_short     = duration_min < 3,
    flag_too_long      = duration_min > 120,
    flag_shallow       = max_depth_m < 15,
    flag_temp_suspect  = temp_range_C > 15,
    flag_time_reversal = n_time_reversal > 0,
    flag_few_obs       = n_obs < 20
  )

cast_qc_px %>%
  summarise(across(starts_with("flag_"), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "flag", values_to = "n_casts_flagged") %>%
  filter(n_casts_flagged > 0) %>%
  arrange(desc(n_casts_flagged))

cast_qc_px %>%
  filter(if_any(starts_with("flag_"), ~.)) %>%
  select(cruise, station, cast, duration_min, max_depth_m,
         temp_range_C, n_time_reversal, n_obs, starts_with("flag_")) %>%
  arrange(cruise, station) %>%
  print(n = Inf, width = Inf)

## ------------------------------------------ ##
##  Plots       ----
## ------------------------------------------ ##

## temp-depth profiles by month
px_data_bongo_final %>%
  filter(down_up == "downcast") %>%
  mutate(month = lubridate::month(date_time, label = TRUE)) %>%
  ggplot(aes(x = temp_C, y = depth_m, color = cruise)) +
  geom_point(size = 0.3, alpha = 0.4) +
  scale_y_reverse() +
  facet_wrap(~month) +
  labs(title = "PX temperature-depth profiles by month (downcast)",
       x = "Temperature (°C)", y = "Depth (m)") +
  theme_minimal()

## large consecutive temp jumps
temp_jumps_px <- px_data_bongo_final %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  mutate(temp_diff = abs(temp_C - dplyr::lag(temp_C))) %>%
  filter(!is.na(temp_diff), temp_diff > 5) %>%
  select(cruise, station, cast, date_time, temp_C, temp_diff, depth_m) %>%
  ungroup()

message("Large consecutive temp jumps: ", nrow(temp_jumps_px))
print(temp_jumps_px, n = 30)
rm(temp_jumps_px)

## max depth vs logsheet target
# need meta loaded for this
meta <- read_csv(file.path("data", "raw",
                              "all-nes-lter-bongologs-20260526.csv"),
                    show_col_types = FALSE) %>%
  filter(cruise %in% unique(px_data_bongo_final$cruise)) %>%
  mutate(cast = paste0("B", cast))

px_data_bongo_final %>%
  filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  summarise(px_max_depth = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  left_join(meta %>% select(cruise, station, cast, depth_target),
            by = c("cruise", "station", "cast")) %>%
  ggplot(aes(x = depth_target, y = px_max_depth)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick", linetype = "dashed") +
  labs(x = "Target depth (m)", y = "PX max depth (m)") +
  theme_minimal()

# near-bottom boxplot by station
px_data_bongo_final %>%
  filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  mutate(max_depth = max(depth_m, na.rm = TRUE)) %>%
  filter(depth_m >= max_depth - 5) %>%
  ungroup() %>%
  mutate(month = lubridate::month(date_time, label = TRUE)) %>%
  ggplot(aes(x = month, y = temp_C)) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = cruise), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~station) +
  labs(title = "PX near-bottom temperature by month (within 5m of max depth, downcast)",
       x = "Month", y = "Temp (°C)", color = "Cruise") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3)))

# near-bottom 
px_data_bongo_final %>%
  filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  mutate(max_depth = max(depth_m, na.rm = TRUE)) %>%
  filter(depth_m >= max_depth - 5) %>%
  ungroup() %>%
  mutate(month = lubridate::month(date_time)) %>%
  ggplot(aes(x = factor(month), y = temp_C, fill = factor(month))) +
  geom_violin(alpha = 0.6, quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.4, color = "grey30") +
  scale_x_discrete(labels = month.abb) +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "PX near-bottom temperature by month (within 5m of max depth, downcast)",
       x = NULL, y = "Temp (°C)") +
  theme_minimal()

rm(meta)

## ------------------------------------------ ##
##  d. Duplicate timestamp check       ----
## ------------------------------------------ ##
dup_times_px <- px_data_bongo_final %>%
  group_by(cruise, station, cast, date_time) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(cruise, station, cast, date_time)

message("Duplicate timestamps: ", nrow(dup_times_px))
rm(dup_times_px)

## ------------------------------------------ ##
##  e. Downcast/upcast balance check   ----
## ------------------------------------------ ##
# most casts should have both a downcast and upcast
cast_coverage_px <- px_data_bongo_final %>%
  group_by(cruise, station, cast) %>%
  summarise(
    has_downcast = any(down_up == "downcast"),
    has_upcast   = any(down_up == "upcast"),
    .groups = "drop"
  ) %>%
  filter(!has_downcast | !has_upcast)

if (nrow(cast_coverage_px) > 0) {
  message("Casts missing downcast or upcast:")
  print(cast_coverage_px)
} else {
  message("All casts have both downcast and upcast labels.")
}

rm(cast_qc_px, cast_coverage_px)

## stations available per cruise
px_data_bongo_final %>%
  distinct(cruise, station) %>%
  group_by(cruise) %>%
  summarise(stations = paste(sort(station), collapse = ", "), .groups = "drop")

## ------------------------------------------ ##
##  Finalize col names and order  ----
## ------------------------------------------ ##
px_data_bongo_final <- px_data_bongo_final %>%
  rename(date_time_utc = date_time,
         file_start_time = cast_start,
         sampling_interval_sec = px_sampling_interval_sec,
         max_gap_sec = px_max_gap_sec,
         n_obs = px_n_obs) %>%
  select(cruise, station, cast, date_time_utc, depth_m, temp_C, down_up, note_code, 
         note_detail, file_start_time, sampling_interval_sec, max_gap_sec, 
         n_obs)

## ------------------------------------------ ##
##   Save output         ----
## ------------------------------------------ ##
saveRDS(px_data_bongo_final,
        here("data", "processed",
             paste0("px_data_bongo_", Sys.Date(), ".rds")))

write_csv(px_data_bongo_final,
          here("data", "processed", "nes-lter-bongo-px.csv"))

tibble(column = names(px_data_bongo_final)) %>%
  write_csv(here("data", "processed", "px-column-headers.csv"))

################################################################################
# go to -----------> 03_tdr_offsets.R
#           OR     > 02_px_sensor_tidy.R
#           OR     > 02_ctd_bongo_tidy.R 
################################################################################