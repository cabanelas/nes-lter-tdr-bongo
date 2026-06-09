###############################################################
##  NES-LTER Bongo PX Sensor: Tidy & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_px_sensor_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Read raw PX Multisensor (SR15) measurements CSV files,
##           assign cruise/station/cast metadata via elog,
##           filter out Isaacs-Kidd Midwater Trawl deployments,
##           trim to elog deploy/recover windows, label downcast/upcast,
##           and export cleaned bongo-only PX sensor data.
##
##  Input:   data/raw/px_sensor/{cruise}_px_sensor/*.csv
##           data/raw/elog_zoop_tows_thruAR99_2026-04-14.csv
##                    (from nes-lter-api-pulls.Rproj; 01_elog_pull.R)
##           NES-LTER API (https://github.com/WHOIGit/nes-lter-api-2/wiki) for MWT elog
##  Output:  data/processed/px_data_bongo_YYYY-MM-DD.rds
##           data/processed/px_data_bongo.csv
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
## EN727: px sensor started 11-34 min before elog deploy
## AR92 MWT: px started ~10 min before MWT deploy
grace_bongo_default <- 2
grace_bongo_en727   <- 40

grace_mwt_default   <- 2  
grace_mwt_ar92      <- 15

elog_bongo_px_window <- elog_bongo_px_window %>%
  mutate(grace = if_else(cruise == "EN727", grace_bongo_en727, grace_bongo_default))

px_cast_meta <- px_data %>%
  distinct(cruise, cast_start) %>%
  left_join(elog_bongo_px_window, by = "cruise", relationship = "many-to-many") %>%
  filter(cast_start >= elog_deploy - minutes(grace),
         cast_start <= elog_recover) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(as.numeric(difftime(cast_start, elog_deploy, units = "mins"))), n = 1) %>%
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

rm(elog_bongo_px_window, grace_bongo_default, grace_bongo_en727, elog)

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
      mutate(grace = if_else(cruise == "AR92", grace_mwt_ar92, grace_mwt_default)),
    by = "cruise", relationship = "many-to-many"
  ) %>%
  filter(cast_start >= mwt_deploy - minutes(grace),
         cast_start <= mwt_recover) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(as.numeric(difftime(cast_start, mwt_deploy, units = "mins"))), n = 1) %>%
  ungroup() %>%
  mutate(instrument = "MWT") %>%
  select(cruise, cast_start, station, cast, instrument)

## anything that matched neither bongo nor MWT
truly_unmatched <- px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start")) %>%
  anti_join(px_mwt, by = c("cruise", "cast_start"))

cat("Unmatched (not bongo):  ", nrow(px_data %>% distinct(cruise, cast_start) %>%
                                       anti_join(px_cast_meta, by = c("cruise", "cast_start"))), "\n")
cat("Truly unidentified:     ", nrow(truly_unmatched), "\n")
cat("Total px cast_starts:   ", n_distinct(paste(px_data$cruise, px_data$cast_start)), "\n")
cat("Matched to bongo:       ", nrow(px_cast_meta), "\n")
cat("Matched to MWT:         ", nrow(px_mwt), "\n")

if (nrow(truly_unmatched) > 0) {
  cat("\n--- truly unidentified (not bongo, not MWT) ---\n")
  print(truly_unmatched)
}

## check what was happening in elog around these unidentified cast_starts
elog_all_instruments <- map_dfr(tolower(unique(truly_unmatched$cruise)), function(cr) {
  url <- paste0("https://nes-lter-api.whoi.edu/api/events/", cr, ".csv")
  message("  fetching: ", cr)
  df <- safe_read_csv(url)
  if (is.null(df)) return(NULL)
  df %>% mutate(cruise = toupper(cr))
})

truly_unmatched %>%
  left_join(elog_all_instruments, by = "cruise", relationship = "many-to-many") %>%
  mutate(diff_mins = as.numeric(difftime(cast_start, dateTime8601, units = "mins"))) %>%
  filter(abs(diff_mins) <= 60) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(diff_mins), n = 5) %>%
  ungroup() %>%
  select(cruise, cast_start, Instrument, Action, Station, Cast, dateTime8601, diff_mins) %>%
  arrange(cruise, cast_start, abs(diff_mins)) %>%
  print(n = Inf)

truly_unmatched
rm(elog_mwt_window, truly_unmatched)

## ------------------------------------------ ##
##  Manual exclusions & assignments         ----
## ------------------------------------------ ##
## AR92 2025-08-18 11:30:24  = MWT Tow7; missing recover in elog
## AR99 2026-01-15 00:00:00  = bad
## EN727 2025-01-27 00:00:01 = bongo cast L7 B14
##   cast_start (00:00) is misleading; deploy was 04:43 UTC within file range
px_manual_mwt <- tibble(
  cruise     = "AR92", cast_start = as.POSIXct("2025-08-18 11:30:24", tz = "UTC"),
  station    = "L7", cast = "Tow7"
)

px_manual_bongo <- tribble(
  ~cruise,  ~cast_start,                                     ~station, ~cast,  ~elog_deploy,                                   ~elog_recover,
  "EN727",  as.POSIXct("2025-01-27 00:00:01", tz = "UTC"), "L7",     "B14",  as.POSIXct("2025-01-27 04:43:44", tz = "UTC"), as.POSIXct("2025-01-27 05:00:52", tz = "UTC"),
  "AR88",   as.POSIXct("2025-04-25 03:00:45", tz = "UTC"), "L4",     "B2",   as.POSIXct("2025-04-25 04:44:00", tz = "UTC"), as.POSIXct("2025-04-25 04:51:17", tz = "UTC"),
  "EN727",  as.POSIXct("2025-01-26 02:16:17", tz = "UTC"), "L11",    "B6",   as.POSIXct("2025-01-26 05:01:30", tz = "UTC"), as.POSIXct("2025-01-26 05:29:41", tz = "UTC")
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
  filter(!(cruise == "AR99" & cast_start == as.POSIXct("2026-01-15 00:00:00", tz = "UTC"))) %>%
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
cat("Excluded (bad file):        1\n")
cat("px_data_bongo rows:        ", nrow(px_data_bongo), "\n")
cat("unique bongo casts:         ", n_distinct(paste(px_data_bongo$cruise, px_data_bongo$cast)), "\n")

## all cast_starts should be accounted for
accounted <- nrow(px_cast_meta) +    # 53 auto bongo
  nrow(px_mwt) +          # 39 auto MWT (includes AR88 + EN727 L11)
  nrow(px_manual_mwt) +   # 1  AR92 Tow7
  1 +                     # 1  EN727 00:00:01 midnight (manual bongo, not in px_mwt)
  1                       # 1  AR99 bad file

cat("Accounted for:", accounted, "of", total, "\n")
rm(px_mwt, px_manual_mwt, px_manual_bongo, accounted)

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
##  QC checks                              ----
## ------------------------------------------ ##
## naming consistency
px_data_bongo_final %>%
  filter(!grepl("^[A-Z]{2,3}[0-9]+[A-Z]?$", cruise)) %>%
  distinct(cruise)

px_data_bongo_final %>%
  filter(!grepl("^L[0-9]+$", station)) %>%
  distinct(cruise, station)

px_data_bongo_final %>%
  filter(!grepl("^B[0-9]+$", cast)) %>%
  distinct(cruise, station, cast)

## physical range checks
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
##   Save output         ----
## ------------------------------------------ ##
saveRDS(px_data_bongo_final,
        here("data", "processed",
             paste0("px_data_bongo_", Sys.Date(), ".rds")))

write_csv(px_data_bongo_final,
          here("data", "processed", "px_data_bongo.csv"))
