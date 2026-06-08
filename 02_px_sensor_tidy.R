###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_px_sensor_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: 
##
##  Input:   elog_zoop_tows_thruAR99_2026-04-14.csv (nes-lter-api-pulls.Rproj)
##  Output:             
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
      date_time  = ymd_hms(paste(date, time), tz = "UTC")
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

elog_px <- elog %>%
  filter(cruise %in% unique(px_data$cruise),
         action == "deploy") %>%
  select(cruise, station, cast, elog_deploy = datetime8601) %>%
  mutate(across(c(cruise, station, cast), as.character))

## match each px cast_start to nearest elog deploy within buffer
buffer_mins <- 60

### SOMETHING IS WRONG WITH THIS; maybe some times in px sensor not utc ??? 
px_cast_meta <- px_data %>%
  distinct(cruise, cast_start) %>%
  left_join(elog_px, by = "cruise") %>%
  mutate(
    diff_mins = as.numeric(difftime(cast_start, elog_deploy, units = "mins"))
  ) %>%
  filter(diff_mins >= -buffer_mins, diff_mins <= buffer_mins) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(diff_mins), n = 1) %>%
  ungroup() %>%
  select(cruise, cast_start, station, cast, elog_deploy, diff_mins)

print(px_cast_meta, n = Inf)

## check which cast_starts didn't get matched
px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start")) %>%
  arrange(cruise, cast_start)

## there is Isaacs-Kidd Midwater Trawl px sensor data in here!
# delete those that arent bongo tows

# get elog from API2 to get Isaacs-Kidd Midwater Trawl 
safe_read_csv <- function(url) {
  tryCatch(
    read_csv(url, show_col_types = FALSE),
    error = function(e) { message("  FAILED: ", url); NULL }
  )
}

elog_full <- map_dfr(tolower(unique(px_data$cruise)), function(cr) {
  url <- paste0("https://nes-lter-api.whoi.edu/api/events/", cr, ".csv")
  message("  fetching: ", cr)
  df <- safe_read_csv(url)
  if (is.null(df)) return(NULL)
  df %>% mutate(cruise = toupper(cr))
}) %>%
  filter(Instrument %in% c("Bongo Net", "Isaacs-Kidd Midwater Trawl"))

unmatched <- px_data %>%
  distinct(cruise, cast_start) %>%
  anti_join(px_cast_meta, by = c("cruise", "cast_start"))

unmatched %>%
  left_join(elog_full, by = "cruise", relationship = "many-to-many") %>%
  mutate(diff_mins = as.numeric(difftime(cast_start, dateTime8601, units = "mins"))) %>%
  filter(abs(diff_mins) <= 60) %>%
  group_by(cruise, cast_start) %>%
  slice_min(abs(diff_mins), n = 3) %>%
  ungroup() %>%
  select(cruise, cast_start, Instrument, Action, Station, Cast, dateTime8601, diff_mins) %>%
  arrange(cruise, cast_start) %>%
  print(n = Inf)

## ------------------------------------------ ##
##  Filter to bongo only + add metadata     ----
## ------------------------------------------ ##
## px_cast_meta already contains only bongo matches (from elog_px which is bongo only)
## use it as a whitelist to filter px_data to bongo tows only

px_data_bongo <- px_data %>%
  semi_join(px_cast_meta, by = c("cruise", "cast_start")) %>%  # keep only bongo cast_starts
  left_join(
    px_cast_meta %>% select(cruise, cast_start, station, cast, elog_deploy),
    by = c("cruise", "cast_start")
  )

glimpse(px_data_bongo)

## check
cat("px_data rows (all):      ", nrow(px_data), "\n")
cat("px_data rows (bongo only):", nrow(px_data_bongo), "\n")
cat("unique bongo casts:       ", n_distinct(paste(px_data_bongo$cruise, px_data_bongo$cast)), "\n")

## which cruise/station/cast combos have px sensor data
px_data_bongo %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_obs     = n(),
    max_depth = max(depth_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cruise, station, cast) %>%
  print(n = Inf)
