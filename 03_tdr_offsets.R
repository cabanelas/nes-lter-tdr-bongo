###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose:cross-ref with elog + CTD to build
## offset table, apply corrections

##  Input:  data/processed/tdr_ctd_tests.csv (created in 02)
##           data/raw/tdr_offsets.csv  

# need to get CTD max depth from api and cross ref with elog
# check what if any offset is needed then apply offsets to tdr data
#### NEED TO FIND WHICH HAVE THESE AND FIND CTD MAX DEPTH FOR EACH OF THESE TOWS
#### NEED TO ADD CAST AND STATION TO SOME OF THESE
#### DOING THIS WILL GIVE OFFSETS FOR ANY OF THESE
## ------------------------------------------ ##
##  5. Join depth offsets                  ----
## ------------------------------------------ ##
#### MOVE THIS TO LATER/FURTHER DOWN  ##########
#### WHAT ABOUT STEP 4     

# offset_m = the instrument depth offset for a given cast
# add depth_offset column
# Corrected depth = depth_m - offset_m 

tdr_ctd_test <- read.csv(here("data", "processed", "tdr_ctd_tests.csv"),
                         stringsAsFactors = FALSE) 
### NEED TO CHECK AND ADD OFFSETS TO CRUISES 
# AR77; EN712, EN715, EN720, AE2426; EN727, AR88, AR92, AR95; AR99

offsets <- read.csv(here("data", "raw", "tdr_offsets.csv"),
                    stringsAsFactors = FALSE) %>%
  mutate(across(c(cruise, station, cast), as.character))
# 
# all_data <- all_data %>%
#   left_join(offsets, by = c("cruise", "station", "cast")) %>%
#   mutate(
#     depth_offset = replace_na(offset_m, 0),  # 0 if no offset recorded
#     depth_m_raw  = depth_m,                  # preserve original
#     depth_m      = depth_m - depth_offset    # corrected depth
#   ) %>%
#   select(-offset_m) %>%
#   # after correction, remove any rows that went negative
#   filter(depth_m >= 0)
# 
# # report which casts received a non-zero offset
# offsets_applied <- all_data %>%
#   filter(depth_offset != 0) %>%
#   distinct(cruise, station, cast, depth_offset)
# 
# message("Depth offsets applied to ", nrow(offsets_applied), " cast(s):")
# print(offsets_applied)

###############################################################
##  NES-LTER Bongo TDR: TDR-CTD Offset Analysis
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: For casts where TDR was attached to CTD (bench tests),
##           pull CTD max depth from NES-LTER API and compare to
##           TDR max depth to identify any depth offsets needed.
##
##  Input:   data/processed/tdr_ctd_tests.csv  (from 02_tdr_tidy.R)
##
##  Output:  data/processed/tdr_ctd_offset_check.csv
###############################################################

library(tidyverse)
library(here)

BASE_URL <- "https://nes-lter-api.whoi.edu/api"

safe_read_csv <- function(url) {
  tryCatch(
    read_csv(url, show_col_types = FALSE),
    error = function(e) {
      message("  FAILED: ", url)
      NULL
    }
  )
}
all_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cruise) %>%
  summarise(
    median_min_depth = median(min_depth, na.rm = TRUE),
    max_min_depth    = max(min_depth, na.rm = TRUE),
    median_max_depth = median(max_depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(max_min_depth))
## ------------------------------------------ ##
##  1. Load TDR-CTD test data               ----
## ------------------------------------------ ##

tdr_ctd_test <- read_csv(here("data", "processed", "tdr_ctd_tests.csv"),
                         show_col_types = FALSE) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC"))

# TDR max depth per cast
tdr_maxdepth <- tdr_ctd_test %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_max_depth_m = max(depth_m, na.rm = TRUE),
    tdr_start       = min(date_time, na.rm = TRUE),
    tdr_end         = max(date_time, na.rm = TRUE),
    n_tdr_obs       = n(),
    .groups = "drop"
  )

message("TDR-CTD test casts:")
print(tdr_maxdepth)

# which cruises to look up
cruises_to_check <- unique(tolower(tdr_maxdepth$cruise))
message("Cruises to pull CTD data for: ", paste(cruises_to_check, collapse = ", "))

## ------------------------------------------ ##
##  2. Pull CTD cast metadata from API      ----
## ------------------------------------------ ##
# get cast metadata (cruise, cast number, nearest_station, lat, lon, date)

meta_list <- map(cruises_to_check, function(cru) {
  message("  pulling metadata: ", cru)
  url <- paste0(BASE_URL, "/ctd/metadata/", cru, ".csv")
  df  <- safe_read_csv(url)
  if (!is.null(df)) {
    df %>%
      mutate(
        cruise = toupper(cru),
        cast   = as.character(cast)
      )
  }
})

ctd_meta <- bind_rows(meta_list)

message("CTD metadata rows: ", nrow(ctd_meta))
glimpse(ctd_meta)

## ------------------------------------------ ##
##  3. Pull CTD bottle data for max depth   ----
## ------------------------------------------ ##
# bottle data gives us discrete depth samples per cast
# max bottle depth = approximate CTD max depth

bottle_list <- map(cruises_to_check, function(cru) {
  message("  pulling bottles: ", cru)
  url <- paste0(BASE_URL, "/ctd/bottles/", cru, ".csv")
  df  <- safe_read_csv(url)
  if (!is.null(df)) {
    df %>%
      mutate(
        cruise = toupper(cru),
        cast   = as.character(cast)
      )
  }
})

ctd_bottles <- bind_rows(bottle_list)

# CTD max depth per cast from bottles
ctd_maxdepth <- ctd_bottles %>%
  group_by(cruise, cast) %>%
  summarise(
    ctd_max_depth_m  = max(depsm, na.rm = TRUE),
    ctd_n_bottles    = n(),
    .groups = "drop"
  )
## ------------------------------------------ ##
##  4. Join and compare                     ----
## ------------------------------------------ ##

# join metadata + max depth, then match to TDR by cruise + nearest_station
ctd_cast_info <- ctd_meta %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast"))

offset_check <- tdr_maxdepth %>%
  left_join(
    ctd_cast_info %>%
      select(cruise, ctd_cast = cast, nearest_station,
             ctd_date = date, ctd_max_depth_m, ctd_n_bottles),
    by = c("cruise", "station" = "nearest_station")
  ) %>%
  mutate(
    depth_diff_m      = tdr_max_depth_m - ctd_max_depth_m,
    flag_large_offset = abs(depth_diff_m) > 5
  )

message("\nTDR vs CTD depth comparison:")
offset_check %>%
  select(cruise, station, cast, ctd_cast, ctd_date,
         tdr_max_depth_m, ctd_max_depth_m,
         depth_diff_m, flag_large_offset) %>%
  print(n = Inf)
## ------------------------------------------ ##
##  4. Match TDR casts to CTD casts         ----
## ------------------------------------------ ##
# TDR test casts have station (e.g. "u9a", "L5") and cast (e.g. "B18")
# CTD casts are numbered (1, 2, 3...) with nearest_station assigned by API
# need to match by cruise + nearest_station + time overlap

# join CTD metadata with max depth
ctd_cast_summary <- ctd_meta %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast")) %>%
  mutate(date = as.POSIXct(date, tz = "UTC")) %>%
  select(cruise, cast, nearest_station, date,
         latitude, longitude, ctd_max_depth_m, ctd_n_bottles)

message("CTD casts with depth info:")
print(ctd_cast_summary %>% arrange(cruise, cast))

## ------------------------------------------ ##
##  5. Match by time proximity              ----
## ------------------------------------------ ##
# for each TDR test cast, find the CTD cast whose time is closest
# to the TDR deployment window

offset_check <- tdr_maxdepth %>%
  rowwise() %>%
  mutate(
    # filter CTD casts from same cruise
    ctd_same_cruise = list(
      ctd_cast_summary %>%
        filter(cruise == !!cruise, !is.na(date))
    ),
    # find closest CTD cast by time
    time_diff_hrs = list(
      as.numeric(difftime(ctd_same_cruise$date, tdr_start, units = "hours"))
    ),
    closest_idx = if (length(time_diff_hrs) > 0)
      which.min(abs(unlist(time_diff_hrs))) else NA_integer_,
    ctd_cast          = if (!is.na(closest_idx)) ctd_same_cruise$cast[closest_idx]          else NA_character_,
    ctd_nearest_sta   = if (!is.na(closest_idx)) ctd_same_cruise$nearest_station[closest_idx] else NA_character_,
    ctd_cast_time     = if (!is.na(closest_idx)) ctd_same_cruise$date[closest_idx]           else as.POSIXct(NA),
    ctd_max_depth_m   = if (!is.na(closest_idx)) ctd_same_cruise$ctd_max_depth_m[closest_idx] else NA_real_,
    time_offset_hrs   = if (!is.na(closest_idx)) unlist(time_diff_hrs)[closest_idx]          else NA_real_
  ) %>%
  ungroup() %>%
  select(-ctd_same_cruise, -time_diff_hrs, -closest_idx) %>%
  mutate(
    depth_diff_m = tdr_max_depth_m - ctd_max_depth_m,
    flag_large_offset = abs(depth_diff_m) > 5  # flag if TDR vs CTD differ by >5m
  )

message("\nTDR vs CTD depth comparison:")
print(offset_check %>% select(cruise, station, cast,
                              tdr_max_depth_m, ctd_max_depth_m,
                              depth_diff_m, flag_large_offset,
                              time_offset_hrs), n = Inf)

## ------------------------------------------ ##
##  6. Flag and review                      ----
## ------------------------------------------ ##

flagged <- offset_check %>% filter(flag_large_offset)

if (nrow(flagged) > 0) {
  message("\n!! ", nrow(flagged), " cast(s) with large TDR-CTD depth offset (>5m):")
  print(flagged %>% select(cruise, station, cast,
                           tdr_max_depth_m, ctd_max_depth_m, depth_diff_m))
} else {
  message("  No large offsets detected.")
}

## ------------------------------------------ ##
##  7. Save                                 ----
## ------------------------------------------ ##

write_csv(offset_check, here("data", "processed", "tdr_ctd_offset_check.csv"))
message("Saved -> tdr_ctd_offset_check.csv")