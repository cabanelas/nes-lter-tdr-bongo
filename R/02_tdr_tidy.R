###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_tdr_tidy.R
##  Author:  Alexandra Cabanelas
##
##  TDR == Star-Oddi DST centi-TD temperature and depth recorder
##  Purpose: Read per cast CSV files from all cruise TDR folders, tidy data,
##           detect multiple casts, label downcast/upcast, export RDS & csv
##
##  Get CSVs for DAT-only cruises (EN608, EN627, EN644) run 01_tdr_dat_to_csv.R
##  Functions are in 00_helpers.R
##
##  Input:  data/raw/tdr_data/<CRUISE>_TDR/*.csv   (one CSV per bongo tow)
##          data/raw/elog_zoop_tows_thruAR99_2026-04-14.csv
##                  from nes-lter-api-pulls.Rproj; 01_elog_pull.R
##          data/raw/all-nes-lter-bongologs-20260526.csv
##                  from nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
##                          
##  Output: NOT EXPORTED**** data/processed/<CRUISE>_tdr_processed.csv  (per cruise)
##          data/processed/tdr_data_no_offset_Sys.Date.rds (all cruises)
##          data/processed/tdr_data_no_offset.csv          (all cruises)
##          data/processed/tdr_ctd_tests.csv               (for 03_tdr_offsets.R)
##          figures/tdr_profiles_raw_check.pdf
##          figures/tdr_profiles_labeled_8a.pdf
##          figures/tdr_profiles_trimmed_9c.pdf
##          figures/tdr_profiles_final_11.pdf
###############################################################

## ------------------------------------------ ##
# includes the following cruises (21): 
# 2018 = EN608, EN617
# 2019 = EN627, EN644
# 2020 = EN649, EN655, EN657
# 2021 = 
# 2022 = AT46, EN687
# 2023 = HRS2303, EN706, AR77
# 2024 = EN712, EN715, EN720, AE2426
# 2025 = EN727, AR88, AR92, AR95
# 2026 = AR99, ***need to add HRS2601***

# MISSING DATA
## EN661 (winter 2021) logsheets say tdr recorded but didnt find data
## EN668 (summer 2021) CTD was used; no tdr data found
## EN695 (winter 2023) tdr was used; no tdr data found 
## ------------------------------------------ ##
# no TDR data for: EN661, AR63*, AR38, AR32, EN695, EN668 (CTD only)

# .DAT files only, csv created in 01_tdr_dat_to_csv.R: EN644, EN627, EN608
# xlsx files converted to csv in 01_tdr_dat_to_csv.R

# CTD data available for: EN668 (no TDR) and EN706  [02_ctd_bongo_tidy.R]
# PxSensor data available starting AE2426           [02_px_sensor_tidy.R]

# AR95 AR99 starting with recent cruises the 20-µm ring net is deployed 
# separately from the Bongo rather than attached above it,
# resulting in separate TDR casts for the same station (e.g. B1 and R1).

## ------------------------------------------ ##
##  Packages               ----
## ------------------------------------------ ##
library(tidyverse)
library(here)
library(zoo)      # for rollmean in auto_split_casts
library(glue)     # for glue() in detect_and_split plots
library(conflicted)

## Run sessionInfo() and save output to document package versions
## > writeLines(capture.output(sessionInfo()), "session_info.txt")

source(here("R", "00_helpers.R"))

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)

## ------------------------------------------ ##
##  Constants              ----
## ------------------------------------------ ##
RAW_DIR    <- here("data", "raw", "tdr_data")
OUT_DIR    <- here("data", "processed")

## ------------------------------------------ ##
##  1. CSV files                  ----
## ------------------------------------------ ##

all_csv_paths <- list.files(RAW_DIR,
                            pattern    = "\\.csv$",
                            full.names  = TRUE,
                            recursive   = TRUE)

message(length(all_csv_paths), " CSV file(s).") # ~240+ files

## ------------------------------------------ ##
##  2. Read data & combine               ----
## ------------------------------------------ ##

all_data <- lapply(all_csv_paths, read_tdr_csv) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()
message("  Total rows: ", nrow(all_data))

length(unique(all_data$cruise)) # 21 cruises

## ------------------------------------------ ##
##  Inspect data          ----
## ------------------------------------------ ##

# --- rows per cruise ----
all_data %>%
  group_by(cruise) %>%
  summarise(n_rows = n(), .groups = "drop") %>%
  arrange(n_rows) %>%
  print(n = Inf)

# --- casts per cruise ----
all_data %>%
  distinct(cruise, station, cast) %>%
  count(cruise, name = "n_casts") %>%
  arrange(n_casts) %>%
  print(n = Inf)

# --- column names across files ----
names(all_data)

# --- Sample rows from each cruise ----
all_data %>%
  group_by(cruise) %>%
  slice_sample(n = 3) %>%
  print(n = Inf)

# --- Depth range per cruise ----
all_data %>%
  group_by(cruise) %>%
  summarise(
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    n_negative = sum(depth_m < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# --- Temp range per cruise ----
all_data %>%
  group_by(cruise) %>%
  summarise(
    min_temp = min(temp_C, na.rm = TRUE),
    max_temp = max(temp_C, na.rm = TRUE),
    n_na_temp = sum(is.na(temp_C)),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# --- Timestamp format ----
all_data %>%
  group_by(cruise) %>%
  summarise(
    min_date  = min(date_time, na.rm = TRUE),
    max_date  = max(date_time, na.rm = TRUE),
    n_na_time = sum(is.na(date_time)),
    .groups = "drop"
  ) %>%
  arrange(min_date) %>%
  print(n = Inf)

# --- Station and cast label ----
sapply(c("cruise", "station", "cast"), function(col) sort(unique(all_data[[col]])))

## ------------------------------------------ ##
##  3. Parse timestamps & clean labels  ----
## ------------------------------------------ ##

all_data <- all_data %>%
  mutate(
    # handle both character timestamps and Excel serial-date numerics
    # after forcing as.character() in read_tdr_csv, numeric serials come in
    # as strings (45603.2) detect with regex before parsing
    date_time = case_when(
      grepl("^\\d{5}\\.?\\d*$", date_time) ~
        as.POSIXct((as.numeric(date_time) - 25569) * 86400,
                   origin = "1970-01-01", tz = "UTC"),
      TRUE ~
        suppressWarnings(
        parse_date_time(as.character(date_time),
                        orders = c("dmY HMS", "Ymd HMS", "mdY IMS p",
                                   "Ymd HM", "Ymd"),
                        tz = "UTC")
        )
    ),
    # station: add L prefix where missing, strip leading zeros
    station = case_when(
      grepl("^MVCO$",  station, ignore.case = TRUE) ~ "MVCO",
      grepl("^TDRCTD", station, ignore.case = TRUE) ~ station,
      grepl("^Lu11c$",  station, ignore.case = TRUE) ~ "u11c",  # opportunistic, not on line
      grepl("^[0-9]",  station)                     ~ paste0("L", station),
      TRUE                                           ~ station
    ),
    station = strip_leading_zeros(station, "L"),
    # cast: strip leading zeros, add B prefix if missing
    cast = gsub("^B0*(\\d+.*)", "B\\1", cast),
    cast = ifelse(!grepl("^B", cast, ignore.case = TRUE),
                  paste0("B", cast), cast)
  ) %>% filter(!is.na(depth_m), !is.na(date_time)) # depth_m >= 0,
# checked warnings and not an issue

# should be 0
sum(is.na(all_data$date_time))

# date range 
range(all_data$date_time, na.rm = TRUE)

all_data %>%
  group_by(cruise) %>%
  summarise(
    min_date = min(date_time, na.rm = TRUE),
    max_date = max(date_time, na.rm = TRUE),
    n_rows   = n()
  ) %>%
  arrange(min_date) %>%
  print(n = Inf)

## ------------------------------------------ ##
##  Visual checks of data  ----
## ------------------------------------------ ##

gap_diagnostics <- all_data %>%
  filter(!is.na(depth_m), depth_m >= 0, !is.na(date_time)) %>%
  group_by(cruise, station, cast) %>%
  arrange(date_time) %>%
  mutate(
    elapsed   = as.numeric(difftime(date_time, first(date_time), units = "mins")),
    peak_idx  = which.max(depth_m),
    past_peak = row_number() > peak_idx,
    near_surf = depth_m < 5 & past_peak
  ) %>%
  filter(near_surf) %>%
  mutate(time_gap_to_next = c(diff(elapsed), NA)) %>%
  filter(!is.na(time_gap_to_next), time_gap_to_next > 2) %>%  # ignore tiny gaps
  ungroup()

# histogram of gaps
ggplot(gap_diagnostics, aes(x = time_gap_to_next)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "firebrick",
             linewidth = 0.8) +  
  labs(
    title = "Time gaps near surface after depth peak",
    subtitle = "Dashed line = 30 min",
    x = "Gap duration (minutes)", y = "Count"
  ) +
  theme_minimal()

# which specific casts have large gaps
gap_diagnostics %>%
  filter(time_gap_to_next > 2) %>%
  select(cruise, station, cast, elapsed, time_gap_to_next, depth_m) %>%
  arrange(desc(time_gap_to_next)) %>%
  print(n = 30)
rm(gap_diagnostics)

# --- depth profile plots per cruise ----
pdf(here("figures", "tdr_profiles_raw_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.3, color = "steelblue", alpha = 0.7) +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("RAW profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          strip.text   = element_text(size = 6))
  print(p)
}

dev.off()
rm(p, cr)

all_data %>%
  distinct(cruise, station, cast) %>%
  arrange(cruise, station, cast) %>%
  print(n=400)

## ---------------------------------------------------- ##
## multiple casts - need to manually inspect:
# EN617 = L11B25ab = flowmeter calibration
# EN627 = L8B19 = re-did deployment; so delete first aborted cast
# EN657 = L3B2 (this contains L1 file); L6B17; L9B8 (L9B14,L8B15)
# EN706 = L5B6 = re-did deployment; so delete first aborted cast
# AR77  = L2B2 = re-did deployment; so delete first aborted cast
# EN715 = L5B6 = re-did deployment; so delete first aborted cast
#       = L6B13 = re-did deployment; so delete first aborted cast
#       = L8B14 = re-did deployment; so delete first aborted cast
# AE2426 = L8B13 = up/downs before actual cast
# AR99 = L2B3  = the second cast is a ring net only at same L2R3
#      = L6B10 = the second cast is a ring net only at same L2R3
#      = L9B5  = the second cast is a ring net only at same L2R3
## ---------------------------------------------------- ##

## ------------------------------------------ ##
# NOTES ABOUT CASTS
# 2018 = EN608 = no duplicates;
#              = missing data for L6B12; L11B19; L10B21: L9B22; L7B23; MVCO B30
# 2018 = EN617 = no duplicates; L11B25a and L11B25ab tow calib
#              = no TDR for L1B1

# 2019 = EN627 = two casts L8B19 = first one hit bottom and redid cast
# 2019 = EN644 = good; complete data

# 2020 = EN649 = good; complete data
# 2020 = EN655 = good; complete data
#              = L9B15 has tdr cast but no sample hit bottom no time to re-do
#              = L3B3 TDR turned on after net in water (data starts at ~3m) 
# 2020 = EN657 = 3 with multiple casts L3B2 (this contains L1 cast); L6B17; L9B8 (L9B14,L8B15)
#              = L1B1 TDR turned on after net in water (data starts at ~4m) 
#              = L4B6 TDR turned on after net in water (data starts at ~6m) 

# 2021 = 

# 2022 = AT46  = L6B4 needs to have cast renamed to B6 (L6B6)
#              = no TDR for L8B13

# 2022 = EN687 = good; complete data

# 2023 = HRS2303 = good; complete data
# 2023 = EN706   = L5B6 re-did deployment; so delete first aborted cast
#                = L7B13 rename to L7B14 (typo)
# 2023 = AR77    = no data for L1B1
#                = L2B2 have multiple casts had to re-deploy; delete first cast

# 2024 = EN712  = no data for L8B12 and L3B16
#               = L6B5 hit bottom = no sample = tdr cast but no sample
# 2024 = EN715  = L5B6; L6B13; L8B14 = hit bottom and re-did cast; delete first bad cast
# 2024 = EN720  = good; complete data
# 2024 = AE2426 = L9B12 upcast only (data starts at ~85m)
#               = L8B13 funky stuff before start of actual cast
#               = L11B9 = typo; should be L11B10

# 2025 = EN727 = good; complete data
# 2025 = AR88  = good; complete data
# 2025 = AR92  = good; complete data
# 2025 = AR95  = L3B19 TDR turned on after net in water (data starts at ~19m)

# 2026 = AR99  = L2B3 = the second cast is a ring net only at same L2R3
#              = L9B5 = the second cast is a ring net only at same L9R5
#              = L10B6 = TDR turned on after net in water (data starts at ~37m)
#              = L6B10 = the second cast is a ring net only at same L6R10

# 2026 = ***need to add HRS2601***
## ------------------------------------------ ##

## ------------------------------------------ ##
##  Fix cast label typos                   ----
## ------------------------------------------ ##
# confirmed typos from logsheet+elog cross-check
# AT46   = L6B4  = typo; should be L6B6
# AE2426 = L11B9 = typo; should be L11B10
# EN657  = L9B8  = typo; should be L9B14
# EN706  = L7B13 = typo; should be L7B14
all_data <- all_data %>%
  mutate(cast = case_when(
    cruise == "AT46"   & station == "L6"  & cast == "B4"  ~ "B6",
    cruise == "AE2426" & station == "L11" & cast == "B9"  ~ "B10",
    cruise == "EN657"  & station == "L9"  & cast == "B8"  ~ "B14",
    cruise == "EN706"  & station == "L7"  & cast == "B13" ~ "B14",
    TRUE ~ cast
  ))

# verify
all_data %>%
  filter(
    (cruise == "AT46"     & station == "L6")  |
      (cruise == "AE2426" & station == "L11") |
      (cruise == "EN657"  & station == "L9" ) |
      (cruise == "EN706"  & station == "L7" )
  ) %>%
  distinct(cruise, station, cast)

## ------------------------------------------ ##
##  4. Isolate TDR-CTD bench tests   ----
## ------------------------------------------ ##
# at a couple of cruises; the TDR was attached to the shiboard/regular 
# CTD cast to then compare max depth between CTD and TDR and apply depth 
# offset if needed. this is dealt with in 03_tdr_offsets

# u9a (AR92) == TDR-CTD test
tdr_test <- filter(all_data,
                   grepl("^TDRCTD", station, ignore.case = TRUE) |
                     station == "u9a") %>%
  mutate(comments = "tdr_ctd_test",
         station = case_when(
           station == "u9a"  ~ "u9a",    # AR92: keep as-is
           grepl("BL\\d+B", cast) ~ paste0("L", str_extract(cast, "(?<=BL)\\d+(?=B)")),  # EN715, EN720
           grepl("BL\\d+test", cast) ~ paste0("L", str_extract(cast, "(?<=BL)\\d+(?=test)")),  # AT46
           TRUE ~ NA_character_          # EN712: Btest = no station
         ),
         cast = case_when(
           station == "u9a"  ~ "18",    # AR92: keep as-is
           grepl("B\\d+test", cast) ~ str_extract(cast, "(?<=B)\\d+(?=test)"),  # EN715, EN720
           TRUE ~ NA_character_          # AT46 BL2test, EN712 Btest = no cast
         ))

if (nrow(tdr_test) > 0) {
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
  write_csv(tdr_test, here(OUT_DIR, "tdr_ctd_tests.csv"))
  message("  Saved ", nrow(tdr_test), " TDR-CTD test rows -> tdr_ctd_tests.csv")
}

# EN617 L11 B25a and B25ab == flowmeter calibration can filter out these
# remove tests + flowmeter cals from main data
all_data <- filter(all_data,
                   !grepl("^TDRCTD", station, ignore.case = TRUE),
                   station != "u9a",
                   !(cruise == "EN617" & station == "L11" & 
                       cast %in% c("B25a", "B25ab")))
rm(tdr_test)

## ------------------------------------------ ##
##  5. Fix/Validate TDR timestamps          ----
## ------------------------------------------ ##

### --- Load elog data --- ###
# elog data fixed and created in nes-lter-api-pulls.Rproj
# 01_elog_pull.R
# https://github.com/cabanelas/nes-lter-api-pulls
elog <- read_csv(file.path("data", "raw",
                           "elog_zoop_tows_thruAR99_2026-04-14.csv"))

# pivot elog to get deploy and recover times in same row
elog_wide <- elog %>%
  filter(action %in% c("deploy", "recover")) %>%
  select(cruise, station, cast, action, datetime8601) %>%
  pivot_wider(names_from = action, values_from = datetime8601) %>%
  rename(elog_deploy = deploy, elog_recover = recover) %>%
  mutate(elog_duration_min = as.numeric(difftime(elog_recover, elog_deploy, units = "mins")))

# get TDR time range per cast
tdr_times <- all_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_start = min(date_time, na.rm = TRUE),
    tdr_end   = max(date_time, na.rm = TRUE),
    tdr_duration_min = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    .groups = "drop"
  )

# join and compute offset
timestamp_check <- tdr_times %>%
  left_join(elog_wide, by = c("cruise", "station", "cast")) %>%
  mutate(
    offset_deploy_min  = as.numeric(difftime(tdr_start, elog_deploy,  units = "mins")),
    offset_recover_min = as.numeric(difftime(tdr_end,   elog_recover, units = "mins")),
    flag_no_elog       = is.na(elog_deploy),
    flag_large_offset  = abs(offset_deploy_min) > 30 | abs(offset_recover_min) > 30
  ) %>%
  arrange(desc(abs(offset_deploy_min)))

## --- on some cruises TDR computer was local time so need to adjust to UTC
# review flagged ones
timestamp_check %>%
  filter(flag_large_offset | flag_no_elog) %>%
  select(cruise, station, cast, tdr_start, elog_deploy, offset_deploy_min,
         tdr_end, elog_recover, offset_recover_min) %>%
  print(n = Inf, width = Inf)

timestamp_check %>%
  group_by(cruise) %>%
  summarise(
    n_casts = n(),
    n_flag_large_offset  = sum(flag_large_offset, na.rm = TRUE),
    n_flag_no_elog       = sum(flag_no_elog),
    median_deploy_offset = median(offset_deploy_min, na.rm = TRUE),
    sd_deploy_offset     = sd(offset_deploy_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(median_deploy_offset)))
# in some more recent cruises the laptop wasnt set up to UTC/TDR recorded
# local time, need to adjust

timestamp_check %>%
  filter(flag_no_elog, !grepl("_\\d+$", cast)) %>%
  select(cruise, station, cast) %>%
  arrange(cruise, station, cast)
# EN655 = L9B15 hit bottom = no sample = tdr cast but no sample
# EN712  = L6B5 hit bottom = no sample = tdr cast but no sample

tdr_maxdepth <- all_data %>%
  group_by(cruise, station, cast) %>%
  slice_max(depth_m, n = 1, with_ties = FALSE) %>%
  select(cruise, station, cast, time_at_maxdepth = date_time, maxdepth = depth_m) %>%
  ungroup()

maxdepth_check <- tdr_maxdepth %>%
  left_join(elog_wide, by = c("cruise", "station", "cast")) %>%
  mutate(
    offset_maxdepth_deploy  = as.numeric(difftime(time_at_maxdepth, elog_deploy,  units = "mins")),
    offset_maxdepth_recover = as.numeric(difftime(time_at_maxdepth, elog_recover, units = "mins")),
    # max depth should fall AFTER deploy and BEFORE recover
    flag_outside_window = offset_maxdepth_deploy < 0 | (!is.na(elog_recover) & offset_maxdepth_recover > 0)
  )

maxdepth_check %>%
  group_by(cruise) %>%
  summarise(
    n_casts             = n(),
    n_flag              = sum(flag_outside_window, na.rm = TRUE),
    median_offset_deploy = median(offset_maxdepth_deploy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_flag > 0) %>%
  arrange(desc(n_flag))

# =============================================================================
# TIMESTAMP CORRECTION NOTES
# Based on timestamp_check output - TDR local time vs UTC offset review
# offset_deploy_min = tdr_start - elog_deploy (negative = TDR clock behind elog/UTC)
# =============================================================================

# --- CRUISE-LEVEL TIMEZONE CORRECTIONS NEEDED ---
# These cruises show consistent ~same offset across all casts (both deploy AND
# recover negative by similar magnitude) TDR computer was in local time (EDT = UTC-4
# or EST = UTC-5)
# Fix by adding hours to tdr datetime column for these cruises

# cruises with systematic clock offsets to correct
clock_offsets <- tribble(
  ~cruise,   ~offset_hrs,
  "EN706",   4, # manually checked
  "EN712",   5, # manually checked
  "EN715",   4, # manually checked
  "EN720",   4  # manually checked
)

# stations on HRS2303 that have correct time (no adjustment needed)
hrs2303_good_stations <- c("L3", "L4", "L6")

# manually checked; L6, L4, L3,  time are good
# apply bulk cruise-level corrections + exceptions
all_data <- all_data %>%
  mutate(date_time = case_when(
    # bulk cruise-level tz corrections
    cruise %in% clock_offsets$cruise ~
      date_time + hours(clock_offsets$offset_hrs[match(cruise, clock_offsets$cruise)]),
    # HRS2303: all stations except the good ones need +4 hrs
    cruise == "HRS2303" & !station %in% hrs2303_good_stations ~ date_time + hours(4),
    # EN617 L1B1 is a bad data cast and is delete below
    TRUE ~ date_time
  ))

# MANUALLY CHECKED TIMESTAMP against all-nes-lter-bongologs EN608, EN617,
# EN627, EN644, EN649, EN655, EN657, AT46, EN687, HRS2303, EN706, AR77,
# EN712, EN715, EN720, AE2426, EN727, AR88, AR92, AR95, AR99 

tdr_times_corrected <- all_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_start        = min(date_time, na.rm = TRUE),
    tdr_end          = max(date_time, na.rm = TRUE),
    tdr_duration_min = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    .groups = "drop"
  )

timestamp_check_corrected <- tdr_times_corrected %>%
  left_join(elog_wide, by = c("cruise", "station", "cast")) %>%
  mutate(
    offset_deploy_min  = as.numeric(difftime(tdr_start, elog_deploy, units = "mins")),
    duration_diff_min  = tdr_duration_min - elog_duration_min,
    flag_no_elog       = is.na(elog_deploy),
    flag_large_offset  = abs(offset_deploy_min) > 60,   # >1hr deploy offset = possible clock issue
  )
rm(elog, elog_wide, tdr_times, tdr_times_corrected, 
   timestamp_check, timestamp_check_corrected, tdr_maxdepth, maxdepth_check,
   clock_offsets, hrs2303_good_stations)

## ------------------------------------------ ##
##  6. Split merged multi-cast files       ----
## ------------------------------------------ ##
# detect_and_split() checks whether a single file contains two or more casts
# back-to-back tows (depth goes deep, returns to surface, then goes deep
# these are either: 2 stations in 1 file; or bongo hit bottom and it was retried

all_data <- all_data %>%
  group_by(cruise, station, cast) %>%
  do({ detect_and_split(., base_cast = .$cast[1]) }) %>%
  ungroup()

# see which got split
all_data %>%
  distinct(cruise, station, cast) %>%
  filter(grepl("_\\d+$", cast)) %>%
  arrange(cruise, station, cast) # 14

n_split <- all_data %>%
  distinct(cruise, station, cast) %>%
  filter(grepl("_\\d+$", cast)) %>%
  nrow()

if (n_split > 0) {
  message("  ", n_split, " cast(s) were auto-split. ",
          "Review")
} else {
  message("  No merged multi-cast files detected.")
}
rm(n_split)

## ------------------------------------------ ##
##  7. Manual corrections  ----
## ------------------------------------------ ##

## ------------------------------------------ ##
##  7a. Manual resolution of auto-split casts ---
## ------------------------------------------ ##
## --- auto split in step 6 ---
# -- manually identify whether second cast is aborted or a different station
all_data %>%
  filter(grepl("_\\d+$", cast)) %>%
  group_by(cruise, station, cast) %>%
  summarise(
    max_depth = max(depth_m, na.rm = TRUE),
    t_start   = min(date_time, na.rm = TRUE),
    t_end     = max(date_time, na.rm = TRUE),
    duration_min = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    .groups   = "drop"
  ) %>%
  mutate(
    t_start = format(t_start, "%Y-%m-%d %H:%M:%S"),
    t_end   = format(t_end,   "%Y-%m-%d %H:%M:%S")
  ) %>%
  arrange(cruise, station, cast) %>%
  print(n = Inf, width = Inf)

## --- Plot depth profiles for all auto-split casts (for manual review) ---
split_casts <- all_data %>%
  filter(grepl("_\\d+$", cast)) %>%
  mutate(base_cast = sub("_\\d+$", "", cast))   # e.g. B13_1 -> B13

# one panel per cruise+station+base_cast combo
split_groups <- split_casts %>%
  distinct(cruise, station, base_cast)

plots <- pmap(split_groups, function(cruise, station, base_cast) {
  df <- split_casts %>%
    filter(cruise == !!cruise,
           station == !!station,
           base_cast == !!base_cast)
  
  # thin to ~1-sec resolution so plots aren't sluggish
  df_thin <- df %>%
    arrange(date_time) %>%
    slice(seq(1, n(), by = max(1L, floor(n() / 500))))
  
  ggplot(df_thin, aes(x = date_time, y = depth_m, color = cast)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.6, alpha = 0.5) +
    scale_y_reverse() +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 mins") +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = glue::glue("{cruise}  |  {station}  |  base cast: {base_cast}"),
      subtitle = glue::glue(
        "Split 1: {format(min(df$date_time[df$cast == paste0(base_cast,'_1')]), '%H:%M:%S')}",
        " – {format(max(df$date_time[df$cast == paste0(base_cast,'_1')]), '%H:%M:%S')}  |  ",
        "Split 2: {format(min(df$date_time[df$cast == paste0(base_cast,'_2')]), '%H:%M:%S')}",
        " – {format(max(df$date_time[df$cast == paste0(base_cast,'_2')]), '%H:%M:%S')}"
      ),
      x     = "Time (UTC)",
      y     = "Depth (m)",
      color = "Cast"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
})

walk(plots, print)

## --- Manual resolution of auto-split casts ---
# need to identify what each one is 
# AE2426 = 3 up/downs unsure for what; before actual cast
#         L8 B13_1 = 6 min, 17 m   -> aborted/test deploy -> DROP
#         L8 B13_2 = 84 min, 131 m -> real tow            -> rename to B13
# **THIS STILL NEEDS A FIX due to multiple up/downs; real cast ~19:10-19:40
#
# AR77   = re-deployed; delete first aborted cast
#        L2 B2_1  = 32 min, 36 m   -> tow 1               -> DROP
#        L2 B2_2  = 25 min, 38 m   -> tow 2               -> rename to B2
#
# AR99   = first cast bongo net; second cast is ring net only
#        L9 B5_1                   -> bongo tow           -> rename to B5 
#        L9 B5_2                   -> ring tow            -> rename to R5
#
# EN657  = no elog for B17_1. no comment on bongo logsheets
#        L6 B17_1 = 69 min, 95 m   -> UNCLEAR             -> DROP for now
#        L6 B17_2 = 64 min, 74 m   -> real tow            -> rename to B17
# the 74m matches written TDR depth on bongo logsheet
#
# EN712  = jus strange auto cut; not really 2 casts 
#        L10 B7_1 = actual cast    -> tow                 -> rename to B7
#        L10 B7_2 = surface read   -> tow                 -> DROP
#
# EN715  = re-deployed; delete first aborted cast
#        L6 B13_1 = 15 min, 90 m   -> tow 1               -> DROP               
#        L6 B13_2 = 9 min,  82 m   -> tow 2               -> rename to B13
#
# EN715  = re-deployed; delete first aborted cast
#        L8 B14_1 = 28 min, 136 m  -> tow 1               -> DROP
#        L8 B14_2 = 13 min, 137 m  -> tow 2               -> rename to B14

## --- Manual resolution of auto-split casts ---
all_data <- all_data %>%
  mutate(cast = case_when(
    # AE2426 L8: B13_1 aborted test; B13_2 real cast
    #             still has multiple up/downs; real cast ~19:10-19:40)
    cruise == "AE2426" & station == "L8" & cast == "B13_2" ~ "B13",
    
    # AR77 L2: B2_1 aborted re-deploy; B2_2 real cast
    cruise == "AR77"   & station == "L2" & cast == "B2_2"  ~ "B2",
    
    # AR99 L6: B10_1 bongo, B10_2 ring net
    # after switching to keeping neg values this one isnt getting caught
    # fixed manually below 
    # cruise == "AR99"   & station == "L6" & cast == "B10_1" ~ "B10",
    # cruise == "AR99"   & station == "L6" & cast == "B10_2" ~ "R10",
    
    # AR99 L9: B5_1 bongo, B5_2 ring net
    cruise == "AR99"   & station == "L9" & cast == "B5_1"  ~ "B5",
    cruise == "AR99"   & station == "L9" & cast == "B5_2"  ~ "R5",
    
    # EN657 L6: B17_1 unclear/no elog; B17_2 real cast
    cruise == "EN657"  & station == "L6" & cast == "B17_2" ~ "B17",
    
    # EN712 L10: B7_1 bongo, B7_2 extra surface time 
    cruise == "EN712"  & station == "L10"& cast == "B7_1"  ~ "B7",
    
    # EN715 L6: B13_1 aborted; B13_2 real cast
    cruise == "EN715"  & station == "L6" & cast == "B13_2" ~ "B13",
    
    # EN715 L8: B14_1 aborted; B14_2 real cast
    cruise == "EN715"  & station == "L8" & cast == "B14_2" ~ "B14",
    
    TRUE ~ cast
  )) %>%
  filter(
    !(cruise == "AE2426" & station == "L8" & cast == "B13_1"),  # aborted
    !(cruise == "AR77"   & station == "L2" & cast == "B2_1"),   # aborted re-deploy
    !(cruise == "EN657"  & station == "L6" & cast == "B17_1"),  # unclear,no elog 
    !(cruise == "EN712"  & station == "L10"& cast == "B7_2"),
    # -- EN657 L6 B17 ---
    # B17_1 = 2020-10-17 00:21-00:38 not sure what this one is == cant find notes on logsheet == went down to 95
    # B17_2 = 2020-10-17 01:14-01:28 == L6B17 == went down to 72
    !(cruise == "EN715"  & station == "L6" & cast == "B13_1"),  # aborted
    !(cruise == "EN715"  & station == "L8" & cast == "B14_1")   # aborted
  )

# verify: no _1/_2 suffixes should remain
remaining_splits <- all_data %>%
  distinct(cruise, station, cast) %>%
  filter(grepl("_\\d+$", cast))

if (nrow(remaining_splits) == 0) {
  message("  All auto-splits resolved.")
} else {
  message("  WARNING: ", nrow(remaining_splits), " unresolved split cast(s):")
  print(remaining_splits)
}

rm(split_casts, split_groups, plots, remaining_splits)

## ------------------------------------------ ##
##  7b. Manual corrections: missed auto-splits ----
## ------------------------------------------ ##
## these were NOT caught by detect_and_split() in step 6 because
## the valley was not shallow enough, or timing was ambiguous.
## Identified via raw_profiles_check.pdf + elog/logsheet cross-check
## For files with 2 real casts, define the time boundary
## manually split & assign correct cast
##
## --- Cases:
# EN627  = L8B19 = re-did deployment; so delete first aborted cast
# EN657  = L3B2  = this contains L1 file; L9B8 (L9B14,L8B15)
# EN706  = L5B6  = re-did deployment; so delete first aborted cast
# EN715  = L5B6  = re-did deployment; so delete first aborted cast
# AE2426 = L8B13 = funky stuff before start of actual cast
# AR99   = L2B3  = the second cast is a ring net only at same L2R3
# AR99   = L6B10 = the second cast is a ring net only at same L6R10

## --- Interactive plot to confirm time boundaries ---
library(plotly)

missed_splits <- all_data %>%
  filter(
    (cruise == "EN627" & station == "L8" & cast == "B19") |
    (cruise == "EN657" & station == "L9" & cast == "B8")  |
    (cruise == "EN657" & station == "L3" & cast == "B2")  |
    (cruise == "EN706" & station == "L5" & cast == "B6")  |
    (cruise == "EN715" & station == "L5" & cast == "B6")  |
    (cruise == "AE2426"& station == "L8" & cast == "B13") |
    (cruise == "AR99"  & station == "L2" & cast == "B3")  |
    (cruise == "AR99"  & station == "L6" & cast == "B10")
  ) %>%
  mutate(label = paste(cruise, station, cast))

p <- ggplot(missed_splits, aes(x = date_time, y = depth_m,
                               text = paste("time:", date_time,
                                            "<br>depth:", round(depth_m, 1)))) +
  geom_point(size = 0.1, color = "steelblue", alpha = 0.5) +
  scale_y_reverse() +
  facet_wrap(~label, scales = "free") +
  labs(x = NULL, y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplotly(p, tooltip = "text")

rm(missed_splits, p)

# -- EN627 = L8B19 --- re-did deployment; delete first aborted cast
# aborted tow = 2019-02-03 18:22-18:52 == DELETE
# real tow    = 2019-02-03 18:55-19:15 == keep
# -- EN657 = L3B2 ---
# 1st tow = 2020-10-13 18:25-18:37     == rename L1B1
# 2nd tow = 2020-10-14 00:25-00:53     == rename L3B2
# -- EN657 = L9B8 ---
# 1st tow = 2020-10-16 10:12-10:34     == rename L9B14
# 2nd tow = 2020-10-16 12:12-12:35     == rename L8B15
# -- EN706 = L5B6 --- re-did deployment; delete first aborted cast
# 1st tow = 2023-08-08 21:08-21:16     == DELETE
# 2nd tow = 2023-08-08 21:19-21:34     == keep
# -- EN715 = L5B6 --- re-did deployment; delete first aborted cast
# interval was not set to 1 sec (less points)
# 1st tow = 2024-05-04 15:21-15:32     == DELETE
# 2nd tow = 2024-05-04 15:40-15:55     == keep
# --- AE2426 = L8B13 --- funky stuff before start of actual cast
# 1st tow = 2024-11-09 18:20-19:00     == DELETE
# 2nd tow = 2024-11-09 19:12-19:40     == keep
# -- AR99 = L2B3 --- the second cast is a ring net only at same L2R3
# this one may be tricky bc interval was not set to 1 sec (less points)
# 1st tow = 2026-01-14 05:08-05:17 == L2B3
# 2nd tow = 2026-01-14 05:43-05:54 == L2R3 (ring net done separate; update cast name)
# -- AR99 = L6 B10 --- first cast bongo net; second cast is ring net only
# 1st tow = 2026-01-15 21:51-22:20 == L6B10                      
# 2nd tow = 2026-01-15 22:56-23:29 == L6R10 (ring net done separate; update cast name)

## ------------------------------------------ ##
## Time boundaries (confirmed from plots + logsheet/elog)
## ------------------------------------------ ##

# EN627 L8 B19: aborted + re-deploy
en627_L8_B19_split  <- as.POSIXct("2019-02-03 18:53:00", tz = "UTC")

# EN657 L3 B2: two different stations in one file
en657_L3_B2_split   <- as.POSIXct("2020-10-13 23:00:00", tz = "UTC")

# EN657 L9 B8 (already renamed to B14 in typo fix): two different stations
en657_L9_B14_split  <- as.POSIXct("2020-10-16 11:00:00", tz = "UTC")

# EN706 L5 B6: aborted + re-deploy (times already UTC-corrected +4 hrs)
en706_L5_B6_split   <- as.POSIXct("2023-08-08 21:17:00", tz = "UTC")

# EN715 L5 B6: aborted + re-deploy (times already UTC-corrected +4 hrs)
en715_L5_B6_split   <- as.POSIXct("2024-05-04 15:36:00", tz = "UTC")

# AE2426 L8 B13: funky pre-cast up/downs (already renamed from B13_2 in step 7)
ae2426_L8_B13_split <- as.POSIXct("2024-11-09 19:10:00", tz = "UTC")

# AR99 L2 B3: bongo + ring net in one file
ar99_L2_B3_split    <- as.POSIXct("2026-01-14 05:30:00", tz = "UTC")

# AR99 L6 B10: bongo + ring net in one file
ar99_L6_B10_split   <- as.POSIXct("2026-01-15 22:40:00", tz = "UTC")

## ------------------------------------------ ##
## Apply corrections
## ------------------------------------------ ##

all_data <- all_data %>%
  mutate(
    # --- station corrections (must come before cast corrections) ---
    station = case_when(
      # EN657 L3B2: first half is actually L1
      cruise == "EN657" & station == "L3" & cast == "B2" &
        date_time <= en657_L3_B2_split   ~ "L1",
      # EN657 L9B14: second half is actually L8
      cruise == "EN657" & station == "L9" & cast == "B14" &
        date_time >  en657_L9_B14_split  ~ "L8",
      TRUE ~ station
    ),
    # --- cast corrections ---
    cast = case_when(
      # EN657 L3B2: first half = L1B1
      cruise == "EN657" & station == "L1" & cast == "B2" &
        date_time <= en657_L3_B2_split   ~ "B1",
      # EN657 L9B14: second half = L8B15
      cruise == "EN657" & station == "L8" & cast == "B14" &
        date_time >  en657_L9_B14_split  ~ "B15",
      # AR99 L2B3: second half = ring net R3
      cruise == "AR99"  & station == "L2" & cast == "B3"  &
        date_time >  ar99_L2_B3_split    ~ "R3",
      # AR99 L6B10: second half = ring net R10
      cruise == "AR99"  & station == "L6" & cast == "B10"  &
        date_time >  ar99_L6_B10_split    ~ "R10",
      TRUE ~ cast
    )
  ) %>%
  ## --- drop aborted / pre-cast rows ---
  filter(
    !(cruise == "EN627"  & station == "L8" & cast == "B19" &
        date_time < en627_L8_B19_split),
    !(cruise == "EN706"  & station == "L5" & cast == "B6"  &
        date_time < en706_L5_B6_split),
    !(cruise == "EN715"  & station == "L5" & cast == "B6"  &
        date_time < en715_L5_B6_split),
    !(cruise == "AE2426" & station == "L8" & cast == "B13" &
        date_time < ae2426_L8_B13_split)
  )

## --- verify ---
all_data %>%
  filter(
    (cruise == "EN627"  & station == "L8"  & cast == "B19") |
      (cruise == "EN657"  & station %in% c("L1","L3") & cast %in% c("B1","B2")) |
      (cruise == "EN657"  & station %in% c("L8","L9") & cast %in% c("B14","B15")) |
      (cruise == "EN706"  & station == "L5"  & cast == "B6")  |
      (cruise == "EN715"  & station == "L5"  & cast == "B6")  |
      (cruise == "AE2426" & station == "L8"  & cast == "B13") |
      (cruise == "AR99"   & station == "L2"  & cast %in% c("B3","R3")) |
      (cruise == "AR99"   & station == "L6"  & cast %in% c("B10","R10"))
  ) %>%
  group_by(cruise, station, cast) %>%
  summarise(
    t_start   = min(date_time),
    t_end     = max(date_time),
    max_depth = max(depth_m),
    n         = n(),
    .groups   = "drop"
  ) %>%
  arrange(cruise, station, cast)

# no double casts should remain
for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.2, color = "steelblue", alpha = 0.7) +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("RAW profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          strip.text   = element_text(size = 6))
  print(p)
}
rm(en627_L8_B19_split, en657_L3_B2_split, en657_L9_B14_split, en706_L5_B6_split,
   en715_L5_B6_split, ae2426_L8_B13_split, ar99_L2_B3_split, p, cr)

## ------------------------------------------ ##
##  8. Label downcast / upcast        ----
## ------------------------------------------ ##
# Within each cast:
#   - find the global depth maximum
#   - detect where sustained descent begins (depth increases > 0.5 m
#     over the next 10 observations) to trim pre-deployment hang time
#   - rows before descent_start  -> "predeploy"
#   - rows to depth maximum      -> "downcast"
#   - rows after depth maximum   -> "upcast"

all_data <- all_data %>%
  group_by(cruise, station, cast) %>%
  do(label_down_up(.)) %>% # function in 00_helpers.R
  ungroup()

all_data %>%
  count(down_up) %>%
  print()

## ------------------------------------------ ##
##  8a. Post-label profile plots           ----
## ------------------------------------------ ##
# profiles (predeploy / downcast / upcast) to PDF.

pdf(here("figures", "tdr_profiles_labeled_8a.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
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
    labs(title = paste("Labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text  = element_text(size = 6),
          legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()
rm(p, cr)

## ------------------------------------------ ##
##  8b. Manual down_up corrections         ----
## ------------------------------------------ ##
# EN617 L1B1 = flat line? does the depth ever change? maybe bad cast?
all_data %>%
  filter((cruise == "EN617" & station == "L1"   & cast == "B1")) %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_m), .groups = "drop") %>%
  arrange(cruise, station, cast) 

## --- delete bad cast --- ##
all_data <- all_data %>%
  filter(!(cruise == "EN617" & station == "L1" & cast == "B1"))

## ------------------------------------------ ##
##  9. Trim casts to elog deploy/recover times ----
## ------------------------------------------ ##
# first tried using the elog times to trim, but some discrepancies (time lags)
# using times from bongo log sheets

# created in nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
meta <- read_csv(file.path("data", "raw",
                           "all-nes-lter-bongologs-20260526.csv"))

## ------------------------------------------ ##
##  Parse logsheet times from meta ----
## ------------------------------------------ ##

meta_times <- meta %>%
  select(cruise, station, cast, 
         meta_deploy  = datetime_UTC_start, 
         meta_recover = datetime_UTC_end)

## --- coverage summary ---
meta_times %>%
  mutate(
    has_deploy  = !is.na(meta_deploy),
    has_recover = !is.na(meta_recover)
  ) %>%
  count(has_deploy, has_recover) %>%
  arrange(desc(has_deploy), desc(has_recover))

## ------------------------------------------ ##
##  Manually fix some times based on logsheet ----
## ------------------------------------------ ##
# HRS2303
#         L9 B6    = end should be 14:36
#         L3 B11   = end should be 23:28
# EN706
#         L4 B5    = end should be 16:29
#         L8 B15   = end should be 01:00
#         L10 B10  = start should be 00:45 (wrong on physical logsheet too)
# EN657
#         L1 B1    = start should be 18:29
#         L1 B1    = end should be 18:34
#         L7 B19   = end should be 06:34
# EN644
#         L7 B8    = end should be 03:10 (wrong on physical logsheet too)
#         L11 B 17 = end should be 03:33 (wrong on physical logsheet too)
#         L4 B23   = end should be 05:01
# EN617
#         L2 B2    = start should be 22:11 
#         L3 B5    = start should be 07:34
#         L6 B12   = start should be 02:50
#         L9 B17   = start should be 19:03
#         MVCO B15 = start should be 01:41 
# AT46
#         L6 B6    = start should be 13:49
# AE2426
#         L1 B1    = start should be 17:12
#         L2 B4    = end should be 09:27

meta_times <- meta_times %>%
  mutate(
    meta_deploy = case_when(
      cruise == "EN706"  & station == "L10" & cast == "10" ~ ymd_hms("2023-08-10 00:45:00", tz = "UTC"),
      cruise == "EN617"  & station == "L2"  & cast == "2"  ~ ymd_hms("2018-07-20 22:11:00", tz = "UTC"),
      cruise == "EN617"  & station == "L3"  & cast == "5"  ~ ymd_hms("2018-07-21 07:34:00", tz = "UTC"),
      cruise == "EN617"  & station == "L6"  & cast == "12" ~ ymd_hms("2018-07-22 02:50:00", tz = "UTC"),
      cruise == "EN617"  & station == "L9"  & cast == "17" ~ ymd_hms("2018-07-22 19:03:00", tz = "UTC"),
      cruise == "EN617"  & station == "MVCO"& cast == "15" ~ ymd_hms("2018-07-25 01:41:00", tz = "UTC"),
      cruise == "EN657"  & station == "L1"  & cast == "1" ~ ymd_hms("2020-10-13 18:29:00", tz = "UTC"),
      cruise == "AT46"   & station == "L6"  & cast == "6"  ~ ymd_hms("2022-02-17 13:49:00", tz = "UTC"),
      cruise == "AE2426" & station == "L1"  & cast == "1"  ~ ymd_hms("2024-11-06 17:12:00", tz = "UTC"),
      TRUE ~ meta_deploy
    ),
    meta_recover = case_when(
      cruise == "HRS2303" & station == "L9"  & cast == "6"  ~ ymd_hms("2023-05-03 14:36:00", tz = "UTC"),
      cruise == "HRS2303" & station == "L3"  & cast == "11" ~ ymd_hms("2023-05-04 23:28:00", tz = "UTC"),
      cruise == "EN706"   & station == "L4"  & cast == "5"  ~ ymd_hms("2023-08-08 16:29:00", tz = "UTC"),
      cruise == "EN706"   & station == "L8"  & cast == "15" ~ ymd_hms("2023-08-11 01:00:00", tz = "UTC"),
      cruise == "EN657"   & station == "L1"  & cast == "1"  ~ ymd_hms("2020-10-13 18:34:00", tz = "UTC"),
      cruise == "EN657"   & station == "L7"  & cast == "19" ~ ymd_hms("2020-10-17 06:34:00", tz = "UTC"),
      cruise == "EN644"   & station == "L7"  & cast == "8"  ~ ymd_hms("2019-08-22 03:10:00", tz = "UTC"),
      cruise == "EN644"   & station == "L11" & cast == "17" ~ ymd_hms("2019-08-23 03:33:00", tz = "UTC"),
      cruise == "EN644"   & station == "L4"  & cast == "23" ~ ymd_hms("2019-08-24 05:01:00", tz = "UTC"),
      cruise == "AE2426"  & station == "L2"  & cast == "4"  ~ ymd_hms("2024-11-07 09:27:00", tz = "UTC"),
      TRUE ~ meta_recover
    )
  )

## ------------------------------------------ ##
##  Which cruises / casts are missing times? ----
## ------------------------------------------ ##

meta_times %>%
  summarise(
    n_missing_deploy  = sum(is.na(meta_deploy)),
    n_missing_recover = sum(is.na(meta_recover)),
    .by = cruise
  ) %>%
  filter(n_missing_deploy > 0 | n_missing_recover > 0) %>%
  arrange(cruise)

## ------------------------------------------ ##
##  9. Trim casts to logsheet deploy/recover times ----
## ------------------------------------------ ##
# casts with no meta_deploy pass through untrimmed (ring nets, missing logsheet times)
# casts with deploy only: trim start, no end cut -> flag for manual review
# casts with both: trim both ends

BUFFER_SECS <- 180  # 3min buffer on each side

tdr_trim <- all_data %>%
  mutate(cast_join = str_remove(cast, "^[BR]")) %>%   # B1 -> 1, R19 -> 19
  left_join(meta_times, by = c("cruise", "station", "cast_join" = "cast")) %>%
  filter(
    grepl("^R", cast) |          # ring nets: skip trim, no logsheet times
    is.na(meta_deploy) |
      (is.na(meta_recover) &
         date_time >= meta_deploy - BUFFER_SECS) |
      (date_time >= meta_deploy  - BUFFER_SECS &
         date_time <= meta_recover + BUFFER_SECS)
  ) %>%
  select(-meta_deploy, -meta_recover, -cast_join)

## --- flag casts that need manual review (deploy only, no recover time) ---
cruises_with_tdr <- all_data %>%
  distinct(cruise)

manual_review <- meta_times %>%
  filter(cruise %in% cruises_with_tdr$cruise) %>%  # only cruises with TDR
  filter(!is.na(meta_deploy), is.na(meta_recover)) %>%
  select(cruise, station, cast, meta_deploy) %>%
  arrange(cruise, station, cast)

print(manual_review)

## --- which casts had no meta match at all (fully untrimmed) ---
tdr_trim %>%
  filter(cruise %in% cruises_with_tdr$cruise) %>%  # only cruises with TDR
  distinct(cruise, station, cast) %>%
  mutate(cast_join = str_remove(cast, "^B")) %>%
  anti_join(meta_times %>% filter(!is.na(meta_deploy)),
            by = c("cruise", "station", "cast_join" = "cast")) %>%
  select(-cast_join) %>%
  arrange(cruise, station, cast)

rm(meta_times, BUFFER_SECS, cruises_with_tdr, manual_review)

## ------------------------------------------ ##
##  9a. Post-trim profile plots  ----
## ------------------------------------------ ##
for (cr in sort(unique(tdr_trim$cruise))) {
  df <- tdr_trim %>% filter(cruise == cr)
  if (nrow(df) == 0) next
  
  p <- df %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
    geom_point(size = 0.8, alpha = 0.6) +
    scale_y_reverse() +
    scale_color_manual(
      values = c(predeploy = "grey70",
                 downcast  = "steelblue",
                 upcast    = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Post-trim labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x    = element_blank(),
          strip.text     = element_text(size = 6),
          legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}
rm(df, p, cr)

## ------------------------------------------ ##
##  9b. Post-trim manual fixes  ----
## ------------------------------------------ ##
# not really a big time discrepancy or anything; just cutting off long tails 
# AR99 ring cast times missing
#         L2 R3    = 2026-01-14 05:43:35-05:53:29
#         L9 R5    = 2026-01-14 23:23:00-00:03:00
#         L6 R10   = 2026-01-15 22:57:00-23:23:00
manual_fixes <- tribble(
  ~cruise,  ~station, ~cast, ~fix_type, ~fix_time,
  "EN720",  "L4",     "B6",  "start",   "2024-09-07 09:53:17",
  "EN720",  "L9",     "B19", "start",   "2024-09-10 07:14:42",
  "EN715",  "L2",     "B2",  "start",   "2024-05-04 04:30:35",
  "EN712",  "L5",     "B2",  "start",   "2024-02-10 04:47:42",
  "AE2426", "L1",     "B1",  "start",   "2024-11-06 17:09:00",
  "EN687",  "L1",     "B1",  "start",   "2022-07-29 19:50:00",
  "AR99",   "L2",     "R3",  "start",   "2026-01-14 05:43:35",
  "AR99",   "L9",     "R5",  "start",   "2026-01-14 23:23:00",
  "AR99",   "L6",     "R10", "start",   "2026-01-15 22:57:00",
  "AR99",   "L2",     "R3",  "end",     "2026-01-14 05:53:29",
  "AR99",   "L9",     "R5",  "end",     "2026-01-14 00:03:00",
  "AR99",   "L6",     "R10", "end",     "2026-01-15 23:23:00",
  "EN687",  "L1",     "B1",  "end",     "2022-07-29 19:55:00",
  "EN687",  "L2",     "B2",  "end",     "2022-07-30 04:01:11",
  "AE2426", "L2",     "B4",  "end",     "2024-11-07 09:27:06",
  "HRS2303","L3",     "B11", "end",     "2023-05-04 23:28:21",
  "HRS2303","L4",     "B10", "end",     "2023-05-04 19:22:12",
  "EN706",  "L1",     "B1",  "end",     "2023-08-07 18:13:00",
  "EN617",  "MVCO",   "B35", "end",     "2018-07-25 01:46:18",
  "EN657",  "MVCO",   "B20", "end",     "2020-10-18 02:11:36",
  "AE2426", "L1",     "B1",  "end",     "2024-11-06 17:17:06"
) %>%
  mutate(fix_time = as.POSIXct(fix_time, tz = "UTC"))

# start fixes: remove everything before fix_time
start_fixes <- manual_fixes %>% filter(fix_type == "start")
# end fixes: remove everything after fix_time
end_fixes   <- manual_fixes %>% filter(fix_type == "end")

tdr_trim <- tdr_trim %>%
  left_join(start_fixes %>% select(cruise, station, cast, fix_time) %>%
              rename(start_cut = fix_time),
            by = c("cruise", "station", "cast")) %>%
  left_join(end_fixes %>% select(cruise, station, cast, fix_time) %>%
              rename(end_cut = fix_time),
            by = c("cruise", "station", "cast")) %>%
  filter(
    (is.na(start_cut) | date_time >= start_cut),
    (is.na(end_cut)   | date_time <= end_cut)
  ) %>%
  select(-start_cut, -end_cut)
rm(manual_fixes, start_fixes, end_fixes)

## ------------------------------------------ ##
##  9c. Post-trim profile plots  ----
## ------------------------------------------ ##

pdf(here("figures", "tdr_profiles_trimmed_9c.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_trim$cruise))) {
  df <- tdr_trim %>% filter(cruise == cr)
  if (nrow(df) == 0) next
  
  p <- df %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
    geom_point(size = 0.8, alpha = 0.6) +
    scale_y_reverse() +
    scale_color_manual(
      values = c(predeploy = "grey70",
                 downcast  = "steelblue",
                 upcast    = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Post-trim labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x    = element_blank(),
          strip.text     = element_text(size = 6),
          legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()
rm(df, p, cr)

## ------------------------------------------ ##
##   10. Add notes column           ----
## ------------------------------------------ ##

cast_notes <- tribble(
  ~cruise,  ~station, ~cast, ~note_code,        ~note_detail,
  "EN655",  "L9",     "B15", "hit_bottom",      "net hit bottom; no zooplankton sample; TDR cast only",
  "EN712",  "L6",     "B5",  "hit_bottom",      "net hit bottom; no zooplankton sample; TDR cast only",
  "AR99",   "L10",    "B6",  "tdr_late_start",  "TDR turned on after net in water; data starts at approx 37m on downcast",
  "AR95",   "L3",     "B19", "tdr_late_start",  "TDR turned on after net in water; data starts at approx 19m on downcast",
  "AE2426", "L9",     "B12", "upcast_only",     "upcast only; data starts at approx 85m on upcast",
  "EN657",  "L1",     "B1",  "tdr_late_start",  "TDR turned on after net in water; data starts at approx 4m on downcast",
  "EN655",  "L3",     "B3",  "tdr_late_start",  "TDR turned on after net in water; data starts at approx 10m on downcast",
  "EN657",  "L4",     "B6",  "tdr_late_start",  "TDR turned on after net in water; data starts at approx 6m on downcast",
  "EN627",  "L2",     "B7",  "tdr_early_end",   "TDR turned off before bongo out; data stopped at approx 4m on upcast"
)

# join notes into tdr_trim
tdr_notes <- tdr_trim %>%
  left_join(cast_notes, by = c("cruise", "station", "cast"))

rm(cast_notes, tdr_trim)

## ------------------------------------------ ##
##   11. Add recording interval column  ----
## ------------------------------------------ ##
# different than PxSensor data and the 2 CTD-Bongo casts
# TDR recording interval is nominally 1 second but varies across casts;
# tdr_sampling_interval_sec documents the actual per-cast interval
cast_intervals <- tdr_notes %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  mutate(interval_sec = as.numeric(difftime(date_time, dplyr::lag(date_time), units = "secs"))) %>%
  summarise(
    tdr_sampling_interval_sec = round(median(interval_sec, na.rm = TRUE)),
    tdr_max_gap_sec           = round(max(interval_sec, na.rm = TRUE)),
    tdr_n_obs                 = n(),
    .groups = "drop"
  )

tdr_data <- tdr_notes %>%
  left_join(cast_intervals, by = c("cruise", "station", "cast"))

# plot
pdf(here("figures", "tdr_profiles_final_11.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_data$cruise))) {
  df <- tdr_data %>% filter(cruise == cr)
  if (nrow(df) == 0) next
  
  p <- df %>%
    mutate(label = paste0(station, " ", cast, 
                          "\n[", tdr_sampling_interval_sec, "s | ",
                          tdr_n_obs, " obs]")) %>%
    ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
    geom_point(size = 0.8, alpha = 0.6) +
    scale_y_reverse() +
    scale_color_manual(
      values = c(predeploy = "grey70",
                 downcast  = "steelblue",
                 upcast    = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Post-trim labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x    = element_blank(),
          strip.text     = element_text(size = 6),
          legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()
rm(tdr_notes, cast_intervals, df, p, cr)

## ------------------------------------------ ##
##  12. QC / Validation checks           ----
## ------------------------------------------ ##

## ------------------------------------------ ##
##  12a. Naming consistency checks       ----
## ------------------------------------------ ##
## All these should not print anything (tibble 0 x 1)

# cruise: should all be uppercase alphanumeric
tdr_data %>%
  filter(!grepl("^[A-Z]{2,3}[0-9]+[A-Z]?$", cruise)) %>%
  distinct(cruise)

# station: should be L + integer, MVCO, or known opportunistic
valid_stations <- c("MVCO", "u11c", "u9a")
tdr_data %>%
  filter(!grepl("^L[0-9]+$", station),
         !station %in% valid_stations) %>%
  distinct(cruise, station)

# cast: should be B + integer or R + integer
tdr_data %>%
  filter(!grepl("^[BR][0-9]+$", cast)) %>%
  distinct(cruise, station, cast)

# down_up: should only be predeploy, downcast, upcast
tdr_data %>%
  filter(!down_up %in% c("predeploy", "downcast", "upcast")) %>%
  distinct(down_up)

rm(valid_stations)

## ------------------------------------------ ##
##  12b. Physical range checks (row-level) ----
## ------------------------------------------ ##

row_flags <- tdr_data %>%
  mutate(
    flag_temp_range  = temp_C < -2 | temp_C > 30,
    flag_depth_range = depth_m < 0 | depth_m > 300,
    flag_temp_na     = is.na(temp_C),
    flag_depth_na    = is.na(depth_m),
    flag_time_na     = is.na(date_time)
  )

# summary of flags
row_flags %>%
  summarise(across(starts_with("flag_"), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "flag", values_to = "n_flagged") %>%
  filter(n_flagged > 0) %>%
  print()

# inspect flagged rows
row_flags %>%
  filter(flag_temp_range) %>%
  select(cruise, station, cast, date_time, temp_C, depth_m) %>%
  arrange(temp_C)

rm(row_flags)

## ------------------------------------------ ##
##  12c. Temporal checks (cast-level)     ----
## ------------------------------------------ ##

cast_qc <- tdr_data %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_obs              = n(),
    t_start            = min(date_time),
    t_end              = max(date_time),
    duration_min       = as.numeric(difftime(max(date_time), min(date_time), units = "mins")),
    max_depth_m        = max(depth_m, na.rm = TRUE),
    temp_min_C         = min(temp_C,  na.rm = TRUE),
    temp_max_C         = max(temp_C,  na.rm = TRUE),
    temp_range_C       = temp_max_C - temp_min_C,
    n_temp_na          = sum(is.na(temp_C)),
    n_time_reversal    = sum(diff(as.numeric(date_time)) < 0, na.rm = TRUE),
    max_gap_sec        = tdr_max_gap_sec[1],
    sampling_interval  = tdr_sampling_interval_sec[1],
    .groups = "drop"
  ) %>%
  mutate(
    # cast-level flags
    flag_too_short      = duration_min < 3,           # < 3 min is suspicious; possible at MVCO
    flag_too_long       = duration_min > 120,         # > 2 hrs is suspicious
    flag_shallow        = max_depth_m < 15,           # barely went down
    flag_temp_suspect   = temp_range_C > 15,          # >15 deg range in one cast
    flag_time_reversal  = n_time_reversal > 0,        # timestamps go backwards
    flag_large_gap      = max_gap_sec > 60,           # >1 min gap
    flag_few_obs        = n_obs < 20,                 # very sparse
    flag_nonstandard_interval = sampling_interval > 2 # not 1 or 2 sec
  )

# print cast-level flag summary
cast_qc %>%
  summarise(across(starts_with("flag_"), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "flag", values_to = "n_casts_flagged") %>%
  filter(n_casts_flagged > 0) %>%
  arrange(desc(n_casts_flagged)) %>%
  print()

# inspect flagged casts
cast_qc %>%
  filter(if_any(starts_with("flag_"), ~.)) %>%
  select(cruise, station, cast, duration_min, max_depth_m,
         temp_range_C, n_time_reversal, max_gap_sec, n_obs,
         starts_with("flag_")) %>%
  arrange(cruise, station, cast) %>%
  print(n = Inf, width = Inf)

## --- plots 
tdr_data %>%
  filter(down_up == "downcast") %>%
  mutate(month = lubridate::month(date_time, label = TRUE)) %>%
  ggplot(aes(x = temp_C, y = depth_m, color = cruise)) +
  geom_point(size = 0.2, alpha = 0.3) +
  scale_y_reverse() +
  facet_wrap(~month) +
  labs(title = "Temperature-depth profiles by month (downcast only)",
       x = "Temperature (°C)", y = "Depth (m)") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3)))

tdr_data %>%
  filter(down_up == "downcast", depth_m < 5) %>%
  mutate(month = lubridate::month(date_time)) %>%
  ggplot(aes(x = factor(month), y = temp_C, fill = factor(month))) +
  geom_violin(alpha = 0.6, quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.4, color = "grey30") +
  scale_x_discrete(labels = month.abb) +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Near-surface temperature by month (depth < 5m, downcast)",
       x = NULL, y = "Temp (°C)") +
  theme_minimal()

tdr_data %>%
  filter(down_up == "downcast", station != "u11c") %>%
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
  labs(title = "Near-bottom temperature by month (within 5m of max depth, downcast)",
       x = NULL, y = "Temp (°C)") +
  theme_minimal()

temp_jumps <- tdr_data %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  mutate(temp_diff = abs(temp_C - dplyr::lag(temp_C))) %>%
  filter(!is.na(temp_diff), temp_diff > 5) %>%  # >5C between consecutive obs
  select(cruise, station, cast, date_time, temp_C, temp_diff, depth_m) %>%
  ungroup()

message("Large consecutive temp jumps: ", nrow(temp_jumps))
print(temp_jumps, n = 30)

tdr_data %>%
  filter(down_up == "downcast") %>%
  mutate(
    month  = lubridate::month(date_time),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      month %in% c(9, 10, 11)~ "Fall"
    )
  ) %>%
  ggplot(aes(x = reorder(cruise, date_time), y = temp_C, fill = season)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  scale_fill_manual(values = c(Winter = "steelblue", Spring = "mediumseagreen",
                               Summer = "tomato",    Fall   = "goldenrod")) +
  labs(title = "Temperature distribution by cruise (downcast)",
       x = NULL, y = "Temp (°C)", fill = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tdr_data %>%
  filter(down_up == "downcast", depth_m < 5,
         station != "u11c") %>%
  mutate(month = lubridate::month(date_time, label = TRUE)) %>%
  ggplot(aes(x = month, y = temp_C)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~station) +
  labs(title = "Near-surface temperature by month",
       x = "Month", y = "Temp (°C)") +
  theme_minimal()

tdr_data %>%
  filter(down_up == "downcast", station != "u11c") %>%
  group_by(cruise, station, cast) %>%
  mutate(max_depth = max(depth_m, na.rm = TRUE)) %>%
  filter(depth_m >= max_depth - 5) %>%
  ungroup() %>%
  mutate(month = lubridate::month(date_time, label = TRUE)) %>%
  ggplot(aes(x = month, y = temp_C)) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = cruise), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~station) +
  labs(title = "Near-bottom temperature by month (within 5m of max depth, downcast)",
       x = "Month", y = "Temp (°C)", color = "Cruise") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3)))

tdr_data %>%
  filter(down_up == "downcast", depth_m < 5,
         station != "u11c") %>%
  mutate(month = lubridate::month(date_time, label = TRUE),
         year  = factor(lubridate::year(date_time))) %>%
  ggplot(aes(x = month, y = temp_C)) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = year), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~station) +
  labs(title = "Near-surface temperature by month",
       x = "Month", y = "Temp (°C)", color = "Year") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3)))

tdr_data %>%
  filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  summarise(tdr_max_depth = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  left_join(meta %>% mutate(cast = paste0("B", cast)),
            by = c("cruise", "station", "cast")) %>%
  ggplot(aes(x = depth_target, y = tdr_max_depth)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick", linetype = "dashed") +
  labs(title = "TDR max depth vs logsheet target depth",
       x = "Target depth (m)", y = "TDR max depth (m)") +
  theme_minimal()

tdr_data %>%
  filter(station != "u11c") %>%
  group_by(cruise, station, cast) %>%
  summarise(duration_min = as.numeric(difftime(max(date_time),
                                               min(date_time), units = "mins")),
            .groups = "drop") %>%
  mutate(station = factor(station, levels = c(paste0("L", 1:11), "MVCO"))) %>%
  ggplot(aes(x = duration_min, fill = station)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_fill_viridis_d(name = "Station") +
  labs(title = "Cast duration distribution by station",
       x = "Duration (minutes)", y = "Count") +
  theme_minimal()

tdr_data %>%
  distinct(cruise, station, cast, tdr_n_obs, tdr_sampling_interval_sec) %>%
  ggplot(aes(x = reorder(cruise, tdr_n_obs), y = tdr_n_obs,
             color = factor(tdr_sampling_interval_sec))) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2.9) +
  labs(title = "Observations per cast by cruise",
       x = NULL, y = "n observations", color = "Interval (sec)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ------------------------------------------ ##
##  12d. Duplicate timestamp check       ----
## ------------------------------------------ ##

dup_times <- tdr_data %>%
  group_by(cruise, station, cast, date_time) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(cruise, station, cast, date_time)

message("Duplicate timestamps: ", nrow(dup_times))
if (nrow(dup_times) > 0) {
  dup_times %>%
    count(cruise, station, cast, name = "n_dups") %>%
    arrange(desc(n_dups)) %>%
    print(n = 20)
}
# these are fine its times when there are sub-second obs == duplicate timestamps
# at 1-sec resolution 
rm(dup_times, temp_jumps)

## ------------------------------------------ ##
##  12e. Downcast/upcast balance check   ----
## ------------------------------------------ ##
# most casts should have both a downcast and upcast

known_upcast_only <- c("AE2426_L9_B12") 

cast_coverage <- tdr_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    has_downcast = any(down_up == "downcast"),
    has_upcast   = any(down_up == "upcast"),
    .groups = "drop"
  ) %>%
  mutate(cast_id = paste(cruise, station, cast, sep = "_")) %>%
  filter(!cast_id %in% known_upcast_only) %>%
  filter(!has_downcast | !has_upcast)

if (nrow(cast_coverage) > 0) {
  message("Casts missing downcast or upcast label:")
  print(cast_coverage)
} else {
  message("All casts have both downcast and upcast labels.")
}

rm(known_upcast_only, cast_coverage)

## ------------------------------------------ ##
##  13. Save outputs                        ----
## ------------------------------------------ ##
# export unbinned
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

saveRDS(tdr_data, here("data", "processed", 
                       paste0("tdr_data_no_offset_", Sys.Date(), ".rds")))

write_csv(tdr_data, here(OUT_DIR, "tdr_data_no_offset.csv"))

# save file with colnames
tibble(column = names(tdr_data)) %>%
  write_csv(here("data", "processed", "tdr-column-headers.csv"))

# per-cruise CSVs
# message("Saving per-cruise CSVs ...")
# walk(unique(tdr_binned$cruise), function(cr) {
#   out_path <- here(OUT_DIR, paste0(cr, "_tdr_processed.csv"))
#   filter(tdr_binned, cruise == cr) %>% write_csv(out_path)
#   message("  Saved: ", basename(out_path))
# })

################################################################################
# go to -----------> 03_tdr_offsets.R
#           OR     > 02_px_sensor_tidy.R
#           OR     > 02_ctd_bongo_tidy.R 
################################################################################