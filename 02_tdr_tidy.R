###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_tdr_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Read per cast CSV files from all cruise TDR folders,
##           tidy data, detect mutiple casts,
##           label downcast/upcast, bin to 1-m depth intervals,
##           and save per-cruise + one combined CSV.
## Get CSVs for DAT-only cruises (EN608, EN627, EN644) run 01_tdr_dat_to_csv.R
## 
##  Input:   data/raw/tdr_data/<CRUISE>_TDR/*.csv   (one CSV per cast)
##           data/raw/elog_zoop_tows_thruAR99_2026-04-14.csv (FROM WHERE)
##
##  Output:  data/processed/<CRUISE>_tdr_processed.csv  (per cruise)
##           data/processed/allTDRdata.csv               (combined)
##           data/processed/tdr_cast_qc_summary.csv      (QC table)
##           data/processed/tdr_ctd_tests.csv            
###############################################################

#####################################EN706 most likely bad -- CHECK
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
## EN661 (winter 2021) logsheets say tdr recoded but didnt find data
## EN668 (summer 2021) CTD was used; no tdr data found
## EN695 (winter 2023) tdr was used; no tdr data found 
## ------------------------------------------ ##

# .DAT files only, csv created in 01_tdr_dat_to_csv.R: EN644, EN627, EN608
# xlsx files converted to csv in 01_tdr_dat_to_csv.R

# no TDR data for the following cruises: EN661, AR63*, AR38, AR32, EN715, EN695, EN668 (CTD only)
# CTD data available for: EN668 (no TDR) and EN706
# PxSensor data available starting AE2426

## ------------------------------------------ ##
##  Packages               ----
## ------------------------------------------ ##
library(tidyverse)
library(here)
library(readr)
library(lubridate)

## ------------------------------------------ ##
##  Constants              ----
## ------------------------------------------ ##

RAW_DIR    <- here("data", "raw", "tdr_data")
OUT_DIR    <- here("data", "processed")

## ------------------------------------------ ##
##  Helpers                                ----
## ------------------------------------------ ##

## --- Parse cruise / station / cast from a TDR CSV filename ---
# handles 2 naming convensions:
#   AE2426:  <serial>_<CRUISE>_<station>_<cast>.csv
#   EN617:   <serial>-<CRUISE>-<station>-<cast>.csv
#' @param file_name  Bare filename
#' @return  Named list: cruise, station, cast
#          Returns NAs with a warning when pattern cannot be matched.
parse_filename_meta <- function(file_name) {
  stem  <- tools::file_path_sans_ext(file_name)
  parts <- unlist(strsplit(stem, "[-_]"))
  
  if (length(parts) < 4) {
    warning("Cannot parse metadata from filename: ", file_name)
    return(list(cruise  = NA_character_,
                station = NA_character_,
                cast    = NA_character_))
  }
  
  list(cruise  = parts[2],
       station = parts[3],
       cast    = parts[4])
}

## --- Strip leading zeros after a single-letter prefix ---
# L03 -> L3, B07 -> B7, L11 unchanged
strip_leading_zeros <- function(x, prefix) {
  sub(paste0("^", prefix, "0*"), prefix, x)
}

## --- Coalesce the two temperature column name variants ---
# Some files export "Temp°C" UTF-8, others "Temp¡C" due to encoding
#  Merge into one clean column
coalesce_temp_col <- function(df) {
  col_utf <- "Temp(\u00b0C)"   # degrees C  (UTF-8)
  col_bad <- "Temp(\u00a1C)"   # inverted ! (bad encoded)
  
  has_utf <- col_utf %in% colnames(df)
  has_bad <- col_bad %in% colnames(df)
  
  if (has_utf && has_bad) {
    df[[col_utf]] <- coalesce(df[[col_utf]], df[[col_bad]])
    df <- select(df, -all_of(col_bad))
  } else if (has_bad && !has_utf) {
    df <- rename(df, !!col_utf := !!col_bad)
  }
  df
}

## --- Read one TDR CSV and standardize its columns ---
#' @param file_path  Full path to the CSV
#' @return  Tidy data frame with cols:
#          date_time, temp_C, depth_m, cruise, station, cast
#          Returns NULL on read error
read_tdr_csv <- function(file_path) {
  file_name <- basename(file_path)
  meta      <- parse_filename_meta(file_name)
  
  enc <- tryCatch(
    guess_encoding(file_path)$encoding[1],
    error = function(e) "UTF-8"
  )
  
  df <- tryCatch(
    read_csv(file_path,
             locale         = locale(encoding = enc),
             show_col_types = FALSE),
    error = function(e) {
      message("  ! Error reading: ", file_name, " — ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df <- coalesce_temp_col(df)
  
  rename_map <- list(
    date_time = c("Date & Time", "DateTime", "Date.&.Time", "Date.&amp;.Time"),
    temp_C    = c("Temp(\u00b0C)", "Temperature"),
    depth_m   = c("Depth(m)", "Depth")
  )
  for (new_nm in names(rename_map)) {
    matched <- rename_map[[new_nm]][rename_map[[new_nm]] %in% colnames(df)]
    if (length(matched) > 0 && !new_nm %in% colnames(df))
      df <- rename(df, !!new_nm := !!matched[1])
  }
  
  if (!"date_time" %in% colnames(df)) {
    warning("No date_time col in: ", file_name,
            "\n  cols: ", paste(colnames(df), collapse = ", "))
    return(NULL)
  }
  
  df %>%
    mutate(
      cruise    = toupper(meta$cruise),
      station   = meta$station,
      cast      = meta$cast,
      date_time = as.character(date_time)
    ) %>%
    select(any_of(c("date_time", "temp_C", "depth_m",
                    "cruise", "station", "cast")))
}

## --- Find first local depth peak (robust to second tow being deeper) ---
find_first_peak <- function(depths, min_depth = 15) {
  n        <- length(depths)
  smoothed <- zoo::rollmean(depths, k = 11, fill = "extend")
  above_min <- which(smoothed > min_depth)
  if (length(above_min) == 0) return(which.max(depths))
  for (i in above_min) {
    lookahead <- min(i + 10, n)
    if (smoothed[lookahead] < smoothed[i]) return(i)
  }
  return(which.max(depths))
}

## --- Auto-split a data frame containing multiple tows ---
# Detects two tows by finding a shallow valley between two deep excursions.
# valley must be < valley_ratio * first peak depth, and second peak > min_peak_depth
#' @param df            Data frame for one (cruise, station, cast)
#' @param base_cast     Original cast label e.g. "B25"
#' @param min_peak_depth Minimum depth (m) to consider a real tow
#' @param valley_ratio  Valley must be shallower than this fraction of first peak
#' @return df with cast relabelled to B25_1, B25_2 if split triggered,
#'         otherwise cast unchanged
auto_split_casts <- function(df, base_cast, min_peak_depth = 15, valley_ratio = 0.25) {
  df <- arrange(df, date_time)
  n  <- nrow(df)
  
  first_peak_idx   <- find_first_peak(df$depth_m, min_peak_depth)
  first_peak_depth <- df$depth_m[first_peak_idx]
  
  if (first_peak_depth < min_peak_depth) {
    df$cast <- base_cast
    return(df)
  }
  
  after_peak <- (first_peak_idx + 1):n
  if (length(after_peak) < 10) {
    df$cast <- base_cast
    return(df)
  }
  
  valley_idx   <- first_peak_idx + which.min(df$depth_m[after_peak])
  valley_depth <- df$depth_m[valley_idx]
  
  after_valley <- (valley_idx + 1):n
  if (length(after_valley) < 10) {
    df$cast <- base_cast
    return(df)
  }
  
  second_peak_depth <- max(df$depth_m[after_valley], na.rm = TRUE)
  
  valley_is_shallow <- valley_depth < (first_peak_depth * valley_ratio)
  second_is_real    <- second_peak_depth > min_peak_depth
  
  if (valley_is_shallow & second_is_real) {
    df$cast[1:valley_idx]       <- paste0(base_cast, "_1")
    df$cast[(valley_idx + 1):n] <- paste0(base_cast, "_2")
  } else {
    df$cast <- base_cast
  }
  
  df
}

## --- Detect and split a multi cast group ---
# Wrapper around auto_split_casts() that prints a VERIFY message when a split occurs
#' @param df        Data frame for one (cruise, station, cast) group
#' @param base_cast Original cast label
#' @return df with cast relabelled if a split was triggered
detect_and_split <- function(df, base_cast) {
  df <- auto_split_casts(df, base_cast)
  
  if (any(grepl("_\\d+$", df$cast))) {
    second_peak <- max(df$depth_m[grepl("_2$", df$cast)], na.rm = TRUE)
    message(sprintf(
      "  AUTO-SPLIT: cruise=%-8s station=%-6s cast=%-8s (2nd excursion: %.0f m) — VERIFY",
      df$cruise[1], df$station[1], base_cast, second_peak))
  }
  
  df
}

## --- Label downcast / upcast rows within a single tow ---
# Rows up to and including the depth maximum = "downcast";
# all rows after = "upcast"
#' @param df  Data frame for one cast, sorted by date_time
#' @return  df with new column `down_up`
label_down_up <- function(df) {
  df       <- arrange(df, date_time)
  peak_idx <- which.max(df$depth_m)
  
  # find where the cast actually starts descending
  # first row where depth exceeds 1 m heading toward the peak
  descent_start <- which(df$depth_m > 1)[1]
  if (is.na(descent_start)) descent_start <- 1L
  
  df$down_up <- case_when(
    seq_len(nrow(df)) < descent_start   ~ "predeploy",
    seq_len(nrow(df)) <= peak_idx       ~ "downcast",
    TRUE                                ~ "upcast"
  )
  df
}

## --- Bin to 1-m depth intervals and average temperature ---
#' @param df  Data frame with depth_m, temp_C, down_up, date_time,
#'            and grouping cols cruise / station / cast
#' @return  One row per cruise x station x cast x down_up x depth_bin
# bin_by_depth <- function(df) {
#   df %>%
#     mutate(depth_bin = floor(depth_m)) %>%
#     group_by(cruise, station, cast, down_up, depth_bin) %>%
#     summarise(
#       avg_temp_C = mean(temp_C,     na.rm = TRUE),
#       date_time  = median(date_time, na.rm = TRUE),
#       n_obs      = n(),
#       .groups    = "drop"
#     )
# }
bin_by_depth <- function(df) {
  df %>%
    filter(down_up != "predeploy") %>%
    # for upcast: only keep rows that are still meaningfully deep
    # drop the trailing surface tail (upcast rows within 2 m of surface)
    filter(!(down_up == "upcast" & depth_m < 2)) %>%
    mutate(depth_bin = floor(depth_m)) %>%
    group_by(cruise, station, cast, down_up, depth_bin) %>%
    summarise(
      avg_temp_C = mean(temp_C,      na.rm = TRUE),
      date_time  = median(date_time, na.rm = TRUE),
      n_obs      = n(),
      .groups    = "drop"
    ) %>%
    arrange(cruise, station, cast, date_time)
}

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
    # as strings like "45603.2" — detect with regex before parsing
    date_time = case_when(
      grepl("^\\d{5}\\.?\\d*$", date_time) ~
        as.POSIXct((as.numeric(date_time) - 25569) * 86400,
                   origin = "1970-01-01", tz = "UTC"),
      TRUE ~
        parse_date_time(as.character(date_time),
                        orders = c("dmY HMS", "Ymd HMS", "mdY IMS p",
                                   "Ymd HM", "Ymd"),
                        tz = "UTC")
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
  ) %>%
  filter(!is.na(depth_m),    depth_m >= 0,
         !is.na(date_time))
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

# --- depth profile plots per cruise ----
pdf(here("figures", "raw_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.2, color = "steelblue", alpha = 0.7) +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("RAW profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          strip.text   = element_text(size = 6))
  print(p)
}

dev.off()

all_data %>%
  distinct(cruise, station, cast) %>%
  arrange(cruise, station, cast) %>%
  print(n=400)

## ---------------------------------------------------- ##
## multiple casts - need to manually inspect:
# EN617 = L11B25ab = flowmeter calibration
# EN627 = L8B19 = re-did deployment; so delete first aborted cast
# EN657 = L3B2 (this contains L1 file); L6B17; L9B8 (L9B14,L8B15)
# AT46  = L5B3 = maybe? not two casts but has mini spike after main cast
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

# 2019 = EN627 = two casts L8B19 = first one hit bottom and redid cast
# 2019 = EN644 = good; complete data

# 2020 = EN649 = good; complete data
# 2020 = EN655 = good; complete data
#              = L9B15 has tdr cast but no sample hit bottom no time to re-do
# 2020 = EN657 = 3 with multiple casts L3B2 (this contains L1 cast); L6B17; L9B8 (L9B14,L8B15)

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
# 2024 = AE2426 = L9B12 upcast only 
#               = L8B13 funky stuff before start of actual cast\
#               = L11B9 = typo; should be L11B10

# 2025 = EN727 = good; complete data
# 2025 = AR88  = good; complete data
# 2025 = AR92  = good; complete data
# 2025 = AR95  = L3B19 TDR turned on after net in water

# 2026 = AR99  = L2B3 = the second cast is a ring net only at same L2R3
#              = L9B5 = the second cast is a ring net only at same L9R5
#              = L10B6 = TDR turned on after net in water
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
    (cruise == "AT46"   & station == "L6"  & cast == "B6")  |
      (cruise == "AE2426" & station == "L11" & cast == "B10") |
      (cruise == "EN657"  & station == "L9"  & cast == "B14") |
      (cruise == "EN706"  & station == "L7"  & cast == "B14")
  ) %>%
  distinct(cruise, station, cast)

## ------------------------------------------ ##
##  4. Isolate TDR-CTD bench tests   ----
## ------------------------------------------ ##
#### NEED TO FIND WHICH HAVE THESE AND FIND CTD MAX DEPTH FOR EACH OF THESE TOWS
#### NEED TO ADD CAST AND STATION TO SOME OF THESE
#### DOING THIS WILL GIVE OFFSETS FOR ANY OF THESE

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
           station == "u9a"  ~ "B18",    # AR92: keep as-is
           grepl("B\\d+test", cast) ~ str_extract(cast, "B\\d+(?=test)"),  # EN715, EN720
           TRUE ~ NA_character_          # AT46 BL2test, EN712 Btest = no cast
         ))

if (nrow(tdr_test) > 0) {
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
  write_csv(tdr_test, here(OUT_DIR, "tdr_ctd_tests.csv"))
  message("  Saved ", nrow(tdr_test),
          " TDR-CTD test rows -> tdr_ctd_tests.csv")
}

# EN617 L11 B25a and B25ab == flowmeter calibration can filter out these
# remove tests + flowmeter cals from main data
all_data <- filter(all_data,
                   !grepl("^TDRCTD", station, ignore.case = TRUE),
                   station != "u9a",
                   !(cruise == "EN617" & station == "L11" & 
                       cast %in% c("B25a", "B25ab")))

## ------------------------------------------ ##
##  5. Fix/Validate TDR timestamps          ----
## ------------------------------------------ ##

# elog data fixed and created in nes-lter-api-pulls.Rproj
# 01_elog_pull
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

## --- or some cruises TDR computer was local time so need to adjust to UTC
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

timestamp_check %>%
  filter(flag_no_elog, !grepl("_\\d+$", cast)) %>%
  select(cruise, station, cast) %>%
  arrange(cruise, station, cast)
# EN655 = L9B15 hit bottom = no sample = tdr cast but no sample
# EN712  = L6B5 hit bottom = no sample = tdr cast but no sample

# =============================================================================
# TIMESTAMP CORRECTION NOTES
# Based on timestamp_check output - TDR local time vs UTC offset review
# offset_deploy_min = tdr_start - elog_deploy (negative = TDR clock behind elog/UTC)
# =============================================================================

# --- CRUISE-LEVEL TIMEZONE CORRECTIONS NEEDED ---
# These cruises show consistent ~same offset across all casts (both deploy AND
# recover negative by similar magnitude) TDR computer was in local time (EDT = UTC-4
# or EST = UTC-5). Fix by adding hours to tdr datetime column for these cruises.

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
    # one-off cast correction ## EN617 L1B1 needs 3 hr adjustment
    cruise == "EN617" & station == "L1" & cast == "B1" ~ date_time + hours(3),
    TRUE ~ date_time
  ))

# MANUALLY CHECKED TIMESTAMP against all-nes-lter-bongologs EN608, EN617,
# EN627, EN644, EN649, EN655, EN657, AT46, EN687, HRS2303, EN706, AR77,
# EN712, EN715, EN720, AE2426, EN727, AR88, AR92, AR95, AR99 

## ------------------------------------------ ##
##  6. Split merged multi-cast files       ----
## ------------------------------------------ ##
# detect_and_split() checks whether a single file contains two or more
# back-to-back tows (depth goes deep, returns to surface, then goes deep = 2 stations)

all_data <- all_data %>%
  group_by(cruise, station, cast) %>%
  do({ detect_and_split(., base_cast = .$cast[1]) }) %>%
  ungroup()

# see which got split
all_data %>%
  distinct(cruise, station, cast) %>%
  filter(grepl("_\\d+$", cast)) %>%
  arrange(cruise, station, cast)

n_split <- all_data %>%
  distinct(cruise, station, cast) %>%
  filter(grepl("_\\d+$", cast)) %>%
  nrow()

if (n_split > 0) {
  message("  ", n_split, " cast(s) were auto-split. ",
          "Review VERIFY messages above")
} else {
  message("  No merged multi-cast files detected.")
}

## ------------------------------------------ ##
##  7. Manual corrections  ----
## ------------------------------------------ ##

## ------------------------------------------ ##
##  7a. Manual resolution of auto-split casts ---
## ------------------------------------------ ##
## --- auto split in step 6 ---
# -- need to manually identify whether second cast is aborted or a different staiton
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
  mutate(base_cast = sub("_\\d+$", "", cast))   # e.g. "B13_1" -> "B13"

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
#        L6 B10_1                  -> bongo tow           -> rename to B10 
#        L6 B10_2                  -> ring tow            -> rename to R10
#        L9 B5_1                   -> bongo tow           -> rename to B5 
#        L9 B5_2                   -> ring tow            -> rename to R5
#
# EN657  =
#        L6 B17_1 = 69 min, 95 m   -> UNCLEAR             -> DROP for now
# no elog for ^this. no comment on bongo logsheets
#        L6 B17_2 = 64 min, 74 m   -> real tow            -> rename to B17
#
# EN715  = re-deployed; delete first aborted cast
#        L6 B13_1 = 15 min, 90 m   -> tow 1               -> DROP               
#        L6 B13_2 = 9 min,  82 m   -> tow 2               -> rename to B13
#
# EN715  = re-deployed; delete first aborted cast
#        L8 B14_1 = 28 min, 136 m  -> tow 1               -> DROP
#        L8 B14_2 = 13 min, 137 m  -> tow 2               -> rename to B14
#

## --- Manual resolution of auto-split casts ---
all_data <- all_data %>%
  mutate(cast = case_when(
    # AE2426 L8: B13_1 aborted test; B13_2 real cast
    #             still has multiple up/downs; real cast ~19:10-19:40)
    cruise == "AE2426" & station == "L8" & cast == "B13_2" ~ "B13",
    
    # AR77 L2: B2_1 aborted re-deploy; B2_2 real cast
    cruise == "AR77"   & station == "L2" & cast == "B2_2"  ~ "B2",
    
    # AR99 L6: B10_1 bongo, B10_2 ring net
    cruise == "AR99"   & station == "L6" & cast == "B10_1" ~ "B10",
    cruise == "AR99"   & station == "L6" & cast == "B10_2" ~ "R10",
    
    # AR99 L9: B5_1 bongo, B5_2 ring net
    cruise == "AR99"   & station == "L9" & cast == "B5_1"  ~ "B5",
    cruise == "AR99"   & station == "L9" & cast == "B5_2"  ~ "R5",
    
    # EN657 L6: B17_1 unclear/no elog; B17_2 real cast
    cruise == "EN657"  & station == "L6" & cast == "B17_2" ~ "B17",
    
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

## ------------------------------------------ ##
##  7b. Manual corrections: missed auto-splits ----
## ------------------------------------------ ##
## these were NOT caught by detect_and_split() in step 6 because
## the valley was not shallow enough, or timing was ambiguous.
## Identified via raw_profiles_check.pdf + elog/logsheet cross-check
## For files with 2 real casts, define the time boundary
## manually split & assign correct cast
##
## Cases:
# EN627 = L8B19 = re-did deployment; so delete first aborted cast
# EN657 = L3B2 (this contains L1 file); L9B8 (L9B14,L8B15)
# EN706 = L5B6 = re-did deployment; so delete first aborted cast
# EN715 = L5B6 = re-did deployment; so delete first aborted cast
# AE2426 = L8B13 funky stuff before start of actual cast
# AR99 = L2B3  = the second cast is a ring net only at same L2R3

## --- Interactive plot to confirm time boundaries ---
library(plotly)

missed_splits <- all_data %>%
  filter(
    (cruise == "EN627" & station == "L8" & cast == "B19") |
    (cruise == "EN657" & station == "L9" & cast == "B8")  |
    (cruise == "EN657" & station == "L3" & cast == "B2")  |
    (cruise == "EN706" & station == "L5" & cast == "B6")  |
    (cruise == "EN715" & station == "L5" & cast == "B6")  |
    (cruise == "AE2426"& station == "L8" & cast == "B13")  |
    (cruise == "AR99"  & station == "L2" & cast == "B3")
  ) %>%
  mutate(label = paste(cruise, station, cast))

p <- ggplot(missed_splits, aes(x = date_time, y = depth_m,
                               text = paste("time:", date_time,
                                            "<br>depth:", round(depth_m, 1)))) +
  geom_point(size = 0.1, color = "steelblue", alpha = 0.5) +
  scale_y_reverse() +
  facet_wrap(~label, scales = "free_x") +
  labs(x = NULL, y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplotly(p, tooltip = "text")

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
      (cruise == "AR99"   & station == "L2"  & cast %in% c("B3","R3"))
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

conflicted::conflict_prefer("filter", "dplyr")
for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.2, color = "steelblue", alpha = 0.7) +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("RAW profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          strip.text   = element_text(size = 6))
  print(p)
}

## ------------------------------------------ ##
##  8. Label downcast / upcast        ----
## ------------------------------------------ ##
# Within each cast:
#   - find the global depth maximum
#   - detect where sustained descent begins (depth increases > 0.5 m
#     over the next 10 observations) — this trims pre-deployment hang time
#   - rows before descent_start  -> "predeploy"
#   - rows to depth maximum      -> "downcast"
#   - rows after depth maximum   -> "upcast"

label_down_up <- function(df) {
  df       <- arrange(df, date_time)
  n        <- nrow(df)
  peak_idx <- which.max(df$depth_m)
  
  # detect sustained descent: first row where depth increases > 0.5 m
  # within the next 10 observations heading toward the peak
  descent_start <- NA_integer_
  for (i in seq_len(peak_idx - 1)) {
    lookahead <- min(i + 10, peak_idx)
    if (max(df$depth_m[i:lookahead]) - df$depth_m[i] > 0.5) {
      descent_start <- i
      break
    }
  }
  if (is.na(descent_start)) descent_start <- 1L
  
  df$down_up <- case_when(
    seq_len(n) < descent_start ~ "predeploy",
    seq_len(n) <= peak_idx     ~ "downcast",
    TRUE                       ~ "upcast"
  )
  df
}

all_data <- all_data %>%
  group_by(cruise, station, cast) %>%
  do(label_down_up(.)) %>%
  ungroup()

all_data %>%
  count(down_up) %>%
  print()

## ------------------------------------------ ##
##  8a. Post-label profile plots           ----
## ------------------------------------------ ##
# profiles (predeploy / downcast / upcast) to PDF.

pdf(here("figures", "labeled_profiles_check2.pdf"),
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
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("Labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text  = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

## ------------------------------------------ ##
##  8b. Manual down_up corrections         ----
## ------------------------------------------ ##
# EN617 L1B1 = flat line? does the depth every change? maybe bad cast?

# quick interactive look at the flagged ones
all_data %>%
  filter(
    (cruise == "EN617" & station == "L1"   & cast == "B1")
  ) %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_m), .groups = "drop") %>%
  arrange(cruise, station, cast) %>%
  print()

# DELETE THIS CAST
# delete bad cast
all_data <- all_data %>%
  filter(!(cruise == "EN617" & station == "L1" & cast == "B1"))

## ------------------------------------------ ##
##  9. Trim casts to elog deploy/recover times ----
## ------------------------------------------ ##
# uses elog_wide from step 5 (already UTC-validated)
# casts with no elog match pass through untrimmed
# ring net casts (R*) won't have elog entries -> pass through untrimmed

BUFFER_SECS <- 180  # 3 min buffer on each side

tdr_trim <- all_data %>%
  left_join(
    elog_wide %>% select(cruise, station, cast, elog_deploy, elog_recover),
    by = c("cruise", "station", "cast")
  ) %>%
  filter(
    is.na(elog_deploy) |
      (date_time >= elog_deploy  - BUFFER_SECS &
       date_time <= elog_recover + BUFFER_SECS)
  ) %>%
  select(-elog_deploy, -elog_recover)

message("Rows after elog trim: ", nrow(tdr_trim))

# relabel any remaining predeploy as downcast
# tdr_trim <- tdr_trim %>%
#   mutate(down_up = if_else(down_up == "predeploy", "downcast", down_up))

## --- which casts had no elog match (untrimmed) ---
untrimmed <- tdr_trim %>%
  distinct(cruise, station, cast) %>%
  anti_join(elog_wide, by = c("cruise", "station", "cast")) %>%
  arrange(cruise, station, cast)

message("  Casts with no elog match: ", nrow(untrimmed))
print(untrimmed, n = Inf)
# EN655 = L9B15 has tdr cast but no sample hit bottom no time to re-do
# EN712  = L6B5 hit bottom = no sample = tdr cast but no sample

## ------------------------------------------ ##
##  9a. Post-trim profile plots (labeled)  ----
## ------------------------------------------ ##
#### NEED TO CHECK THE PLOTS STILL UNSURE ABOUT THE CUTS
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
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("Post-trim labeled profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x    = element_blank(),
          strip.text     = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

## ------------------------------------------ ##
##  10. Bin to 1-m depth intervals          ----
## ------------------------------------------ ##
# average temp within each 1-m bin per cast x down_up
# drop upcast rows shallower than 2 m (surface tail noise)

tdr_binned <- bin_by_depth(all_data)

message("Binned rows: ", nrow(tdr_binned))
message("Depth bins range: ", min(tdr_binned$depth_bin), " – ",
        max(tdr_binned$depth_bin), " m")

## ------------------------------------------ ##
##  10a. Binned profile plots               ----
## ------------------------------------------ ##

pdf(here("figures", "binned_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = avg_temp_C, y = depth_bin, color = down_up)) +
    geom_path() +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(
      values = c(downcast = "steelblue", upcast = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Binned T profiles —", cr),
         x = "Avg temp (°C)", y = "Depth bin (m)") +
    theme_minimal() +
    theme(strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

pdf(here("figures", "binned_depth_time_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_bin, color = avg_temp_C)) +
    geom_point(size = 1.2) +
    scale_y_reverse() +
    scale_color_viridis_c(option = "plasma", name = "°C") +
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("Depth vs time (colored by temp) —", cr),
         x = NULL, y = "Depth bin (m)") +
    theme_minimal() +
    theme(axis.text.x     = element_blank(),
          strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

pdf(here("figures", "binned_temp_spread_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = avg_temp_C, y = depth_bin,
               color = down_up, group = down_up)) +
    geom_path(linewidth = 0.6) +
    geom_point(size = 0.8, alpha = 0.6) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(
      values = c(downcast = "steelblue", upcast = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Down vs upcast temp —", cr),
         x = "Avg temp (°C)", y = "Depth bin (m)") +
    theme_minimal() +
    theme(strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_bin), .groups = "drop") %>%
  ggplot(aes(x = reorder(paste(station, cast), max_depth), y = max_depth)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~cruise, scales = "free_x") +
  labs(title = "Max depth per cast by cruise",
       x = NULL, y = "Max depth bin (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        strip.text  = element_text(size = 7))

tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(max_temp = max(avg_temp_C), .groups = "drop") %>%
  ggplot(aes(x = reorder(paste(station, cast), max_temp), y = max_temp)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~cruise, scales = "free_x") +
  labs(title = "Max temp per cast by cruise",
       x = NULL, y = "Max temp bin (C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        strip.text  = element_text(size = 7))

## ------------------------------------------ ##
##  11. ADD COMMENTS !!                    ----
## ------------------------------------------ ##




## ------------------------------------------ ##
##  12. Cast QC summary                    ----
## ------------------------------------------ ##

cast_qc <- tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(
    max_depth_m     = max(depth_bin,      na.rm = TRUE),
    n_depth_bins    = n_distinct(depth_bin),
    n_downcast_bins = sum(down_up == "downcast"),
    n_upcast_bins   = sum(down_up == "upcast"),
    date_time_start = min(date_time,      na.rm = TRUE),
    date_time_end   = max(date_time,      na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    duration_min = as.numeric(difftime(date_time_end, date_time_start,
                                       units = "mins"))
  )

print(cast_qc, n = 20)

## ------------------------------------------ ##
##  13. Save outputs                        ----
## ------------------------------------------ ##
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# per-cruise CSVs
message("Saving per-cruise CSVs ...")
walk(unique(tdr_binned$cruise), function(cr) {
  out_path <- here(OUT_DIR, paste0(cr, "_tdr_processed.csv"))
  filter(tdr_binned, cruise == cr) %>% write_csv(out_path)
  message("  Saved: ", basename(out_path))
})

# combined
write_csv(tdr_binned, here(OUT_DIR, "allTDRdata.csv"))
message("Saved combined -> allTDRdata.csv")

# QC table
write_csv(cast_qc, here(OUT_DIR, "tdr_cast_qc_summary.csv"))
message("Saved QC -> tdr_cast_qc_summary.csv")

message("\nDone.")





## ------------------------------------------ ##
##       OLD CODE ----
## ------------------------------------------ ##

## --------------------------------------------------------- ##
# --- AE2426 L8 B13 ---
# tdr recorded ~55 min of surface noise before actual cast.
# ae2426_L8_B13_descent <- as.POSIXct("2024-11-09 19:12:37", tz = "UTC")
# 
# all_data <- all_data %>%
#   mutate(down_up = case_when(
#     cruise == "AE2426" & station == "L8" & cast == "B13" &
#       date_time < ae2426_L8_B13_descent ~ "predeploy",
#     TRUE ~ down_up
#   ))

## --- flag sparse casts ---
sparse_casts <- all_data %>%
  count(cruise, station, cast) %>%
  filter(n < 50) %>%  # fewer than 50 obs = likely high interval recording
  mutate(flag_sparse = TRUE)

message(nrow(sparse_casts), " sparse cast(s):")
print(sparse_casts)

# don't remove yet — just flag for now and revisit
all_data <- all_data %>%
  left_join(sparse_casts %>% select(cruise, station, cast, flag_sparse),
            by = c("cruise", "station", "cast")) %>%
  mutate(flag_sparse = replace_na(flag_sparse, FALSE))

# EN617 L11 B25ab
# en617_L11_B25a_end   <- as.POSIXct("2018-07-25 08:50:00", tz = "UTC")
# en617_L11_B25b_start <- as.POSIXct("2018-07-25 09:16:00", tz = "UTC")
# 
# all_data <- all_data %>%
#   mutate(cast = case_when(
#     cruise == "EN617" & station == "L11" & cast == "B25ab" &
#       date_time <= en617_L11_B25a_end   ~ "B25a",
#     cruise == "EN617" & station == "L11" & cast == "B25ab" &
#       date_time >= en617_L11_B25b_start ~ "B25b",
#     cruise == "EN617" & station == "L11" & cast == "B25ab" ~ NA_character_,
#     TRUE ~ cast
#   )) %>%
#   filter(!is.na(cast))
## ---------------------- ##
# multiple casts - need to manually inspect
# AE2426 = L8 B13 FIXED; L9 B12 correctly identified upcast
#        = L1B1 & L4B18 = have a not-connected bit early on (surface time) that is still part of the cast (should be deleted)

# AR77 = L02 B02 = these may be two separate casts that i may need to manually split; need to check
#        L5B5 = did we accidentally collect data longer interval than 1sec??

# AR88 = L5B5 = have a not-connected bit early on (surface time) that is still part of the cast (should be deleted)

# AR92 = u9a B18 = have a not-connected bit early on (surface time) that is still part of the cast (should be deleted)

# AT46 = all casts have long predeploy sections that should be excluded/deleted
#      = L2B21; L3B22; L4B2; L5B3; L6B4; L7B14; L9B12; MVCO B23 = all have upcast data that keeps going well past net being on surface/back on deck

# EN649 = pretty much all casts have upcast that keep going past net being back on deck and a couple have downcast before net was deployed

# EN655 = same issue with many keeping upcast points well past net being on deck

# EN657 = same issue with many keeping upcast points well past net being on deck
#       = L03 B02 have quick cast and then a deeper one - two casts in this one file - may need to manually check??
#       = L6B17 maybe two casts by accident need to manually check; L9 same issue

# EN687 = L9B12 too few data points; did we accidentally set record to higher interval
#       = the rest are good except that some have downcast or upcast while net was on deck

# EN706 = L05B06 = need to manually check; maybe two casts on same file?

# EN715 = L5B6; L6B13; L8B14 = manually chec; maybe two casts on same file? 
#       = L2B2 has downcast values not connected to rest of cast; before net went in water
#
# HRS2303 = L1B2; L6B9; L7B4; L8B5; L9B6; MVCO B1 = too few data points; did we accidentally set record to higher interval
# DELETE THIS CAST
# delete bad cast
all_data <- all_data %>%
  filter(!(cruise == "EN617" & station == "L1" & cast == "B1"))

# add notes which messed up 
# 2019 = EN627 = two casts L8B19 = first one hit bottom and redid cast
# 2020 = EN655 = L9B15 has tdr cast but no sample hit bottom no time to re-do
# 2020 = EN657 = 3 with multiple casts L3B2 (this contains L1 cast); L9B8 (L9B14,L8B15)
# 2022 = AT46  = no TDR for L8B13
# 2023 = EN706   = L5B6 re-did deployment; so delete first aborted cast
# 2024 = EN712  = L6B5 hit bottom = no sample = tdr cast but no sample
# 2024 = EN715  = L5B6 = hit bottom and re-did cast; delete first bad cast
# 2024 = AE2426 = L9B12 upcast only 
# 2025 = AR95  = L3B19 TDR turned on after net in water
# 2026 = AR99  = L2B3 = the second cast is a ring net only at same L2R3
#              = L10B6 = TDR turned on after net in water
# includes the following cruises (21): 
# 2018 = EN608, EN617
# 2019 = EN627, EN644
# 2020 = EN649, EN655, EN657
# 2021 = 
# 2022 = AT46, EN687
# 2023 = HRS2303, EN706, AR77
# 2024 = EN712, EN715, EN720, AE2426
# 2025 = EN727, AR88, AR92, AR95
# 2026 = AR99
# EN617 = L11B25ab = flowmeter calibration
# EN627 = L8B19 = re-did deployment; so delete first aborted cast
# EN657 = L3B2 (this contains L1 file); L6B17; L9B8 (L9B14,L8B15)
# AT46  = L5B3 = maybe? not two casts but has mini spike after main cast
# EN706 = L5B6 = re-did deployment; so delete first aborted cast
# AR77  = L2B2 = re-did deployment; so delete first aborted cast
# EN715 = L5B6 = re-did deployment; so delete first aborted cast
#       = L6B13 = re-did deployment; so delete first aborted cast
#       = L8B14 = re-did deployment; so delete first aborted cast
# AE2426 = L8B13 = up/downs before actual cast
# AR99 = L2B3  = the second cast is a ring net only at same L2R3
#      = L6B10 = the second cast is a ring net only at same L2R3
#      = L9B5  = the second cast is a ring net only at same L2R3