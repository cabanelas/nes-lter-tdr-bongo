###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  tdr_files_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Read per cast CSV files from all cruise TDR folders,
##           tidy data, detect mutiple casts,
##           label downcast/upcast, bin to 1-m depth intervals,
##           and save per-cruise + one combined CSV.
##
##  Need to have: files_convert_dat_to_csv.R having already produced
##              CSVs for DAT-only cruises (EN608, EN627, EN644).
##
##  Input:   raw/<CRUISE>_TDR/*.csv   (one CSV per cast)
##           raw/tdr_offsets.csv      (excluded from read)
##
##  Output:  data/processed/<CRUISE>_tdr_processed.csv  (per cruise)
##           data/processed/allTDRdata.csv               (combined)
##           data/processed/tdr_cast_qc_summary.csv      (QC table)
##           data/processed/tdr_ctd_tests.csv            
###############################################################

#####################################EN706 most likely bad -- CHECK
## ------------------------------------------ ##
# includes the following cruises (16): 
# 2018 = EN617
# 2019 = 
# 2020 = EN649, EN655, EN657
# 2021 = 
# 2022 = AT46, EN687
# 2023 = HRS2303, EN706, AR77
# 2024 = EN712, EN715, EN720, AE2426
# 2025 = EN727, AR88, AR92, AR95
# 2026 = AR99, ***need to add HRS2601***
## ------------------------------------------ ##

# .DAT files only, csv created in a separate R script: EN644, EN627, EN608

# no TDR data for the following cruises: EN661, AR63*, AR38, AR32, EN715, EN695, EN668 (CTD only)
# CTD data available for: EN668 (no TDR) and EN706

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

RAW_DIR    <- here("data", "raw")
OUT_DIR    <- here("data", "processed")
SKIP_FILES <- "tdr_offsets.csv"  # excluded from read

# Minimum time gap (seconds) between end of one upcast and start
MULTI_CAST_GAP_SEC <- 1200   # 20 minutes 

## ------------------------------------------ ##
##  Helpers                                ----
## ------------------------------------------ ##

## --- Parse cruise / station / cast from a TDR CSV filename ---
# handles 2 naming convesions:
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
  
  rename_map <- c(
    date_time = "Date & Time",
    temp_C    = "Temp(\u00b0C)",
    depth_m   = "Depth(m)"
  )
  for (new_nm in names(rename_map)) {
    old_nm <- rename_map[[new_nm]]
    if (old_nm %in% colnames(df)) df <- rename(df, !!new_nm := !!old_nm)
  }
  
  if ("Date.&amp;.Time" %in% colnames(df)) {
    df <- rename(df, date_time = `Date.&amp;.Time`)
  }
  
  df %>%
    mutate(
      cruise  = toupper(meta$cruise),
      station = meta$station,
      cast    = meta$cast,
      date_time = as.character(date_time) 
    ) %>%
    select(any_of(c("date_time", "temp_C", "depth_m",
                    "cruise", "station", "cast")))
}

## --- Auto-split a data frame containing multiple tows ---
#   1. Sort by time
#   2. Find the global depth maximum (end of tow 1 downcast)
#   3. After that peak, find where the instrument returns near the
#      surface (depth < 5 m) and then a time gap >= MULTI_CAST_GAP_SEC
#      appears = this gap marks the boundary between tows
#   4. Assign cast suffix _1, _2, _3 ... to each segment
#' @param df        Data frame for one original (cruise, station, cast)
#' @param base_cast Original cast label e.g. "B25"
#' @return  df with cast column relabelled to B25_1, B25_2, etc
#'          If no valid split boundary is found, returns df with cast
#'          relabeled to B25_1 only
auto_split_casts <- function(df, base_cast) {
  df <- arrange(df, date_time)
  n  <- nrow(df)
  
  elapsed       <- as.numeric(difftime(df$date_time, df$date_time[1],
                                       units = "secs"))
  first_peak_idx <- which.max(df$depth_m)
  
  # rows after first peak where instrument is near surface
  after_peak    <- which(seq_len(n) > first_peak_idx & df$depth_m < 2)
  
  if (length(after_peak) == 0) {
    df$cast <- paste0(base_cast, "_1")
    return(df)
  }
  
  # look for a time gap > MULTI_CAST_GAP_SEC within the surface window
  surface_times <- elapsed[after_peak]
  gaps          <- diff(surface_times)
  big_gap_pos   <- which(gaps > MULTI_CAST_GAP_SEC)
  
  if (length(big_gap_pos) == 0) {
    df$cast <- paste0(base_cast, "_1")
    return(df)
  }
  
  # split boundaries: row index of the first observation after each gap
  boundary_times <- surface_times[big_gap_pos + 1]
  split_at       <- sapply(boundary_times, function(t) which(elapsed >= t)[1])
  split_at       <- c(1L, split_at, n + 1L)   # bookend with start / sentinel
  
  df$cast <- NA_character_
  for (seg in seq_len(length(split_at) - 1)) {
    rows            <- split_at[seg]:(split_at[seg + 1] - 1)
    df$cast[rows]   <- paste0(base_cast, "_", seg)
  }
  
  df
}

## --- Detect and split a multi cast group ---
# Check for a second deep excursion > 20 m after a gap of at
# least MULTI_CAST_GAP_SEC from the first depth maximum
# When found, calls auto_split_casts() and prints a VERIFY warning
#' @param df        Data frame for one (cruise, station, cast) group
#' @param base_cast Original cast label
#' @return  df with cast relabelled if a split was triggered
detect_and_split <- function(df, base_cast) {
  df         <- arrange(df, date_time)
  elapsed    <- as.numeric(difftime(df$date_time, df$date_time[1],
                                    units = "secs"))
  peak1_time <- elapsed[which.max(df$depth_m)]
  
  after_gap  <- df[elapsed > peak1_time + MULTI_CAST_GAP_SEC & df$depth_m < 2, ]
  max_after  <- if (nrow(after_gap) > 0) max(df$depth_m[elapsed > peak1_time + MULTI_CAST_GAP_SEC], na.rm = TRUE) else 0
  
  if (max_after > 20) {
    message(sprintf(
      "  AUTO-SPLIT: cruise=%-8s station=%-6s cast=%-8s (2nd excursion: %.0f m) — VERIFY",
      df$cruise[1], df$station[1], base_cast, max_after))
    df <- auto_split_casts(df, base_cast)
  } else {
    df$cast <- base_cast   # no split; preserve original label
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

all_csv_paths <- all_csv_paths[!basename(all_csv_paths) %in% SKIP_FILES]
message(length(all_csv_paths), " CSV file(s).")

## ------------------------------------------ ##
##  2. Read data & combine               ----
## ------------------------------------------ ##

all_data <- lapply(all_csv_paths, read_tdr_csv) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()
message("  Total rows: ", nrow(all_data))

## ------------------------------------------ ##
##  EDA          ----
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
# look for unexpected formats before cleaning
sort(unique(all_data$station))
sort(unique(all_data$cast))

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

## ---------------------- ##
# multiple casts - need to manually inspect
# AE2426 = L8 B13; L9 B12 (messed up like it started after net was in water)
# AR77 = L02 B02
# AT46 = L5 B3 = maybe?
# EN617 = L11B25ab = i think tdr calibration
# EN657 = L03 B02; L6B17; L9
# EN706 = L05B06
# EN715 = L5B6; L6B13; L8B14

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

# should be 0
sum(is.na(all_data$date_time))

# date range should make sense — check all cruises present
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
             linewidth = 0.8) +  # your candidate threshold
  labs(
    title = "Time gaps near surface after depth peak",
    subtitle = "Dashed line = 30 min",
    x = "Gap duration (minutes)",
    y = "Count"
  ) +
  theme_minimal()

# which specific casts have large gaps
gap_diagnostics %>%
  filter(time_gap_to_next > 2) %>%   # lower than your threshold to see candidates
  select(cruise, station, cast, elapsed, time_gap_to_next, depth_m) %>%
  arrange(desc(time_gap_to_next)) %>%
  print(n = 30)

## ------------------------------------------ ##
##  4. Isolate TDR-CTD bench tests   ----
## ------------------------------------------ ##
tdr_test <- filter(all_data,
                   grepl("^TDRCTD", station, ignore.case = TRUE)) %>%
  mutate(comments = "tdr_ctd_test",
         station  = str_extract(cast, "L\\d+"),
         cast     = str_extract(cast, "B\\d+"))

if (nrow(tdr_test) > 0) {
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
  write_csv(tdr_test, here(OUT_DIR, "tdr_ctd_tests.csv"))
  message("  Saved ", nrow(tdr_test),
          " TDR-CTD test rows -> tdr_ctd_tests.csv")
}

all_data <- filter(all_data,
                   !grepl("^TDRCTD", station, ignore.case = TRUE))

## ------------------------------------------ ##
##  5. Join depth offsets                  ----
## ------------------------------------------ ##
# offset_m is the instrument depth offset for a given cast
# add depth_offset column.
# Corrected depth = depth_m - offset_m 

### NEED TO CHECK AND ADD OFFSETS TO CRUISES 
# AR77; EN712, EN715, EN720, AE2426; EN727, AR88, AR92, AR95; AR99

offsets <- read.csv(here("data", "raw", "tdr_offsets.csv"),
                    stringsAsFactors = FALSE) %>%
  mutate(across(c(cruise, station, cast), as.character))

all_data <- all_data %>%
  left_join(offsets, by = c("cruise", "station", "cast")) %>%
  mutate(
    depth_offset = replace_na(offset_m, 0),  # 0 if no offset recorded
    depth_m_raw  = depth_m,                  # preserve original
    depth_m      = depth_m - depth_offset    # corrected depth
  ) %>%
  select(-offset_m) %>%
  # after correction, remove any rows that went negative
  filter(depth_m >= 0)

# report which casts received a non-zero offset
offsets_applied <- all_data %>%
  filter(depth_offset != 0) %>%
  distinct(cruise, station, cast, depth_offset)

message("Depth offsets applied to ", nrow(offsets_applied), " cast(s):")
print(offsets_applied)

## ------------------------------------------ ##
##  6. Split merged multi-cast files       ----
## ------------------------------------------ ##
# detect_and_split() checks whether a single file contains two or more
# back-to-back tows (depth goes deep, returns to surface, then goes deep
# after a gap >= MULTI_CAST_GAP_SEC)

all_data <- all_data %>%
  group_by(cruise, station, cast) %>%
  do({
    detect_and_split(., base_cast = .$cast[1])
  }) %>%
  ungroup()

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
##  7. Cruise-specific manual corrections  ----
## ------------------------------------------ ##
# After running once and reviewing the PDF + VERIFY messages,
# add case_when() blocks here to fix any mis-labeled or mis-split casts.
#
# Template:
# all_data <- all_data %>%
#   mutate(cast = case_when(
#     cruise == "XXXXX" & station == "LXX" &
#       date_time >= as.POSIXct("YYYY-MM-DD HH:MM:SS", tz = "UTC") &
#       date_time <= as.POSIXct("YYYY-MM-DD HH:MM:SS", tz = "UTC") ~ "BXXa",
#     TRUE ~ cast
#   ))







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
##  8a. Manual down_up corrections         ----
## ------------------------------------------ ##

# --- AE2426 L8 B13 ---
# tdr recorded ~55 min of surface noise before actual cast.
ae2426_L8_B13_descent <- as.POSIXct("2024-11-09 19:12:37", tz = "UTC")

all_data <- all_data %>%
  mutate(down_up = case_when(
    cruise == "AE2426" & station == "L8" & cast == "B13" &
      date_time < ae2426_L8_B13_descent ~ "predeploy",
    TRUE ~ down_up
  ))

## ------------------------------------------ ##
##  8b. Post-label QC checks              ----
## ------------------------------------------ ##

label_qc <- all_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_predeploy    = sum(down_up == "predeploy"),
    n_downcast     = sum(down_up == "downcast"),
    n_upcast       = sum(down_up == "upcast"),
    max_depth      = max(depth_m, na.rm = TRUE),
    predeploy_mins = {
      pd <- date_time[down_up == "predeploy"]
      if (length(pd) > 1) as.numeric(difftime(max(pd), min(pd), units = "mins"))
      else 0
    },
    downcast_mins  = {
      dc <- date_time[down_up == "downcast"]
      if (length(dc) > 1) as.numeric(difftime(max(dc), min(dc), units = "mins"))
      else 0
    },
    .groups = "drop"
  ) %>%
  mutate(
    pct_predeploy        = n_predeploy / (n_predeploy + n_downcast + n_upcast),
    flag_no_downcast     = n_downcast == 0,
    flag_shallow         = max_depth < 10,
    flag_long_predeploy  = predeploy_mins > 5,
    flag_high_predeploy  = pct_predeploy > 0.3,
    # cast that reaches 60m should take at least 1 min to descend
    flag_too_fast        = downcast_mins < (max_depth / 60) * 0.3,
    any_flag             = flag_no_downcast | flag_shallow |
      flag_long_predeploy | flag_high_predeploy |
      flag_too_fast
  )

flagged_labels <- filter(label_qc, any_flag)

if (nrow(flagged_labels) > 0) {
  message("\n!! ", nrow(flagged_labels),
          " cast(s) flagged for label review:")
  flagged_labels %>%
    select(cruise, station, cast, max_depth, predeploy_mins,
           downcast_mins, pct_predeploy, starts_with("flag_")) %>%
    print(n = 50)
} else {
  message("  All cast labels look clean.")
}

# Save label QC for reference
write_csv(label_qc, here(OUT_DIR, "tdr_label_qc.csv"))

## ------------------------------------------ ##
##  8c. Post-label profile plots           ----
## ------------------------------------------ ##
# profiles (predeploy / downcast / upcast) to PDF.

pdf(here("figures", "labeled_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(all_data$cruise))) {
  p <- all_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
    geom_point(size = 0.3, alpha = 0.6) +
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


### WHERE TO PUT THE MANUAL CORRECTIONS
## ------------------------------------------ ##
##  7. Cruise-specific manual corrections  ----
## ------------------------------------------ ##
# get start and end times of each cast

## ------------------------------------------ ##
##  3b. Trim TDR data to tow start/end times ----
##      Uses tow_meta start/end UTC timestamps
##      Removes pre-deploy noise and post-recovery tail
## ------------------------------------------ ##

BUFFER_SECS <- 120  # 2 min buffer on each end

tow_meta <- read_csv(here("data", 
                          "nes-lter-zooplankton-tow-metadata-v2.csv"),
                     show_col_types = FALSE)

tow_windows <- tow_meta %>%
  mutate(cruise = toupper(cruise)) %>%
  filter(!is.na(date_start_UTC), !is.na(time_start_UTC),
         !is.na(date_end_UTC),   !is.na(time_end_UTC)) %>%
  mutate(
    tow_start = as.POSIXct(
      paste(date_start_UTC, format(time_start_UTC, "%H:%M:%S")),
      format = "%d-%m-%y %H:%M:%S", tz = "UTC"
    ),
    tow_end = as.POSIXct(
      paste(date_end_UTC, format(time_end_UTC, "%H:%M:%S")),
      format = "%d-%m-%y %H:%M:%S", tz = "UTC"
    )
  ) %>%
  filter(!is.na(tow_start), !is.na(tow_end)) %>%
  select(cruise, station, cast, tow_start, tow_end)

message(glue::glue("Tow windows available: {nrow(tow_windows)} cruise × station"))

# which TDR cruises have tow_meta coverage?
trimmed_cruises  <- intersect(unique(all_data$cruise), unique(tow_windows$cruise))
untrimmed_cruises <- setdiff(unique(all_data$cruise), unique(tow_windows$cruise))
message(glue::glue("Cruises with tow windows:    {paste(sort(trimmed_cruises),  collapse = ', ')}"))
message(glue::glue("Cruises WITHOUT tow windows: {paste(sort(untrimmed_cruises), collapse = ', ')}"))

# NEED TO ADD TIMESTAMPS FOR NEWER CRUISES NOT IN THE METADATA FILE PACKAGE
# Cruises WITHOUT METADATA/TIME: AE2426, AR88, AR92, AR95, AR99, EN727

n_before <- nrow(all_data)

# strip B prefix from TDR cast to match tow_meta numeric cast
# "B1" -> "1", "B21" -> "21"
all_data <- all_data %>%
  mutate(cast_num = gsub("^B", "", cast)) %>%
  left_join(tow_windows, by = c("cruise", "station", "cast_num" = "cast")) %>%
  mutate(
    keep = is.na(tow_start) |
      (date_time >= tow_start - BUFFER_SECS &
         date_time <= tow_end   + BUFFER_SECS)
  ) %>%
  filter(keep) %>%
  select(-tow_start, -tow_end, -keep, -cast_num)

n_after <- nrow(all_data)
message(glue::glue("Rows removed by tow window trim: {n_before - n_after}"))
message(glue::glue("Rows remaining: {n_after}"))




## --- Category 1: trim upcast tail by timestamp ---
## These casts have real data recorded after net was back on deck
## Use event log or bongo log times to set hard cutoffs

upcast_cutoffs <- tribble(
  ~cruise,   ~station, ~cast,   ~cutoff_utc,
  # AT46 — all casts run long; add cutoffs from event log
  # EN649 — most casts run long
  # template:
  # "EN657",  "L3",    "B2",  "2020-10-15 14:30:00"
)

# apply cutoffs — remove rows after net back on deck
if (nrow(upcast_cutoffs) > 0) {
  upcast_cutoffs <- upcast_cutoffs %>%
    mutate(cutoff_utc = as.POSIXct(cutoff_utc, tz = "UTC"))
  
  all_data <- all_data %>%
    left_join(upcast_cutoffs, by = c("cruise", "station", "cast")) %>%
    filter(is.na(cutoff_utc) | date_time <= cutoff_utc) %>%
    select(-cutoff_utc)
}

## --- Category 2: manual cast splits by timestamp ---
## For files with 2 casts, define the time boundary

# EN617 L11 B25ab
en617_L11_B25a_end   <- as.POSIXct("2018-07-25 08:50:00", tz = "UTC")
en617_L11_B25b_start <- as.POSIXct("2018-07-25 09:16:00", tz = "UTC")

all_data <- all_data %>%
  mutate(cast = case_when(
    cruise == "EN617" & station == "L11" & cast == "B25ab" &
      date_time <= en617_L11_B25a_end   ~ "B25a",
    cruise == "EN617" & station == "L11" & cast == "B25ab" &
      date_time >= en617_L11_B25b_start ~ "B25b",
    cruise == "EN617" & station == "L11" & cast == "B25ab" ~ NA_character_,
    TRUE ~ cast
  )) %>%
  filter(!is.na(cast))

# Add more splits here following same pattern:
# all_data <- all_data %>%
#   mutate(cast = case_when(
#     cruise == "EN706" & station == "L5" & cast == "B6" &
#       date_time <= as.POSIXct("YYYY-MM-DD HH:MM:SS", tz="UTC") ~ "B6a",
#     ...
#   ))

## --- Category 3: flag sparse casts ---
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





## ------------------------------------------ ##
##  8d. Bin to 1-m depth intervals  ----
## ------------------------------------------ ##

tdr_binned <- bin_by_depth(all_data)

## ------------------------------------------ ##
##  8e. Binned output verification plots   ----
## ------------------------------------------ ##
#### STILL NEED FIXES


pdf(here("figures", "binned_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = avg_temp_C, y = depth_bin, color = down_up)) +
    geom_path() +        # path respects date_time order, not depth order
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
    theme(strip.text = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

## ------------------------------------------ ##
##  9. QC summary                          ----
## ------------------------------------------ ##
cast_qc <- tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(
    max_depth_m     = max(depth_bin,  na.rm = TRUE),
    n_depth_bins    = n_distinct(depth_bin),
    n_downcast_bins = sum(down_up == "downcast"),
    n_upcast_bins   = sum(down_up == "upcast"),
    date_time_start = min(date_time,  na.rm = TRUE),
    date_time_end   = max(date_time,  na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    duration_min = as.numeric(
      difftime(date_time_end, date_time_start, units = "mins")),
    auto_split   = grepl("_[0-9]+$", cast)  # TRUE = came from auto-split
  )

message("\nQC summary (first 10 rows):")
print(head(cast_qc, 10))

## ------------------------------------------ ##
##  10. Save outputs                       ----
## ------------------------------------------ ##
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# per-cruise CSVs
message("\nSaving per-cruise CSVs ...")
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
message("Saved QC summary -> tdr_cast_qc_summary.csv")

message("\nDone.")
