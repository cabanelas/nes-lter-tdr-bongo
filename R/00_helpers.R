###############################################################
##  NES-LTER Bongo TDR: Helper Functions
##  Project: nes-lter-tdr-bongo
##  Script:  00_helpers.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Shared utility functions for TDR bongo data processing.
##           Sourced by 02_tdr_tidy.R and downstream scripts.
##
##  Functions:
##    parse_filename_meta()  - parse cruise/station/cast from filename
##    strip_leading_zeros()  - strip leading zeros after letter prefix
##    coalesce_temp_col()    - merge encoding variants of temp column
##    read_tdr_csv()         - read and standardize one TDR CSV file
##    find_first_peak()      - find first local depth peak in a profile
##    auto_split_casts()     - split merged multi-cast files by depth valley
##    detect_and_split()     - wrapper with message for auto_split_casts()
##    label_down_up()        - label predeploy / downcast / upcast rows
##    bin_by_depth()         - bin to 1-m depth intervals, average temp
##
###############################################################

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

## --- Auto-split a data frame containing multiple casts ---
# Detects two casts by finding a shallow valley between two deeper excursions.
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
# label_down_up <- function(df) {
#   df       <- arrange(df, date_time)
#   peak_idx <- which.max(df$depth_m)
#   
#   # find where the cast actually starts descending
#   # first row where depth exceeds 1 m heading toward the peak
#   descent_start <- which(df$depth_m > 1)[1]
#   if (is.na(descent_start)) descent_start <- 1L
#   
#   df$down_up <- case_when(
#     seq_len(nrow(df)) < descent_start   ~ "predeploy",
#     seq_len(nrow(df)) <= peak_idx       ~ "downcast",
#     TRUE                                ~ "upcast"
#   )
#   df
# }
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

## --- for reading API2 data
# 02_px_sensor_tidy.R
# 03_tdr_offsets.R
safe_read_csv <- function(url) {
  tryCatch(
    read_csv(url, show_col_types = FALSE),
    error = function(e) { message("  FAILED: ", url); NULL }
  )
}