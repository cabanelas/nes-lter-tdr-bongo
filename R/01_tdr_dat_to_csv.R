###############################################################
##  NES-LTER Bongo TDR: Convert .DAT files to CSV
##  Project: nes-lter-tdr-bongo
##  Script:  01_tdr_dat_to_csv.R
##  Author:  Alexandra Cabanelas
##  Purpose: Parse raw TDR .DAT files for cruises EN608, EN627,
##           EN644; save individual cast CSVs and one combined
##           per-cruise CSV to data/raw/tdr_data
###############################################################
# converting .DAT files to csv for TDR files that were never
# saved as xlsx/csv
# saving csv files for cruises that only have .DAT and/or xlsx
## TDR == Time-Depth Recorder Sea Star ODDI

## ------------------------------------------ ##
##  Packages                               ----
## ------------------------------------------ ##
library(dplyr)
library(here)
library(readxl)
library(openxlsx)

## ------------------------------------------ ##
##  Helpers                                ----
## ------------------------------------------ ##
TDR_DIR <- here("data", "raw", "tdr_data")

## --- Parse a single TDR .DAT file ---
#' @param file  Full path to a .DAT file.
#' @return      A df with cols:
#             Index, DateTime, Temperature, Depth, cruise, station, cast
read_dat_file <- function(file) {
  df <- read.table(
    file,
    skip             = 15,
    sep              = "\t",
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  colnames(df) <- c("Index", "DateTime", "Temperature", "Depth")

  df$Temperature <- as.numeric(gsub(",", ".", df$Temperature))
  df$Depth       <- as.numeric(gsub(",", ".", df$Depth))
  df$DateTime    <- as.POSIXct(
    df$DateTime,
    format = "%d.%m.%Y %H:%M:%OS",
    tz     = "UTC"
  )

  # filename: <cruise>-<station>-<cast>.<ext>
  parts      <- strsplit(basename(file), "-", fixed = TRUE)[[1]]
  df$cruise  <- parts[2]
  df$station <- parts[3]
  df$cast    <- gsub("\\.[^.]+$", "", parts[4])   # strip extension
  df$file_stem <- gsub("\\.[^.]+$", "", basename(file)) 
  df
}

## --- Strip leading zeros from station / cast labels ---
# Converts L03 to L3, B07 to B7
#' @param x  Character vector of station or cast codes
#' @param prefix  The single-letter prefix "L" or "B"
#' @return   Character vector with leading zeros removed
strip_leading_zeros <- function(x, prefix) {
  pattern     <- paste0("^", prefix, "0*")
  replacement <- prefix
  sub(pattern, replacement, x)
}

## --- Save individual cast CSVs + one combined CSV for a cruise ---
# Individual files: data/processed/<cruise>_individual/
# Combined file:  data/processed/<cruise>_allTDRcasts.csv
#' @param combined_df  Data frame with all casts for one cruise.
#' @param cruise_id    String used in output filenames, e.g. "EN627".
save_cruise_outputs <- function(combined_df, cruise_id) {
  # --- individual cast CSVs ---
  indiv_dir <- here("data", "raw", "tdr_data", paste0(cruise_id, "_TDR"))
  
  if (!dir.exists(indiv_dir)) {
    stop("TDR folder not found: ", indiv_dir, 
         "\nCreate the folder first or check cruise_id spelling.")
  }

  file_stems <- unique(combined_df$file_stem)
  
  for (stem in file_stems) {
    cast_df  <- combined_df[combined_df$file_stem == stem, ]
    out_file <- file.path(indiv_dir, paste0(stem, ".csv"))
    write.csv(cast_df, out_file, row.names = FALSE)
  }
  
  message(sprintf("  Saved %d individual cast CSV(s) → %s",
                  length(file_stems), indiv_dir))
  
  # --- combined cruise CSV ---
  # proc_dir <- here("data", "processed")
  # if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
  # 
  # combined_file <- here("data", "processed",
  #                       paste0(cruise_id, "_allTDRcasts.csv"))
  # write.csv(combined_df, combined_file, row.names = FALSE)
  # message(sprintf("  Saved combined CSV → %s", combined_file))
}

## ------------------------------------------ ##
##             EN627       ----
## ------------------------------------------ ##
# NOTE: L02 & L03 casts together in same .DAT file. fixed here w timestamps

message("Processing EN627 ...")

en627_files <- list.files(
  file.path(TDR_DIR, "EN627_TDR"),
  pattern   = "\\.dat$",
  full.names = TRUE
)

en627_dat_list <- lapply(en627_files, read_dat_file)
en627_combined <- do.call(rbind, en627_dat_list)

# --- Split the merged L02&L03 cast ---
#L02 B07 timestamp 02.02.2019 08:30:01,000 to 02.02.2019 08:37:01,000
#L03 B08 timestamp 02.02.2019 10:43:01,000 to 02.02.2019 10:50:01,000
start_L02 <- as.POSIXct("2019-02-02 08:30:01", tz = "UTC")
end_L02   <- as.POSIXct("2019-02-02 08:37:01", tz = "UTC")
start_L03 <- as.POSIXct("2019-02-02 10:43:01", tz = "UTC")
end_L03   <- as.POSIXct("2019-02-02 10:50:01", tz = "UTC")

en627_L02 <- en627_combined[
  en627_combined$DateTime >= start_L02 &
  en627_combined$DateTime <= end_L02, ]
en627_L03 <- en627_combined[
  en627_combined$DateTime >= start_L03 &
  en627_combined$DateTime <= end_L03, ]

# Fix station/cast labels on the split subsets
en627_L02$station <- "L02"
en627_L02$cast    <- "B07"
en627_L02$file_stem <- "1$28C9447-EN627-L02-B07"  

en627_L03$station <- "L03"
en627_L03$cast    <- "B08"
en627_L03$file_stem <- "1$28C9447-EN627-L03-B08"

# Rebuild: drop the merged row, add the two separated rows
en627_combined <- en627_combined %>%
  filter(station != "L02&03") %>%
  bind_rows(en627_L02, en627_L03)

save_cruise_outputs(en627_combined, "EN627")

## ------------------------------------------ ##
##         EN644         ----
## ------------------------------------------ ##
message("Processing EN644 ...")

en644_files <- list.files(
  file.path(TDR_DIR, "EN644_TDR"),
  pattern    = "\\.dat$",
  full.names = TRUE
)

en644_combined <- do.call(rbind, lapply(en644_files, read_dat_file))

en644_combined$station <- strip_leading_zeros(en644_combined$station, "L")
en644_combined$cast    <- strip_leading_zeros(en644_combined$cast,    "B")

save_cruise_outputs(en644_combined, "EN644")

## ------------------------------------------ ##
##         EN608        ----
## ------------------------------------------ ##
# NOTE: EN608 files have a non-standard extension; list.files()
#       uses full.names only (no pattern filter) to capture all files.

message("Processing EN608 ...")

en608_files <- list.files(
  file.path(TDR_DIR, "EN608_TDR"),
  full.names = TRUE
)

en608_combined <- do.call(rbind, lapply(en608_files, read_dat_file))

en608_combined$station <- strip_leading_zeros(en608_combined$station, "L")
en608_combined$cast    <- strip_leading_zeros(en608_combined$cast,    "B")

save_cruise_outputs(en608_combined, "EN608")

## ------------------------------------------ ##
##  Convert xlsx-only files to CSV        ----
##  Runs automatically for any cruise folder
##  where xlsx exists but no matching csv
## ------------------------------------------ ##

RAW_DIR <- here("data", "raw")
SKIP_FILES <- "tdr_offsets.csv"

message("\nChecking for xlsx files without matching CSV ...")

all_xlsx <- list.files(RAW_DIR,
                       pattern    = "\\.xlsx$",
                       full.names = TRUE,
                       recursive  = TRUE)

# skip offsets file
all_xlsx <- all_xlsx[!basename(all_xlsx) %in% SKIP_FILES]

converted <- 0

for (xlsx_path in all_xlsx) {
  csv_path <- sub("\\.xlsx$", ".csv", xlsx_path)
  
  if (!file.exists(csv_path)) {
    message(glue::glue("  Converting: {basename(xlsx_path)}"))
    
    # try readxl first
    df <- tryCatch(
      readxl::read_excel(xlsx_path),
      error = function(e) {
        # fall back to openxlsx for non-standard xlsx files (AR95, AR99 etc.)
        tryCatch({
          d <- openxlsx::read.xlsx(xlsx_path, sheet = "DAT")
          # openxlsx returns date as Excel serial number — convert to POSIXct
          date_col <- names(d)[grepl("date|time", names(d), 
                                     ignore.case = TRUE)][1]
          if (!is.na(date_col) && is.numeric(d[[date_col]])) {
            d[[date_col]] <- as.POSIXct(
              (as.numeric(d[[date_col]]) - 25569) * 86400,
              origin = "1970-01-01", tz = "UTC"
            )
          }
          d
        },
        error = function(e2) {
          message(glue::glue("    ! Failed both readxl and openxlsx: {basename(xlsx_path)}"))
          NULL
        })
      }
    )
    
    if (!is.null(df) && nrow(df) > 0) {
      write_csv(df, csv_path)
      converted <- converted + 1
    }
  }
}

if (converted == 0) {
  message("  All xlsx files already have matching CSV — nothing to convert.")
} else {
  message(glue::glue("  Converted {converted} xlsx file(s) to CSV."))
}

################################################################################
# go to -----------> 02_tdr_tidy.R
#           OR     > 02_px_sensor_tidy.R
#           OR     > 02_ctd_bongo_tidy.R 
################################################################################