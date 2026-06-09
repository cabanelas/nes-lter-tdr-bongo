###############################################################
##  NES-LTER Bongo CTD: Tidy & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_ctd_bongo_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Read raw SeaBird SBE19plus CNV files from bongo-attached
##           CTD deployments, assign cruise/station/cast metadata,
##           inspect and resolve multi-version casts (EN706 L11 B9),
##           label downcast/upcast, and export cleaned CTD-bongo data.
##
##  Cruises: EN668 (2021), EN706 (2023)
##
##  Known data issues:
##    - EN706 L11 B9 has 3 versions (main=aborted, _b=real, _c=partial);
##      keep _b only (18:51 UTC, 3433 rows, matches logsheet ~18:52-19:22)
##    - Station naming inconsistent across files (leading zeros, lowercase);
##      standardized to L1, L2, ... format
##
##  Input:   data/raw/ctd_bongo/SBE19plus_EN668/*.cnv
##           data/raw/ctd_bongo/EN706_CTD_Data/raw/*.cnv
##
##  Output:  data/processed/ctd_bongo_data_YYYY-MM-DD.rds
##           data/processed/ctd_bongo_data.csv
##           figures/ctd_bongo_profiles_check.pdf
##           figures/ctd_bongo_profiles_labeled.pdf
###############################################################

## ------------------------------------------ ##
##  Packages               ----
## ------------------------------------------ ##
library(tidyverse)
library(here)

ctd_files <- list.files(
  here("data", "raw", "ctd_bongo"),
  recursive = TRUE,
  full.names = TRUE
)

## file inventory
tibble(path = ctd_files) %>%
  mutate(
    rel_path  = path %>%
      str_remove(fixed(here("data", "raw", "ctd_bongo"))) %>%
      str_remove("^[\\/]"),
    extension = tools::file_ext(path),
    filename  = basename(path)
  ) %>%
  arrange(rel_path) %>%
  select(rel_path, extension) %>%
  print(n = Inf)

read_ctd_bongo_cnv <- function(f) {
  lines    <- readLines(f)
  end_line <- which(grepl("\\*END\\*", lines))
  
  station_line <- lines[grepl("^\\*\\* Station:", lines)]
  start_line   <- lines[grepl("# start_time", lines)]
  
  station    <- str_extract(station_line, "(?<=Station: ).*") %>% str_trim()
  start_time <- str_extract(start_line, "(?<= = ).*(?= \\[)") %>%
    parse_date_time(orders = "b d Y HMS", tz = "UTC")
  
  fname  <- basename(f) %>% tools::file_path_sans_ext()
  cruise <- str_extract(fname, "^[A-Za-z]{2,3}\\d+") %>% toupper()
  cast   <- str_extract(fname, "(?i)B\\d+(_[bc])?") %>% toupper() %>%
    str_replace("B0*(\\d+)", "B\\1")
  
  df <- read_table(f, skip = end_line, col_names = FALSE,
                   show_col_types = FALSE) %>%
    suppressWarnings()
  
  col_names <- lines[grepl("^# name", lines)] %>%
    str_extract("(?<== )\\S+") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]", "_") %>%
    str_remove("_$")
  
  if (ncol(df) == length(col_names)) names(df) <- col_names
  
  df %>%
    mutate(cruise = cruise, station = station, cast = cast,
           file_start_time = start_time) %>%
    select(cruise, station, cast, file_start_time,
           depth_m         = depsm,
           temp_C          = tv290c,
           conductivity_sm = c0s_m,
           density_kg_m3   = density00,
           descent_rate_ms = dz_dtm,
           elapsed_s       = times,
           flag)
}

## get all CNV files to read (exclude junk, deck tests, _b/_c variants)
cnv_files <- ctd_files[grepl("\\.cnv$", ctd_files, ignore.case = TRUE)] %>%
  .[!grepl("junk|jubk|Deck_Test|TEST\\.cnv|proc/|Copy", 
           ., ignore.case = TRUE)]

ctd_cnv_data <- map_dfr(cnv_files, function(f) {
  message("  reading: ", basename(f))
  tryCatch(
    read_ctd_bongo_cnv(f),
    error = function(e) { message("  FAILED: ", basename(f), " - ", e$message); NULL }
  )
})

glimpse(ctd_cnv_data)

ctd_cnv_data %>%
  distinct(cruise, station, cast, file_start_time) %>%
  arrange(cruise, station, cast) %>%
  print(n = Inf)

tibble(file = basename(cnv_files)) %>% print(n = Inf)

ctd_cnv_data <- ctd_cnv_data %>%
  mutate(station = str_replace(station, "^[Ll]0*(\\d+)$", "L\\1"),
         station = str_to_upper(station))  # catches lowercase l cases

## verify
ctd_cnv_data %>%
  distinct(cruise, station, cast, file_start_time) %>%
  arrange(cruise, station, cast) %>%
  print(n = Inf)

