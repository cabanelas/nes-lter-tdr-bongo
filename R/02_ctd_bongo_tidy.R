###############################################################
##  NES-LTER Bongo CTD: Tidy & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  02_ctd_bongo_tidy.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Read raw SeaBird SBE19plus V2 CNV files from bongo-attached
##           CTD deployments, assign cruise/station/cast metadata, tidy,
##           label downcast/upcast, export cleaned CTD-bongo data
##
##  Cruises: EN668 (2021), EN706 (2023)
##
##  Input:  data/raw/ctd_bongo/SBE19plus_EN668/*.cnv
##          data/raw/ctd_bongo/EN706_CTD_Data/raw/*.cnv
##          data/raw/all-nes-lter-bongologs-20260526.csv
##                  from nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
##
##  Output: data/processed/ctd_bongo_data_YYYY-MM-DD.rds
##          data/processed/ctd_bongo_data.csv
##          figures/ctd_bongo_profiles_raw_check.pdf
##          figures/ctd_bongo_profiles_labeled.pdf
##          figures/ctd_bongo_profiles_final.pdf
###############################################################

# CTD data available for: EN668 (no TDR) and EN706 (also has TDR)
# CTD sampling interval is uniformly 0.25 sec (4 Hz) across all casts and cruises
# typo: EN668 L9 B14 has files labeled as B15. B14 is correct. fixed here

## ------------------------------------------ ##
##  Packages               ----
## ------------------------------------------ ##
library(tidyverse)
library(here)
library(oce) # optional for TS plot at end

source(here("R", "00_helpers.R"))

## ------------------------------------------ ##
##  Files available         ----
## ------------------------------------------ ##
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

# variables available
lines <- readLines(here("data/raw/ctd_bongo/EN706_CTD_Data/raw/EN706_L01_B01.cnv"))
lines[grepl("^# name", lines)]

## ------------------------------------------ ##
##  Read data        ----
## ------------------------------------------ ##
read_ctd_bongo_cnv <- function(f) {
  # Read all lines of the CNV file (header + data)
  lines    <- readLines(f)
  # *END* marks where the header ends and data begins
  end_line <- which(grepl("\\*END\\*", lines))
  
  # Pull the header lines containing station name and instrument start time
  station_line <- lines[grepl("^\\*\\* Station:", lines)]
  start_line   <- lines[grepl("# start_time", lines)]
  
  # extract station and start time 
  station    <- str_extract(station_line, "(?<=Station: ).*") %>% str_trim()
  start_time <- str_extract(start_line, "(?<= = ).*(?= \\[)") %>%
    parse_date_time(orders = "b d Y HMS", tz = "UTC")
  
  # create cruise column from filename prefix 
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
           depth_m         = depsm, # depth in meters (derived from pressure)
           temp_C          = tv290c, # temperature (C, ITS-90)
           conductivity_sm = c0s_m, # conductivity (S/m)
           density_kg_m3   = density00, # seawater density (kg/m^3)
           descent_rate_ms = dz_dtm, # descent rate (m/s)
           elapsed_s       = times, # elapsed seconds since start
           flag)
}

## read CNV files (exclude junk and deck tests)
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

## ------------------------------------------ ##
##  Check data          ----
## ------------------------------------------ ##

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

rm(cnv_files, ctd_files)

## ------------------------------------------ ##
##  Add time         ----
## ------------------------------------------ ##
ctd_cnv_data <- ctd_cnv_data %>%
  mutate(date_time = file_start_time + seconds(elapsed_s)) %>%
  relocate(date_time, .after = file_start_time)

## ------------------------------------------ ##
##  Check stations         ----
## ------------------------------------------ ##
# created in nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
meta <- read_csv(file.path("data", "raw",
                           "all-nes-lter-bongologs-20260526.csv")) %>%
  filter(cruise %in% c("EN668", "EN706")) %>%
  mutate(cast_b = paste0("B", cast))  # 1 -> B1 to match CTD format

ctd_ids <- ctd_cnv_data %>%
  filter(!grepl("_B$|_C$", cast)) %>%
  distinct(cruise, station, cast)

# in CTD but not in meta
anti_join(ctd_ids, meta, by = c("cruise", "station", "cast" = "cast_b"))
# in meta but not in CTD
anti_join(meta, ctd_ids, by = c("cruise", "station", "cast_b" = "cast"))

ctd_cnv_data <- ctd_cnv_data %>%
  mutate(station = str_replace(station, "^[Ll]0*(\\d+)$", "L\\1"),
         station = str_to_upper(station)) %>%
  # en668L04B05.cnv has "L05" in header but is actually L4 — fix station
  mutate(station = case_when(
    cruise == "EN668" & station == "L5" &
      file_start_time == as.POSIXct("2021-07-17 13:54:16", tz = "UTC") ~ "L4",
    TRUE ~ station
  )) %>%
  # fix cast number errors confirmed against logsheet
  mutate(cast = case_when(
    cruise == "EN706" & station == "L4" & cast == "B4"  ~ "B5",
    cruise == "EN668" & station == "L9" & cast == "B15" ~ "B14",
    TRUE ~ cast
  ))

# cross check with times 
ctd_cnv_data %>%
  distinct(cruise, station, cast, file_start_time) %>%
  left_join(meta %>% select(cruise, station, cast_b, datetime_UTC_start),
            by = c("cruise", "station", "cast" = "cast_b")) %>%
  mutate(diff_min = as.numeric(difftime(file_start_time, datetime_UTC_start, units = "mins"))) %>%
  arrange(cruise, station) %>% print(n=40)

rm(ctd_ids)

## ------------------------------------------ ##
##  Plot         ----
## ------------------------------------------ ##
# --- raw CTD depth profile plots per cruise ----
pdf(here("figures", "ctd_bongo_profiles_raw_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(ctd_cnv_data$cruise))) {
  p <- ctd_cnv_data %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.3, color = "steelblue", alpha = 0.7) +
    scale_y_reverse() +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("RAW CTD profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          strip.text   = element_text(size = 6))
  print(p)
}

dev.off()
rm(p, cr)

## ------------------------------------------ ##
##  Label downcast / upcast        ----
## ------------------------------------------ ##
# function in 00_helpers.R
ctd_cnv_data <- ctd_cnv_data %>%
  group_by(cruise, station, cast) %>%
  do(label_down_up(.)) %>%
  ungroup()

ctd_cnv_data %>%
  count(down_up)

## ------------------------------------------ ##
##  Post-label CTD profile plots      ----
## ------------------------------------------ ##

pdf(here("figures", "ctd_bongo_profiles_labeled.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(ctd_cnv_data$cruise))) {
  p <- ctd_cnv_data %>%
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
    labs(title = paste("Labeled CTD profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x      = element_blank(),
          strip.text       = element_text(size = 6),
          legend.position  = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()
rm(p, cr)

## EN706 L1B1 = bad data = delete
## EN706 L11B9 = bad data = delete
##      is it possible to patch L11 B9_B (downcast) and L11 B9_C (upcast)
## EN706 L5B6 = started recording mid downcast (~40m)
## EN706 L10B10 = started recording mid downcast (~40m)
## EN706 L7B14 = started recording mid downcast (~25m)
## EN706 L8B15 = stopped recording on upcast right after max depth
## EN706 L3B20 = started recording mid downcast (~12m)
# trim end time for EN706 L6 B19 left ctd running while on deck 09:39

## EN668 L1B1 = started recording mid downcast (~8m)

## ------------------------------------------ ##
##  Manual fixes      ----
## ------------------------------------------ ##
# --- delete bad casts ---
ctd_cnv_data <- ctd_cnv_data %>%
  filter(!(cruise == "EN706" & station == "L1"  & cast == "B1"),
         !(cruise == "EN706" & station == "L11" & cast == "B9"))

# --- trim EN706 L6 B19: CTD left running on deck after recovery ---
# logsheet end time ~09:39, trim anything after
ctd_cnv_data <- ctd_cnv_data %>%
  filter(!(cruise == "EN706" & station == "L6" & cast == "B19" &
             date_time > as.POSIXct("2023-08-11 09:39:00", tz = "UTC")))

# --- inspect B9_B and B9_C before patching ---
ctd_cnv_data %>%
  filter(cruise == "EN706", station == "L11", cast %in% c("B9_B", "B9_C")) %>%
  mutate(label = paste(station, cast)) %>%
  ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
  geom_point(size = 0.8, alpha = 0.7) +
  scale_y_reverse() +
  facet_wrap(~label, scales = "free") +
  theme_minimal() 

b9b_end <- ctd_cnv_data %>%
  filter(cruise == "EN706", station == "L11", cast == "B9_B") %>%
  summarise(end = max(date_time)) %>% pull(end)

b9c_start <- ctd_cnv_data %>%
  filter(cruise == "EN706", station == "L11", cast == "B9_C") %>%
  summarise(start = min(date_time)) %>% pull(start)

cat("Gap:", as.numeric(difftime(b9c_start, b9b_end, units = "secs")), "seconds\n")
# missing about 3 mins of data
# logsheet comments say ctd y tubing got disconned midcast
# i will patch these and fill in with NAs
# generate the missing timestamps at 0.25s intervals across the gap
gap_times <- seq(b9b_end + 0.25, b9c_start - 0.25, by = 0.25)

gap_df <- tibble(
  cruise          = "EN706",
  station         = "L11",
  cast            = "B9",
  file_start_time = NA_POSIXct_,
  date_time       = gap_times,
  depth_m         = NA_real_,
  temp_C          = NA_real_,
  conductivity_sm = NA_real_,
  density_kg_m3   = NA_real_,
  descent_rate_ms = NA_real_,
  elapsed_s       = NA_real_,
  flag            = NA_real_,
  down_up         = "downcast"  # gap is at max depth turnaround
)

# stitch B9_B + gap + B9_C into B9
ctd_cnv_data <- ctd_cnv_data %>%
  mutate(cast = case_when(
    cruise == "EN706" & station == "L11" & cast %in% c("B9_B", "B9_C") ~ "B9",
    TRUE ~ cast
  )) %>%
  bind_rows(gap_df) %>%
  arrange(cruise, station, cast, date_time)

ctd_cnv_data %>%
  filter(cruise == "EN706", station == "L11", cast == "B9") %>%
  ggplot(aes(x = date_time, y = depth_m, color = down_up)) +
  geom_point(size = 0.8, alpha = 0.7) +
  scale_y_reverse() +
  theme_minimal()

# remove negative depths
ctd_cnv_data <- ctd_cnv_data %>%
  filter(is.na(depth_m) | depth_m >= 0)

## ------------------------------------------ ##
##  Plot final casts    ----
## ------------------------------------------ ##

pdf(here("figures", "ctd_bongo_profiles_final.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(ctd_cnv_data$cruise))) {
  p <- ctd_cnv_data %>%
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
    labs(title = paste("Labeled CTD profiles —", cr),
         x = NULL, y = "Depth (m)") +
    theme_minimal() +
    theme(axis.text.x      = element_blank(),
          strip.text       = element_text(size = 6),
          legend.position  = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  print(p)
}

dev.off()
rm(p, cr)

## ------------------------------------------ ##
##  Add notes column      ----
## ------------------------------------------ ##
## EN706 L11B9 = patched L11 B9_B (downcast) and L11 B9_C (upcast)
##                max depth is not available for this cast due to ctd turning off
##               logsheet comments say ctd y tubing got disconned midcast
## EN706 L5B6 = started recording mid downcast (~40m)
## EN706 L10B10 = started recording mid downcast (~40m)
## EN706 L7B14 = started recording mid downcast (~25m)
## EN706 L8B15 = stopped recording on upcast right after max depth
## EN706 L3B20 = started recording mid downcast (~12m)
## EN668 L1B1 = started recording mid downcast (~8m)

ctd_cast_notes <- tribble(
  ~cruise,  ~station, ~cast,  ~note_code,       ~note_detail,
  # --- stitched cast ---
  ## EN706 L11B9 = patched L11 B9_B (downcast) and L11 B9_C (upcast)
  ##               max depth is not available for this cast due to ctd turning off
  ##               logsheet comments say ctd y tubing got disconned midcast
  "EN706",  "L11",    "B9",   "patched_cast",   "Patched from B9_B downcast and B9_C upcast; CTD turned off mid-cast ~193 sec gap near max depth; no max depth available; logsheet notes Ytubing disconnected mid-cast",
  
  # --- incomplete profiles: started mid-downcast ---
  "EN706",  "L5",     "B6",   "ctd_late_start", "CTD started recording mid-downcast ~40 m",
  "EN706",  "L10",    "B10",  "ctd_late_start", "CTD started recording mid-downcast ~40 m",
  "EN706",  "L7",     "B14",  "ctd_late_start", "CTD started recording mid-downcast ~25 m",
  "EN706",  "L3",     "B20",  "ctd_late_start", "CTD started recording mid-downcast ~12 m",
  "EN668",  "L1",     "B1",   "ctd_late_start", "CTD started recording mid-downcast ~8 m",
  
  # --- incomplete profiles: stopped early ---
  "EN706",  "L8",     "B15",  "ctd_early_end",  "CTD stopped recording on upcast immediately after max depth",
  
  # --- metadata fixes: original CNV header/filename errors ---
  "EN668",  "L4",     "B5",   "header_typo_corrected","CNV header had station L05; corrected to L4 based on filename and logsheet",
  "EN706",  "L4",     "B5",   "filename_typo_corrected","CNV filename had cast B04; corrected to B5 based on logsheet",
  "EN668",  "L9",     "B14",  "filename_typo_corrected","CNV filename had cast B15; corrected to B14 based on logsheet"
)

ctd_cnv_data <- ctd_cnv_data %>%
  left_join(ctd_cast_notes, by = c("cruise", "station", "cast"))

## ------------------------------------------ ##
##  QC / Validation checks           ----
## ------------------------------------------ ##
## ------------------------------------------ ##
##  a. Naming consistency checks   ----
## ------------------------------------------ ##
# all should return 0 rows

# cruise: uppercase alphanumeric
ctd_cnv_data %>%
  filter(!grepl("^[A-Z]{2,3}[0-9]+[A-Z]?$", cruise)) %>%
  distinct(cruise)

# station: L + integer or MVCO
ctd_cnv_data %>%
  filter(!grepl("^L[0-9]+$", station), station != "MVCO") %>%
  distinct(cruise, station)

# cast: B + integer (allow B9_B, B9_C variants)
ctd_cnv_data %>%
  filter(!grepl("^B[0-9]+(_[BC])?$", cast)) %>%
  distinct(cruise, station, cast)

# down_up: only valid labels
ctd_cnv_data %>%
  filter(!down_up %in% c("predeploy", "downcast", "upcast")) %>%
  distinct(down_up)

## ------------------------------------------ ##
##  b. Physical range checks      ----
## ------------------------------------------ ##
ctd_cnv_data %>%
  summarise(
    n_neg_depth      = sum(depth_m < 0,   na.rm = TRUE),
    n_deep           = sum(depth_m > 300,  na.rm = TRUE),
    n_temp_low       = sum(temp_C < -2,    na.rm = TRUE),
    n_temp_high      = sum(temp_C > 30,    na.rm = TRUE),
    n_cond_negative  = sum(conductivity_sm < 0, na.rm = TRUE),
    n_na_depth       = sum(is.na(depth_m)),
    n_na_temp        = sum(is.na(temp_C)),
    n_na_time        = sum(is.na(date_time))
  )

## ------------------------------------------ ##
##  c. Cast-level checks          ----
## ------------------------------------------ ##
cast_qc <- ctd_cnv_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    n_obs         = n(),
    t_start       = min(date_time, na.rm = TRUE),
    t_end         = max(date_time, na.rm = TRUE),
    duration_min  = as.numeric(difftime(max(date_time, na.rm = TRUE),
                                        min(date_time, na.rm = TRUE), units = "mins")),
    max_depth_m   = max(depth_m,   na.rm = TRUE),
    temp_min_C    = min(temp_C,    na.rm = TRUE),
    temp_max_C    = max(temp_C,    na.rm = TRUE),
    temp_range_C  = temp_max_C - temp_min_C,
    n_temp_na     = sum(is.na(temp_C)),
    n_time_reversal = sum(diff(as.numeric(date_time)) < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    flag_too_short     = duration_min < 5,
    flag_too_long      = duration_min > 120,
    flag_shallow       = max_depth_m < 15,
    flag_temp_suspect  = temp_range_C > 15,
    flag_time_reversal = n_time_reversal > 0,
    flag_few_obs       = n_obs < 20
  )

# flag summary
cast_qc %>%
  summarise(across(starts_with("flag_"), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "flag", values_to = "n_casts_flagged") %>%
  filter(n_casts_flagged > 0) %>%
  arrange(desc(n_casts_flagged))

# inspect flagged casts
cast_qc %>%
  filter(if_any(starts_with("flag_"), ~.)) %>%
  select(cruise, station, cast, duration_min, max_depth_m,
         temp_range_C, n_time_reversal, n_obs, starts_with("flag_")) %>%
  arrange(cruise, station) %>%
  print(n = Inf, width = Inf)

## ------------------------------------------ ##
##  Plots       ----
## ------------------------------------------ ##

## near-surface temp by month
ctd_cnv_data %>%
  filter(down_up == "downcast", depth_m < 5) %>%
  mutate(month = lubridate::month(date_time)) %>%
  ggplot(aes(x = factor(month), y = temp_C, fill = factor(month))) +
  geom_violin(alpha = 0.6, quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.4, color = "grey30") +
  scale_x_discrete(labels = month.abb) +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "CTD near-surface temperature by month (depth < 5m, downcast)",
       x = NULL, y = "Temp (°C)") +
  theme_minimal()

## near-bottom temp by month
ctd_cnv_data %>%
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
  labs(title = "CTD near-bottom temperature by month (within 5m of max depth, downcast)",
       x = "Month", y = "Temp (°C)", color = "Cruise") +
  theme_minimal()

## large consecutive temp jumps
temp_jumps_ctd <- ctd_cnv_data %>%
  arrange(cruise, station, cast, date_time) %>%
  group_by(cruise, station, cast) %>%
  mutate(temp_diff = abs(temp_C - dplyr::lag(temp_C))) %>%
  filter(!is.na(temp_diff), temp_diff > 5) %>%
  select(cruise, station, cast, date_time, temp_C, temp_diff, depth_m) %>%
  ungroup()

message("Large consecutive temp jumps: ", nrow(temp_jumps_ctd))
print(temp_jumps_ctd, n = 30)
rm(temp_jumps_ctd)

## ------------------------------------------ ##
##  12d. Duplicate timestamp check   ----
## ------------------------------------------ ##
dup_times <- ctd_cnv_data %>%
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
rm(dup_times)

## ------------------------------------------ ##
##  12e. Downcast/upcast balance check  ----
## ------------------------------------------ ##
known_incomplete <- c("EN706_L8_B15")   # stopped right after max depth

cast_coverage <- ctd_cnv_data %>%
  group_by(cruise, station, cast) %>%
  summarise(
    has_downcast = any(down_up == "downcast"),
    has_upcast   = any(down_up == "upcast"),
    .groups = "drop"
  ) %>%
  mutate(cast_id = paste(cruise, station, cast, sep = "_")) %>%
  filter(!cast_id %in% known_incomplete) %>%
  filter(!has_downcast | !has_upcast)

if (nrow(cast_coverage) > 0) {
  message("Casts missing downcast or upcast label:")
  print(cast_coverage)
} else {
  message("All casts have both downcast and upcast labels.")
}

rm(known_incomplete, cast_coverage, cast_qc)

## ------------------------------------------ ##
##  TS plot  ----
## ------------------------------------------ ##
#library(oce)

ctd_cnv_data <- ctd_cnv_data %>%
  mutate(
    salinity = swSCTp(conductivity_sm, temp_C, 
                      pressure = depth_m * 0.1, 
                      conductivityUnit = "S/m")
  )

# T-S diagram colored by cruise
ctd_cnv_data %>%
  filter(down_up == "downcast", !is.na(salinity),
         salinity > 28) %>%   # removes clearly erroneous near-zero values
  mutate(station = factor(station, levels = c(paste0("L", 1:11), "MVCO"))) %>%
  ggplot(aes(x = salinity, y = temp_C, color = station)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_viridis_d(name = "Station") +
  facet_wrap(~cruise, scales = "free_y") +  # free y only, share x axis
  labs(title = "T-S diagram by cruise and station (downcast only)",
       x = "Salinity (PSU)", y = "Temperature (°C)") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3)))

# temp-depth profiles by station colored by cruise
ctd_cnv_data %>%
  filter(down_up == "downcast", !is.na(temp_C)) %>%
  ggplot(aes(x = temp_C, y = depth_m, color = cruise)) +
  geom_point(size = 0.3, alpha = 0.4) +
  scale_y_reverse() +
  facet_wrap(~station) +
  labs(title = "Temperature profiles by station (downcast only)",
       x = "Temperature (°C)", y = "Depth (m)") +
  theme_minimal()

# conductivity sanity
ctd_cnv_data %>%
  filter(down_up == "downcast") %>%
  ggplot(aes(x = conductivity_sm, y = depth_m, color = cruise)) +
  geom_point(size = 0.3, alpha = 0.4) +
  scale_y_reverse() +
  labs(title = "Conductivity profiles (downcast only)",
       x = "Conductivity (S/m)", y = "Depth (m)") +
  theme_minimal()

ctd_cnv_data <- ctd_cnv_data %>%
  select(-salinity)

## ------------------------------------------ ##
##  Save output              ----
## ------------------------------------------ ##
saveRDS(ctd_cnv_data,
        here("data", "processed",
             paste0("ctd_bongo_data_", Sys.Date(), ".rds")))

write_csv(ctd_cnv_data,
          here("data", "processed", "ctd_bongo_data.csv"))

tibble(column = names(ctd_cnv_data)) %>%
  write_csv(here("data", "processed", "ctd-column-headers.csv"))

################################################################################
# go to -----------> 03_tdr_offsets.R
#           OR     > 02_px_sensor_tidy.R
#           OR     > 02_ctd_bongo_tidy.R 
################################################################################