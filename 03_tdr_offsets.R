###############################################################
##  NES-LTER Bongo TDR: TDR-CTD Offset Analysis
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: cross-ref with Pxsensor + CTD to get TDR depth offsets as needed
## For casts where TDR was attached to CTD (bench tests),
##           pull CTD max depth from NES-LTER API and compare to TDR max depth 
##
##  Input:  data/tdr_data_no_offset_DATE.RDS
## data/processed/tdr_ctd_tests.csv (from 02_tdr_tidy.R)
##          data/raw/tdr_offsets.csv  
##          data/raw/all-nes-lter-bongologs-20260526.csv
##  NES-LTER API 2
##    https://github.com/WHOIGit/nes-lter-api-2/wiki
##    https://nes-lter-api.whoi.edu/api/docs#/
##
##  Output: data/processed/tdr_ctd_offset.csv
###############################################################

## cruises with CTD-TDR tests: 
# 2022 = AT46   L2   17    
# 2024 = EN712  d1a  21    
# 2024 = EN715  L8   14   
# 2024 = EN720  L9   19   
# 2025 = AR92   u9a  18   

library(tidyverse)
library(here)

source(here("R", "00_helpers.R"))

## ------------------------------------------ ##
##  Input files                            ----
## ------------------------------------------ ##

## --- most recent tdr_data RDS (from 02_tdr_tidy.R) --- ##
tdr_file <- sort(list.files(here("data", "processed"),
                            pattern = "^tdr_data_no_offset_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
message("Reading: ", basename(tdr_file))
tdr_data <- readRDS(tdr_file)

## --- TDR-CTD bench tests (from 02_tdr_tidy.R) --- ##
tdr_ctd_test <- read_csv(here("data", "processed", "tdr_ctd_tests.csv"))

## --- TDR offsets --- ##
offsets <- read_csv(here("data", "raw", "tdr_offsets.csv")) %>%
  mutate(across(c(cruise, station, cast), as.character)) #11 cruises
# this was created a while ago

# created in nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
meta <- read_csv(file.path("data", "raw",
                           "all-nes-lter-bongologs-20260526.csv"))

## ------------------------------------------ ##
##  1. Compare with CTD-TDR tests 
## ------------------------------------------ ##
# on a couple of cruises tdr attached to shipboard CTD

## ------------------------------------------ ##
##  Adjust time on tdr ctd test data
## ------------------------------------------ ##
# similar to what was done in 02_tdr_tidy.R line ~507
# tdr_ctd_tests.csv was written BEFORE clock corrections in 02_tdr_tidy.R
# must re-apply the same offsets here

clock_offsets <- tribble(
  ~cruise,   ~offset_hrs,
  "EN712",   5,
  "EN715",   4,
  "EN720",   4
)

tdr_ctd_test <- tdr_ctd_test %>%
  left_join(clock_offsets, by = "cruise") %>%
  mutate(
    date_time = case_when(
      !is.na(offset_hrs) ~ date_time + hours(offset_hrs),
      TRUE ~ date_time
    )
  ) %>%
  select(-offset_hrs)

rm(clock_offsets)

## ------------------------------------------ ##
##  NES-LTER API2: Lookup missing metadata
## ------------------------------------------ ##
# 2 casts in the tdr ctd test df are missing meta
## manually identify missing station/cast for incomplete rows
tdr_ctd_test %>%
  distinct(cruise, station, cast, comments) %>%
  arrange(cruise, station, cast) 
# AT46 L2 (missing cast)
# EN712   (missing station + cast)

lookup_ctd_metadata <- function(cruise) {
  url <- paste0("https://nes-lter-api.whoi.edu/api/ctd/metadata/", 
                tolower(cruise), ".csv")
  message("Fetching: ", url)
  read_csv(url, show_col_types = FALSE)
}

## AT46 - find which casts are at L2
lookup_ctd_metadata("AT46") %>% 
  filter(nearest_station == "L2")

tdr_ctd_test %>%
  filter(cruise == "AT46", station == "L2") %>%
  summarise(start = min(date_time), end = max(date_time))
# cast 17 

## EN712 
lookup_ctd_metadata("EN712") %>% print(n=25)

tdr_ctd_test %>% 
  filter(cruise == "EN712") %>% 
  summarise(start = min(date_time), end = max(date_time))
# d1a cast 21

rm(lookup_ctd_metadata)

## ------------------------------------------ ##
##  Manual metadata fixes                  ----
## ------------------------------------------ ##

tdr_ctd_test <- tdr_ctd_test %>%
  mutate(
    # fill cast 
    cast = case_when(
      cruise == "AT46" & station == "L2" & is.na(cast) ~ "17",  
      cruise == "EN712" & is.na(cast)                  ~ "21",    
      TRUE ~ cast
    ),
    # fill station
    station = case_when(
      cruise == "EN712" & is.na(station) ~ "d1a",  
      TRUE ~ station
    )
  )

tdr_ctd_test %>%
  distinct(cruise, station, cast, comments) %>%
  arrange(cruise, station, cast)

## --- plot tdr-ctd test casts --- 
ggplot(tdr_ctd_test, aes(x = date_time, y = depth_m)) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~paste(cruise, station, cast), scales = "free") +
  labs(x = NULL, y = "depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

## --- get tdr max depth --- 
tdr_ctd_test_maxdepth <- tdr_ctd_test %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_max_depth_m = max(depth_m, na.rm = TRUE),
    tdr_start       = min(date_time, na.rm = TRUE),
    tdr_end         = max(date_time, na.rm = TRUE),
    .groups = "drop"
  )

## ------------------------------------------ ##
##  NES-LTER API2: CTD data                ----
## ------------------------------------------ ##

BASE_URL <- "https://nes-lter-api.whoi.edu/api"

ctd_maxdepth <- tdr_ctd_test_maxdepth %>%
  distinct(cruise, cast) %>%
  pmap_dfr(function(cruise, cast) {
    cr       <- tolower(cruise)
    ctd_cast <- str_remove(cast, "^B")   # strip B prefix for API
    url      <- paste0(BASE_URL, "/ctd/cast/", cr, "/", ctd_cast, ".csv")
    message("  fetching: ", url)
    df <- safe_read_csv(url) # this function is in 00_helpers.R
    if (is.null(df)) return(NULL)
    df %>%
      summarise(ctd_max_depth_m = max(depsm, na.rm = TRUE)) %>%
      mutate(cruise = toupper(cruise), cast = cast)  # keep original cast (with B) for joining back
  })

tdr_ctd_offset <- tdr_ctd_test_maxdepth %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast")) %>%
  mutate(depth_offset_m = ctd_max_depth_m - tdr_max_depth_m)

tdr_ctd_offset %>%
  ggplot(aes(x = ctd_max_depth_m, y = tdr_max_depth_m, label = paste(cruise, station, cast, sep = "/"))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = cruise), size = 3) +
  geom_text(nudge_y = 2, size = 3, check_overlap = TRUE) +
  labs(
    title = "bench test casts",
    x = "CTD max depth (m)", y = "TDR max depth (m)",
    color = "cruise"
  ) +
  theme_minimal()

## ------------------------------------------ ##
##  Add meta context to offset check        ----
## ------------------------------------------ ##
# adding depth_bottom and depth_target from bongo logsheets
meta_context <- meta %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  filter(cruise %in% tdr_ctd_offset$cruise) %>%
  distinct(cruise, station, cast, depth_bottom, depth_target)

tdr_ctd_offset <- tdr_ctd_offset %>%
  mutate(cast_stripped = str_remove(cast, "^B")) %>%
  left_join(meta_context, by = c("cruise", "station", "cast_stripped" = "cast")) %>%
  select(-cast_stripped)

rm(meta_context)

## ------------------------------------------ ##
##  AT46 cross-reference with manual offsets ----
## ------------------------------------------ ##
# has manual (via logsheet notes) and TDR test offset
offsets %>%
  filter(cruise == "AT46") %>%
  arrange(offset_m, station) 
## compare to what CTD bench test gives us
tdr_ctd_offset %>%
  filter(cruise == "AT46") %>%
  select(cruise, station, cast, tdr_max_depth_m, ctd_max_depth_m, 
         depth_offset_m) 

## ------------------------------------------ ##
##  Expand cruise-wide offsets to all casts ----
## ------------------------------------------ ##
## for cruises NOT in manual offsets: AR92, EN712, EN715, EN720
## use the bench test depth_offset_m for all casts on that cruise
new_offsets <- tdr_ctd_offset %>%
  filter(!cruise %in% offsets$cruise) %>%  # exclude AT46 - already in offsets
  select(cruise, depth_offset_m) %>%
  mutate(depth_offset_m = round(depth_offset_m)) %>%
  left_join(
    tdr_data %>% distinct(cruise, station, cast),
    by = "cruise"
  ) %>%
  rename(offset_m = depth_offset_m)

## combine with existing manual offsets
offsets_combined <- bind_rows(offsets, new_offsets) %>%
  arrange(cruise, station, cast)

rm(new_offsets)

## ------------------------------------------ ##
##  2. PXsensor offsets
## ------------------------------------------ ##

## load most recent px_data_bongo RDS (from 02_px_sensor_tidy.R)
px_file <- sort(list.files(here("data", "processed"),
                           pattern = "^px_data_bongo_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                           full.names = TRUE)) %>% tail(1)
px_data_bongo <- readRDS(px_file)

unique(px_data_bongo$cruise) # 6 cruises

## px max depth per cast 
px_maxdepth <- px_data_bongo %>%
  #filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  summarise(px_max_depth_m = max(depth_m, na.rm = TRUE), .groups = "drop")

## add cruises without existing offsets, but with PX sensor data
## cruises in px data but not yet in offsets_combined
px_only_cruises <- px_maxdepth %>%
  filter(!cruise %in% offsets_combined$cruise) %>%
  select(cruise, station, cast) %>%
  mutate(offset_m = NA_real_)

# offsets combined has manual offsets file + ones with ctd-tdr tests
## add px max depth data to offsets_combined to compare
offsets_combined <- bind_rows(offsets_combined, px_only_cruises) %>%
  arrange(cruise, station, cast)

## add px_max_depth_m for everyone
offsets_combined <- offsets_combined %>%
  left_join(px_maxdepth, by = c("cruise", "station", "cast"))

## ------------------------------------------ ##
##  CTD on bongo                           ----
## ------------------------------------------ ##

## load most recent ctd_bongo RDS (from 02_ctd_bongo_tidy.R)
ctd_file <- sort(list.files(here("data", "processed"),
                            pattern = "^ctd_bongo_data_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
message("Reading: ", basename(ctd_file))
ctd_bongo_data <- readRDS(ctd_file)

unique(ctd_bongo_data$cruise) # EN668, EN706

## ctd bongo max depth per cast
ctd_bongo_maxdepth <- ctd_bongo_data %>%
  filter(down_up == "downcast") %>%
  group_by(cruise, station, cast) %>%
  summarise(ctd_bongo_max_depth_m = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  mutate(cast = str_remove(cast, "^B"))  # strip B to match other cast formats if needed

## ------------------------------------------ ##
##  Build master depth comparison df        ----
## ------------------------------------------ ##

## tdr max depth; all bongo casts
tdr_maxdepth_all <- tdr_data %>%
  group_by(cruise, station, cast) %>%
  summarise(tdr_max_depth_m = max(depth_m, na.rm = TRUE), .groups = "drop")

## ctd bench test max depths (tdr attached to ship CTD rosette)
# comparing the CTD depth to the TDR depth
ctd_test_maxdepth <- tdr_ctd_offset %>%
  select(cruise, station, cast,
         tdr_benchtest_max_depth_m = tdr_max_depth_m,
         shipctd_benchtest_max_depth_m = ctd_max_depth_m)

## manual offsets from file
manual_offsets <- offsets %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  select(cruise, station, cast, manual_offset_m = offset_m)

## all unique keys across instruments
all_keys <- bind_rows(
  tdr_maxdepth_all    %>% select(cruise, station, cast),
  px_maxdepth         %>% select(cruise, station, cast),
  ctd_bongo_maxdepth  %>% select(cruise, station, cast),
  manual_offsets      %>% select(cruise, station, cast)
) %>%
  distinct() %>%
  arrange(cruise, station, cast)

depth_comparison <- all_keys %>%
  left_join(tdr_maxdepth_all,    by = c("cruise", "station", "cast")) %>%
  left_join(px_maxdepth,         by = c("cruise", "station", "cast")) %>%
  left_join(ctd_bongo_maxdepth,  by = c("cruise", "station", "cast")) %>%
  left_join(ctd_test_maxdepth,   by = c("cruise", "station", "cast")) %>%
  left_join(manual_offsets,      by = c("cruise", "station", "cast"))

glimpse(depth_comparison)

## which instruments have data per cruise
depth_comparison %>%
  group_by(cruise) %>%
  summarise(
    n_casts              = n(),
    has_tdr              = any(!is.na(tdr_max_depth_m)),
    has_px               = any(!is.na(px_max_depth_m)),
    has_ctd_bongo        = any(!is.na(ctd_bongo_max_depth_m)),
    has_ctd_benchtest    = any(!is.na(shipctd_benchtest_max_depth_m)),
    has_manual_offset    = any(!is.na(manual_offset_m)),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# which have 2 depths; any with 3 or more???? 
# check against logsheets? 

## ------------------------------------------ ##
##  Surface min depth check (all cruises)  ----
## ------------------------------------------ ##

tdr_surface <- tdr_data %>%
  group_by(cruise, station, cast) %>%
  summarise(min_depth_m = min(depth_m, na.rm = TRUE), .groups = "drop")

## per cruise surface summary
tdr_surface %>%
  group_by(cruise) %>%
  summarise(
    min    = min(min_depth_m),
    median = median(min_depth_m),
    max    = max(min_depth_m),
    n_above_2m = sum(min_depth_m > 2),
    .groups = "drop"
  ) %>%
  arrange(desc(median))

tdr_surface %>%
  group_by(cruise, cast) %>%
  summarise(
    min    = min(min_depth_m),
    n_above_2m = sum(min_depth_m > 2),
    .groups = "drop"
  ) %>%
  filter(min > 2) %>%
  arrange(desc(min)) %>% print(n=100)

## plot suspicious cruises
suspicious_cruises <- tdr_surface %>%
  group_by(cruise) %>%
  summarise(
    median_min = median(min_depth_m),
    n_above_2m = sum(min_depth_m > 2),
    .groups = "drop"
  ) %>%
  filter(median_min > 2 | n_above_2m >= 3) %>% 
  pull(cruise)

tdr_data %>%
  filter(cruise %in% suspicious_cruises) %>%
  ggplot(aes(x = date_time, y = depth_m)) +
  geom_line(linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_reverse(limits = c(25, 0)) +
  facet_wrap(~ paste(cruise, station, cast), scales = "free_x") +
  labs(x = NULL, y = "depth (m)",
       title = "TDR depth profiles — suspicious surface values (0–25m)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), strip.text = element_text(size = 7))

tdr_surface %>%
  filter(cruise %in% suspicious_cruises) %>%
  ggplot(aes(x = reorder(cast, min_depth_m), y = min_depth_m, color = min_depth_m > 2)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
                     labels = c("TRUE" = ">2m", "FALSE" = "≤2m"),
                     name = "surface depth") +
  facet_wrap(~ cruise, scales = "free_x") +
  labs(x = "cast", y = "min depth (m)",
       title = "Surface entry depth by cast — suspicious cruises") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

## ------------------------------------------ ##
##  HERE!!* 
## ------------------------------------------ ##

## ------------------------------------------ ##
##  Build final offsets df                 ----
## ------------------------------------------ ##
## bench test offsets for reference
# compared against calculated_offset_m downstream
bench_test_offsets <- tdr_ctd_offset %>%
  select(cruise, bench_offset_m = depth_offset_m) %>%
  mutate(bench_offset_m = round(bench_offset_m))

print(bench_test_offsets)

## add min_depth to depth_comparison
depth_comparison <- depth_comparison %>%
  left_join(tdr_surface, by = c("cruise", "station", "cast"))

## Calculate TDR depth offset (calculated_offset_m) for each cast
##   1. PX sensor vs TDR      - per cast
##   2. CTD on bongo vs TDR   - per cast
##   3. Surface min depth     - for casts flagged as suspicious (min_depth > 2m)
##                               and no instrument reference available
##   4. 0                     - no reference, surface looks clean
##
## bench_offset_m (from CTD-TDR bench tests) is retained as a reference
## column only — not applied directly, but used to flag disagreements
## (|bench - calculated| > 1m → needs_review = TRUE)
##
## manual_offset_m (from tdr_offsets.csv) is also retained for comparison;
## disagreements flagged in notes for manual review
##
## needs_review = TRUE when:
##   - offset derived from surface min depth (less reliable)
##   - bench test value disagrees with calculated offset by >1m
##   - manual offset exists and differs from calculated

offsets_draft <- depth_comparison %>%
  left_join(bench_test_offsets, by = "cruise") %>%
  mutate(
    is_suspicious = paste(cruise, station, cast) %in%
      paste(suspicious_casts$cruise, suspicious_casts$station, suspicious_casts$cast),
    calculated_offset_m = case_when(
      # 1. PX vs TDR per cast
      !is.na(px_max_depth_m)        ~ px_max_depth_m - tdr_max_depth_m,
      # 2. CTD bongo vs TDR per cast
      !is.na(ctd_bongo_max_depth_m) ~ ctd_bongo_max_depth_m - tdr_max_depth_m,
      # 3. surface min depth for suspicious casts
      is_suspicious                 ~ min_depth_m,
      # 4. clean surface, no reference needed
      TRUE                          ~ 0
    ),
    offset_source = case_when(
      !is.na(px_max_depth_m)        ~ "px_tdr",
      !is.na(ctd_bongo_max_depth_m) ~ "ctd_bongo_tdr",
      is_suspicious                 ~ "surface_min",
      TRUE                          ~ "no_ref_clean_surface"
    ),
    needs_review = case_when(
      offset_source == "surface_min"                             ~ TRUE,
      !is.na(bench_offset_m) &
        abs(bench_offset_m - calculated_offset_m) > 1           ~ TRUE,
      !is.na(manual_offset_m) &
        manual_offset_m != calculated_offset_m                  ~ TRUE,
      TRUE                                                       ~ FALSE
    ),
    notes = case_when(
      !is.na(manual_offset_m) & manual_offset_m != calculated_offset_m ~
        paste0("manual=", manual_offset_m,
               " vs calculated=", round(calculated_offset_m, 2)),
      !is.na(bench_offset_m) &
        abs(bench_offset_m - calculated_offset_m) > 1           ~
        paste0("bench_test=", bench_offset_m,
               " vs calculated=", round(calculated_offset_m, 2)),
      offset_source == "surface_min"                            ~
        "offset from surface min depth; verify against logsheets",
      TRUE                                                      ~ NA_character_
    )
  ) %>%
  select(cruise, station, cast,
         calculated_offset_m, offset_source,
         needs_review, notes,
         bench_offset_m,
         manual_offset_m,
         tdr_max_depth_m, px_max_depth_m,
         ctd_bongo_max_depth_m, min_depth_m) %>%
  arrange(cruise, station, cast)

## quick summaries
offsets_draft %>% count(offset_source, needs_review)

## duplicates: manual vs calculated
offsets_draft %>%
  filter(!is.na(manual_offset_m)) %>%
  select(cruise, station, cast,
         calculated_offset_m, manual_offset_m,
         offset_source, notes) %>%
  print(n = Inf)

## ------------------------------------------ ##
##  Export draft offsets                   ----
## ------------------------------------------ ##

out_file <- here("data", "processed",
                 paste0("tdr_offsets_draft_", Sys.Date(), ".csv"))
write_csv(offsets_draft, out_file)
message("Written: ", basename(out_file))








## ------------------------------------------ ##
##  TDR offsets comments in meta
## ------------------------------------------ ##
# check bongo logsheet comments 
check_comments_meta <- meta %>%
  filter(grepl("TDR|tdr|offset", comments, ignore.case = TRUE)) %>%
  select(cruise, station, cast, depth_bottom, depth_target, comments) %>%
  arrange(cruise, station)
## based on comments, check: AR88 MVCO; EN727 L1 TDR check for offset 

# already applied; had notes about offsets in metadata
# AT46; EN644; EN655
# EN715 TDR was tested on CTD cast. CTD = 135.7m vs TDR = 132.7m = 3m offset on TDR readings

## ------------------------------------------ ##
##  Check which cruises missing offsets
## ------------------------------------------ ##
all_cruises <- tdr_data %>% distinct(cruise) %>% pull(cruise)
cruises_with_offsets <- offsets_combined %>% distinct(cruise) %>% pull(cruise)

missing_offsets <- setdiff(all_cruises, cruises_with_offsets)
cat("Cruises in tdr_data with NO offsets:\n")
print(missing_offsets)

print(sort(cruises_with_offsets))

## ------------------------------------------ ##
##  Visual QC: cruises with offset > 0      ----
## ------------------------------------------ ##
cruises_nonzero <- offsets_combined %>%
  group_by(cruise) %>%
  summarise(max_offset = max(abs(offset_m)), .groups = "drop") %>%
  filter(max_offset > 0) %>%
  arrange(desc(max_offset)) %>%
  pull(cruise)

cat("\nCruises with offset > 0 (to visually check):\n")
print(cruises_nonzero)

## PRINT MIN DEPTH ON EACH CAST ON PLOT
## plot depth profiles for each, colored by offset magnitude
tdr_data %>%
  filter(cruise %in% cruises_nonzero) %>%
  left_join(offsets_combined, by = c("cruise", "station", "cast")) %>%
  filter(down_up == "downcast") %>%
  ggplot(aes(x = date_time, y = depth_m, color = offset_m)) +
  geom_line(linewidth = 0.3) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", na.value = "gray70") +
  facet_wrap(~cruise, scales = "free_x") +
  labs(title = "TDR downcast depth profiles — cruises with nonzero offset",
       x = NULL, y = "depth (m)", color = "offset (m)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# output
# file name thruCRUISE_date
################################################################################
# go to -----------> 04.R
################################################################################