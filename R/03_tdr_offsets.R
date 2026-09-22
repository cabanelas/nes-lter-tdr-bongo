###############################################################
##  NES-LTER Bongo TDR: TDR Depth Offsets
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: cross-ref with Pxsensor + CTD to get TDR depth offsets as needed
##           For casts where TDR was attached to CTD (bench tests),
##           get CTD max depth from NES-LTER API and compare to TDR max depth 
##
##  Input:  data/processed/
##            tdr_data_no_offset_Sys.Date.RDS
##            tdr_ctd_tests.csv                (from 02_tdr_tidy.R)
##            px_data_bongo_DATE.RDS     (from 02_px_sensor_tidy.R)
##            ctd_bongo_data_DATE.RDS    (from 02_ctd_bongo_tidy.R)
##
##          data/raw/
##            tdr_offsets.csv  
##            tow-meta-v3-intermediate-HRS2609-20260918.rds
##             (previously named nes-lter-bongologs-CRUISE-YYYYMMDD.csv)
##                      from nes-lter-tow-meta-v3.Rproj; 03_bongo_logs_merge.R
##
##  NES-LTER API 2
##    https://github.com/WHOIGit/nes-lter-api-2/wiki
##    https://nes-lter-api.whoi.edu/api/docs#/
##
##  Output: data/processed/tdr_offsets_Sys.Date.csv
##          data/processed/nes-lter-bongo-tdr-offsets.csv
##          data/processed/tdr-offsets-column-headers.csv
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
basename(tdr_file)
tdr_data <- readRDS(tdr_file)

## --- TDR-CTD bench tests (from 02_tdr_tidy.R) --- ##
tdr_ctd_test <- read_csv(here("data", "processed", "tdr_ctd_tests.csv"))

## --- TDR offsets --- ##
offsets <- read_csv(here("data", "raw", "tdr_offsets.csv")) %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  filter(cast != "B25a") %>%
  mutate(station = str_replace(station, "^L0(\\d)$", "L\\1")) 
# 11 cruises
# this was created a while ago; manually based on logsheet notes

# created in nes-lter-tow-meta-v3.Rproj; 03_bongo_logs_merge.R
# formerly nes-lter-bongologs-AR99-20260811.csv
meta <- readRDS(file.path("data", "raw",
                          "tow-meta-v3-intermediate-HRS2609-20260918.rds"))

## ------------------------------------------ ##
##  1. CTD-TDR bench tests 
## ------------------------------------------ ##
# on a couple of cruises tdr attached to shipboard CTD
## cruises with CTD-TDR bench tests: 
# AT46, EN712, EN715, EN720, AR92 

## ------------------------------------------ ##
##  1a. Adjust time on tdr ctd test data
## ------------------------------------------ ##
# similar to what was done in 02_tdr_tidy.R line ~572
# tdr_ctd_tests.csv was written BEFORE clock corrections in 02_tdr_tidy.R
# re-apply the same offsets here

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
##  1b. NES-LTER API2: fill in missing metadata
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

## AT46 - find cast at L2
lookup_ctd_metadata("AT46") %>% 
  filter(nearest_station == "L2")

tdr_ctd_test %>%
  filter(cruise == "AT46", station == "L2") %>%
  summarize(start = min(date_time), end = max(date_time))
# cast 17 

## EN712 - find station and cast
lookup_ctd_metadata("EN712") %>% print(n=25)

tdr_ctd_test %>% 
  filter(cruise == "EN712") %>% 
  summarize(start = min(date_time), end = max(date_time))
# d1a cast 21

rm(lookup_ctd_metadata)

## fill metadata 
tdr_ctd_test <- tdr_ctd_test %>%
  mutate(
    # fill cast 
    cast = case_when(
      cruise == "AT46" & station == "L2" & is.na(cast) ~ 17,  
      cruise == "EN712" & is.na(cast)                  ~ 21,    
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

## ------------------------------------------ ##
##  1c. NES-LTER API2: fetch ship CTD max depth ----
## ------------------------------------------ ##

## --- get tdr max depth --- 
tdr_ctd_test_maxdepth <- tdr_ctd_test %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_max_depth_m = max(depth_m, na.rm = TRUE),
    tdr_start       = min(date_time, na.rm = TRUE),
    tdr_end         = max(date_time, na.rm = TRUE),
    .groups = "drop"
  )

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

# negative means TDR reads deeper than the bench CTD (TDR too deep)
# positive means TDR reads shallower
bench_test_offsets <- tdr_ctd_test_maxdepth %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast")) %>%
  mutate(depth_offset_m = ctd_max_depth_m - tdr_max_depth_m)

bench_test_offsets %>%
  ggplot(aes(x = ctd_max_depth_m, y = tdr_max_depth_m, label = paste(cruise, station, cast, sep = "/"))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = cruise), size = 3) +
  geom_text(nudge_y = 2, size = 3, check_overlap = TRUE) +
  labs(title = "bench test casts",
    x = "CTD max depth (m)", y = "TDR max depth (m)",
    color = "cruise"
  ) +
  theme_minimal()

## AT46 cross-check: compare bench test vs existing manual offset
# has manual (via logsheet notes) and TDR test offset
offsets %>% filter(cruise == "AT46") %>% arrange(station)
bench_test_offsets %>% filter(cruise == "AT46") %>% select(-c(tdr_start, tdr_end))

rm(tdr_ctd_test_maxdepth, BASE_URL, ctd_maxdepth)

## ------------------------------------------ ##
##  2. PX sensor data
## ------------------------------------------ ##

## --- load most recent px_data_bongo RDS (from 02_px_sensor_tidy.R)
px_file <- sort(list.files(here("data", "processed"),
                           pattern = "^px_data_bongo_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                           full.names = TRUE)) %>% tail(1)
px_data_bongo <- readRDS(px_file)
unique(px_data_bongo$cruise) # 6 cruises

## px max depth per cast 
px_maxdepth <- px_data_bongo %>%
  group_by(cruise, station, cast) %>%
  summarise(px_max_depth_m = max(depth_m, na.rm = TRUE), .groups = "drop")

# px_maxdepth <- px_maxdepth %>%
#   bind_rows(
#     tibble(
#       cruise = c("AR99", "HRS2609"),
#       station = c("L11", "L1"),
#       cast = c("B9", "B1"),
#       px_max_depth_m = c(199, 13.4)
#     )
#   )
## PX casts where sensor data was deleted (looked bad) but max depth was
## still recorded/known -> add back manually
## AR99 L11 B9:  PX sensor did not record; PX sensor logsheet max depth ~199m
## HRS2609 L1 B1: PX cast deleted as bad; max depth 13.4m
## EN727 L9 B11: PX deleted, depth written = 200m
## AR99 L2 B3:   PX deleted, depth written = 44m
## AR99 L9 B5:   PX deleted, depth written = 205m
## AR95 L6 B11:  PX deleted, depth written = 91m
px_maxdepth <- px_maxdepth %>%
  bind_rows(
    tibble(
      cruise         = c("AR99", "HRS2609", "EN727", "AR99", "AR99", "AR95"),
      station        = c("L11",  "L1",      "L9",    "L2",   "L9",   "L6"),
      cast           = c("B9",   "B1",      "B11",   "B3",   "B5",   "B11"),
      px_max_depth_m = c(199,    13.4,      200,     44,     205,    91)
    )
  )
rm(px_file)

## ------------------------------------------ ##
##  3. CTD on bongo data               ----
## ------------------------------------------ ##

## load most recent ctd_bongo RDS (from 02_ctd_bongo_tidy.R)
ctd_file <- sort(list.files(here("data", "processed"),
                            pattern = "^ctd_bongo_data_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
ctd_bongo_data <- readRDS(ctd_file)
unique(ctd_bongo_data$cruise) # 2 cruises only: EN668, EN706

## ctd bongo max depth per cast
ctd_bongo_maxdepth <- ctd_bongo_data %>%
  group_by(cruise, station, cast) %>%
  filter(note_code != "no_max_depth") %>%
  summarise(ctd_bongo_max_depth_m = max(depth_m, na.rm = TRUE), .groups = "drop")

rm(ctd_file)

## ------------------------------------------ ##
##  4. TDR max depth and surface min depth     ----
## ------------------------------------------ ##

## tdr max depth; all bongo casts
tdr_depth_summary <- tdr_data %>%
  group_by(cruise, station, cast) %>%
  summarize(
    tdr_max_depth_m = max(depth_m, na.rm = TRUE),
    tdr_min_depth_m = min(depth_m, na.rm = TRUE),
    .groups = "drop"
  )

# no max depth
# AE2426 L9 = tdr start late
tdr_depth_summary <- tdr_depth_summary %>%
  mutate(tdr_max_depth_m = if_else(
    cruise == "AE2426" & station == "L9" & cast == "B12",
    NA_real_,
    tdr_max_depth_m
  ))

## ------------------------------------------ ##
##  4a. Surface min depth check (all cruises)  ----
## ------------------------------------------ ##

## per cruise surface summary
tdr_depth_summary %>%
  group_by(cruise) %>%
  summarise(
    min    = min(tdr_min_depth_m),
    median = median(tdr_min_depth_m),
    max    = max(tdr_min_depth_m),
    n_suspicious = sum(tdr_min_depth_m > 2 | tdr_min_depth_m < -2),
    .groups = "drop"
  ) %>%
  arrange(desc(median)) %>% print(n=Inf)

tdr_depth_summary %>%
  group_by(cruise, cast) %>%
  summarise(
    min    = min(tdr_min_depth_m),
    .groups = "drop"
  ) %>%
  filter(min > 2 | min < -2) %>%
  arrange(desc(min)) %>% print(n=180)

## ------------------------------------------ ##
##  4b. Plots  ----
## ------------------------------------------ ##

all_cruises <- unique(tdr_depth_summary$cruise)

walk(all_cruises, function(cr) {
  p <- tdr_data %>%
    filter(cruise == cr) %>%
    left_join(tdr_depth_summary %>% select(cruise, station, cast, tdr_min_depth_m),
              by = c("cruise", "station", "cast")) %>%
    ggplot(aes(x = date_time_utc, y = depth_m)) +
    geom_line(linewidth = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_y_reverse(limits = c(25, -5)) +
    facet_wrap(~ paste(station, cast, paste0("(min: ", round(tdr_min_depth_m, 1), "m)")),
               scales = "free_x") +
    labs(x = NULL, y = "depth (m)",
         title = paste("TDR depth profiles —", cr)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), strip.text = element_text(size = 7))
  print(p)
})

tdr_depth_summary %>%
  ggplot(aes(x = reorder(cast, tdr_min_depth_m), y = tdr_min_depth_m, 
             color = case_when(
               tdr_min_depth_m > 2  ~ "too deep",
               tdr_min_depth_m < -2 ~ "too negative",
               TRUE                 ~ "ok"
             ))) +
  geom_point(size = 3) +
  geom_hline(yintercept =  2, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("too deep"     = "tomato", "too negative" = "orange",
               "ok"           = "steelblue"),
    name = "surface depth"
  ) +
  facet_wrap(~ cruise, scales = "free") +
  labs(x = "cast", y = "Surface min depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

## ------------------------------------------ ##
##  5a. Offsets1: TDR compared to PX or CTD depth
## ------------------------------------------ ##

## all unique cast keys across instruments + manual offsets
all_keys <- bind_rows(
  tdr_depth_summary %>% select(cruise, station, cast),
  px_maxdepth       %>% select(cruise, station, cast),
  ctd_bongo_maxdepth %>% select(cruise, station, cast),
  offsets %>% mutate(across(c(cruise, station, cast), as.character)) %>%
    select(cruise, station, cast)
) %>%
  distinct() %>%
  arrange(cruise, station, cast)

offsets1 <- all_keys %>%
  left_join(tdr_depth_summary,   by = c("cruise", "station", "cast")) %>%
  left_join(px_maxdepth,         by = c("cruise", "station", "cast")) %>%
  left_join(ctd_bongo_maxdepth,  by = c("cruise", "station", "cast")) %>%
  left_join(offsets %>%
      mutate(across(c(cruise, station, cast), as.character)) %>%
      select(cruise, station, cast, manual_offset_m = offset_m),
      by = c("cruise", "station", "cast")
  ) %>%
  mutate(
    calculated_offset_m = case_when(
      !is.na(px_max_depth_m)        ~ px_max_depth_m - tdr_max_depth_m,
      !is.na(ctd_bongo_max_depth_m) ~ ctd_bongo_max_depth_m - tdr_max_depth_m,
      TRUE                          ~ NA_real_   # handled later
    ),
    offset_source = case_when(
      !is.na(px_max_depth_m)        ~ "px_tdr",
      !is.na(ctd_bongo_max_depth_m) ~ "ctd_bongo_tdr",
      TRUE                          ~ NA_character_
    )
  )

offsets1 %>% count(offset_source)

## ------------------------------------------ ##
##  5b. Offsets2: Bench tests check
## ------------------------------------------ ##

bench_cruises <- bench_test_offsets %>%
  filter(!cruise %in% offsets$cruise) %>% # AT46 already has offsets
  pull(cruise) %>%
  unique()

bench_test_offsets %>% filter(cruise %in% bench_cruises) %>% print(width = Inf)

## --- Plot all casts for bench test cruises ---
walk(bench_cruises, function(cr) {
  bench_offset <- bench_test_offsets$depth_offset_m[bench_test_offsets$cruise == cr]
  
  label_df <- tdr_depth_summary %>%
    filter(cruise == cr) %>%
    left_join(
      tdr_data %>%
        filter(cruise == cr) %>% #, down_up == "downcast"
        group_by(cruise, station, cast) %>%
        summarise(label_time = min(date_time_utc), .groups = "drop"),
      by = c("cruise", "station", "cast")
    )
  
  p <- tdr_data %>%
    filter(cruise == cr) %>%
    left_join(tdr_depth_summary, by = c("cruise", "station", "cast")) %>%
    #filter(down_up == "downcast") %>%
    ggplot(aes(x = date_time_utc, y = depth_m)) +
    geom_line(linewidth = 0.3) +
    geom_text(
      data = label_df, aes(x = label_time, y = tdr_min_depth_m,
      label = round(tdr_min_depth_m, 1)),
      vjust = 0.5, hjust = -1, size = 4.5, color = "tomato"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    scale_y_reverse(limits = c(15, -5)) +
    facet_wrap(~ paste(station, cast), scales = "free") +
    labs(
      title = paste("Bench test — ", cr,
                    "| bench offset:", bench_offset, "m"),
      x = NULL, y = "depth (m)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), strip.text = element_text(size = 7))
  
  print(p)
})

## --- Bench tests offsets ---
# bench offsets
bench_test_offsets %>%
  filter(cruise %in% bench_cruises) %>%
  arrange(cruise) %>%
  print(n = Inf, width = Inf)

# min depth range per cruise
tdr_depth_summary %>%
  filter(cruise %in% bench_cruises) %>%
  group_by(cruise) %>%
  summarise(
    min_depth_min    = min(tdr_min_depth_m),
    min_depth_median = median(tdr_min_depth_m),
    min_depth_max    = max(tdr_min_depth_m),
    n_casts          = n(),
    .groups = "drop"
  ) %>%
  arrange(cruise) 

## --- apply offsets --- 
offsets2_decisions <- tribble(
  ~cruise, ~offset_m, ~offset_applies_to, ~notes,
  # positive offsets == TDR reading too shallow = negative depths at surface
  "AR92",  4.6,      "all_casts",  "bench test offset", #median min depth -4.61 matches bench closely",
  "EN712", 1.7,      "all_casts",  "bench test offset", #median min depth -1.62 matches bench closely",
  "EN715", 2,        "all_casts",  "bench test offset",#midpoint of bench offset (3.23) and implied offset (2.09)",
  "EN720", 2.9,      "all_casts",  "bench test offset" #midpoint of bench offset (3.09) and implied offset (2.78)"
)

## apply offsets2 decisions to offsets1 (for casts still missing offset)
offsets2 <- offsets1 %>%
  filter(is.na(calculated_offset_m), cruise %in% bench_cruises) %>%
  left_join(offsets2_decisions %>% select(cruise, offset_m, notes),
            by = "cruise") %>%
  mutate(
    calculated_offset_m = offset_m,
    offset_source       = "bench_test_cruise_wide"
  ) %>%
  select(-offset_m)

## ------------------------------------------ ##
##  5c. Offsets3: Surface min depth
## ------------------------------------------ ##
## Casts with no instrument reference AND suspicious min depth

## suspicious surface cruises
suspicious_cruises <- tdr_depth_summary %>%
  group_by(cruise) %>%
  summarise(
    median_min = median(tdr_min_depth_m),
    n_suspicious = sum(tdr_min_depth_m > 2 | tdr_min_depth_m < -2),
    .groups = "drop"
  ) %>%
  filter(median_min > 2 | median_min < -2 | n_suspicious >= 3) %>% 
  pull(cruise)

suspicious_casts <- tdr_depth_summary %>%
  filter(tdr_min_depth_m > 2 | tdr_min_depth_m < -2, 
         cruise %in% suspicious_cruises) %>%
  select(cruise, station, cast)

offsets3_candidates <- offsets1 %>%
  filter(
    is.na(calculated_offset_m),
    !cruise %in% bench_cruises,
    paste(cruise, station, cast) %in%
      paste(suspicious_casts$cruise, suspicious_casts$station, suspicious_casts$cast)
  ) 
  #select(cruise, station, cast, tdr_min_depth_m)

print(offsets3_candidates, n = Inf)

walk(unique(offsets3_candidates$cruise), function(cr) {
  candidates_cr <- offsets3_candidates %>% filter(cruise == cr)
  
  label_df <- tdr_depth_summary %>%
    filter(cruise == cr,
           paste(station, cast) %in% 
             paste(candidates_cr$station, candidates_cr$cast)) %>%
    left_join(
      tdr_data %>%
        filter(cruise == cr, down_up == "downcast") %>%
        group_by(cruise, station, cast) %>%
        summarise(label_time = min(date_time_utc), .groups = "drop"),
      by = c("cruise", "station", "cast")
    )
  
  p <- tdr_data %>%
    filter(cruise == cr,
           paste(station, cast) %in% 
             paste(candidates_cr$station, candidates_cr$cast),
           down_up == "downcast") %>%
    ggplot(aes(x = date_time_utc, y = depth_m)) +
    geom_line(linewidth = 1.3) +
    geom_text(
      data = label_df,
      aes(x = label_time, y = tdr_min_depth_m,
          label = round(tdr_min_depth_m, 1)),
      vjust = -0.5, hjust = -0.8, size = 3.8, color = "tomato"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    scale_y_reverse(limits = c(20, -8)) +
    facet_wrap(~ paste(station, cast), scales = "free_x") +
    labs(
      title = paste(cr),
      x = NULL, y = "depth (m)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), strip.text = element_text(size = 7))
  
  print(p)
})

offsets3 <- offsets3_candidates %>%
  mutate(
    calculated_offset_m = -tdr_min_depth_m,
    offset_source       = "surface_min_to_zero"
  )

## ------------------------------------------ ##
##  Merge 
## ------------------------------------------ ##

## casts with no reference and clean surface=offset = 0
offsets_clean <- offsets1 %>%
  filter(
    is.na(calculated_offset_m),
    !cruise %in% bench_cruises,
    !paste(cruise, station, cast) %in%
      paste(suspicious_casts$cruise, suspicious_casts$station, suspicious_casts$cast)
  ) %>%
  mutate(
    calculated_offset_m = 0,
    offset_source       = "no_ref_clean_surface"
  )

offsets_draft <- bind_rows(
  offsets1 %>% filter(!is.na(calculated_offset_m)), # PX + CTD bongo 
  offsets2,                                         # bench test cruise-wide
  offsets3,                                         # surface min depth
  offsets_clean                                     # clean surface, no offset
) %>%
  ## add bench_offset_m as reference column
  left_join(
    bench_test_offsets %>% select(cruise, bench_offset_m = depth_offset_m),
    by = "cruise"
  ) %>%
  mutate(
    notes = case_when(
      !is.na(notes) ~ notes,  
      !is.na(manual_offset_m) & manual_offset_m != calculated_offset_m ~
        paste0("manual=", manual_offset_m,
               " vs calculated=", round(calculated_offset_m, 2)),
      !is.na(bench_offset_m) &
        !is.na(calculated_offset_m) &
        abs(bench_offset_m - calculated_offset_m) > 1 ~
        paste0("bench_test=", bench_offset_m,
               " vs calculated=", round(calculated_offset_m, 2)),
      TRUE ~ NA_character_
    )
  ) %>%
  select(cruise, station, cast,
         calculated_offset_m, offset_source, notes,
         bench_offset_m, manual_offset_m,
         tdr_max_depth_m, px_max_depth_m,
         ctd_bongo_max_depth_m, tdr_min_depth_m) %>%
  arrange(cruise, station, cast)

offsets_draft <- offsets_draft %>%
  mutate(net_prefix = str_extract(cast, "^[BR]"),
         cast_num = str_remove(cast, "^[BR]")) %>%
  left_join(
    meta %>%
      distinct(cruise, station, cast, net_type, depth_bottom, depth_target) %>%
      mutate(net_prefix = if_else(net_type == "ring", "R", "B")),
    by = c("cruise", "station", "cast_num" = "cast", "net_prefix")
  ) %>%
  select(-cast_num, -net_prefix, -net_type)

## quick summaries
offsets_draft %>% count(offset_source)

rm(offsets1, offsets2, offsets3)

## ------------------------------------------ ##
# negative means TDR reads deeper than the bench CTD (TDR too deep)
# positive means TDR reads shallower
## ------------------------------------------ ##

# 2018 = EN608   = -1.3m offset
# 2018 = EN617   = -1m offset
##
# 2019 = EN627   = -2m
# 2019 = EN644   = -4m ; except at L4, L5, MVCO == 0
##
# 2020 = EN649   = GOOD = 0m
# 2020 = EN655   = -5m L2-L5; other stations == 0  
# 2020 = EN657   = GOOD = 0m
##
# 2021 = :(
##
# 2022 = AT46    = -10m at L1; other stations == -4m; exept L8 == 0 
#        AT46 L8 no tdr data delete
# 2022 = EN687   = GOOD = 0m
##
# 2023 = HRS2303 = -5m all; delete one with NAs (L2)
# 2023 = EN706   = -10.5; L4 & L5 gets -19
# 2023 = AR77    = GOOD = 0m 
##
# 2024 = EN712   = 1.7m; bench test offset; (surf -1.1 to -2)
# 2024 = EN715   = 2m; bench test offset; (surf -1.6 to -2.5)
# CTD depth == 135.7 m; TDR depth == 132.7 m, offset == 3 m. 
# 2024 = EN720   = 2.9m; bench test offset; (surf -2.5 to -2.9)
# CTD depth == 201   m; TDR depth == 197.91 m, offset == 3.09 m. TDR used during this cruise SN: C11871. 
# 2024 = AE2426  = 3.3m; (surf -3.1 to -3.5)
##
# 2025 = EN727   = 4m  
# 2025 = AR88    = 4.3m (surf -3.9 to -4.5)
# 2025 = AR92    = 4.6m; bench test offset; (surf -4.4 to -4.9)
# 2025 = AR95    = 5.3m (surf -5.1 to -5.3)
##
# 2026 = AR99    = 5.9m (surf -5.6 to -6)
# 2026 = HRS2601 = 6m
# 2026 = HRS2909 = 6.5m

## ------------------------------------------ ##
##  TDR offsets comments in meta
## ------------------------------------------ ##
# check bongo logsheet comments 
check_comments_meta <- meta %>%
  filter(grepl("TDR|tdr|offset", comments, ignore.case = TRUE)) %>%
  select(cruise, station, cast, depth_bottom, depth_target, comments) %>%
  arrange(cruise, station)
# already applied; had notes about offsets in metadata
# AT46; EN644; EN655; EN727 L1; AR88 MVCO

## ------------------------------------------ ##
##  OFFSETS ---
## ------------------------------------------ ##
# check AR95 L3B19; AR92 L1B1; AR99 L3B18; AR99 L4B17 ; AR88 L7B16; EN655 L1; EN644 L1, L2 

cast_overrides <- tribble(
  ~cruise,    ~station, ~cast,  ~offset_m,
  # AT46 — L1 == -10; L8 has no TDR data (drop)
  "AT46",    "L1",   "B1",   -10.0,
  "AT46",    "L8",   "B13",   NA, # no TDR data for this station
  # EN644 — L4, L5, MVCO == 0
  "EN644",   "L4",   "B23",   0,
  "EN644",   "L5",   "B22",   0,
  "EN644",   "MVCO", "B29",   0,
  # EN655 — L2-L5 == -5m; rest == 0
  "EN655",   "L2",   "B2",   -5.0,
  "EN655",   "L3",   "B3",   -5.0,
  "EN655",   "L4",   "B7",   -5.0,
  "EN655",   "L5",   "B10",  -5.0,
  # EN706 — L4/L5 get -19; rest get -10.5 from cruise_offsets
  "EN706",   "L4",   "B5",  -19.0,
  "EN706",   "L5",   "B6",  -19.0,
  # HRS2303 — L2 has no TDR data (drop)
  "HRS2303", "L2",   "B12",   NA,
  # HRS2601 L7 B10 has no TDR data (drop)
  "HRS2601", "L7", "B10", NA
)

cruise_offsets <- tribble(
  ~cruise,    ~offset_m,
  "EN608",    -1.3,
  "EN617",    -1.0,
  "EN627",    -2.0,
  "EN644",    -4.0,  # except L4, L5, MVCO == 0m offset
  "EN649",     0,
  "EN655",     0,    # L2-L5 overridden to -5 via cast_overrides
  "EN657",     0,
  "AT46",     -4.0,  # L1 overridden to -10; L8 dropped
  "EN687",     0,
  "HRS2303",  -5.0,  # L2 dropped
  "EN706",   -10.5,  # L4/L5 overridden to -19
  "AR77",      0,
  "EN712",     1.7,
  "EN715",     2.0,
  "EN720",     2.9,
  "AE2426",    3.3,
  "EN727",     4.0,
  "AR88",      4.3,
  "AR92",      4.6,
  "AR95",      5.3,
  "AR99",      5.9,
  "HRS2601",   6,
  "HRS2609",   6.5
)

# offsets_final <- offsets_draft %>%
#   left_join(cruise_offsets, by = "cruise") %>%
#   left_join(
#     cast_overrides %>% rename(offset_override = offset_m),
#     by = c("cruise", "station", "cast")
#   ) %>%
#   mutate(offset_m = coalesce(offset_override, offset_m)) %>%
#   select(-offset_override) %>%
#   filter(!is.na(offset_m)) %>%   # drops AT46 L8, HRS2303 L2
#   select(cruise, station, cast,
#          offset_m,
#          tdr_max_depth_m, px_max_depth_m,
#          ctd_bongo_max_depth_m, tdr_min_depth_m,
#          depth_bottom, depth_target) %>%
#   arrange(cruise, station, cast)

offsets_final <- offsets_draft %>%
  left_join(cruise_offsets, by = "cruise") %>%
  left_join(
    cast_overrides %>%
      rename(offset_override = offset_m) %>%
      mutate(has_override = TRUE),
    by = c("cruise", "station", "cast")
  ) %>%
  mutate(offset_m = if_else(!is.na(has_override), 
                            offset_override, offset_m)) %>%
  select(-offset_override, -has_override) %>%
  filter(!is.na(offset_m)) %>% # drops AT46 L8, HRS2303 L2
  select(cruise, station, cast,
         offset_m,
         tdr_max_depth_m, px_max_depth_m,
         ctd_bongo_max_depth_m, tdr_min_depth_m,
         depth_bottom, depth_target) %>%
  arrange(cruise, station, cast)

## ------------------------------------------ ##
##  Check which cruises missing offsets
## ------------------------------------------ ##
# check cruise-cast-station 
all_cruises <- tdr_data %>% distinct(cruise) %>% pull(cruise)
cruises_with_offsets <- offsets_final %>% distinct(cruise) %>% pull(cruise)

missing_offsets <- setdiff(all_cruises, cruises_with_offsets)
cat("Cruises in tdr_data with NO offsets:\n")
print(missing_offsets)

print(sort(cruises_with_offsets))

## check every tdr cast has an offset
tdr_all_casts <- tdr_data %>% distinct(cruise, station, cast)
tdr_with_offsets <- offsets_final %>% distinct(cruise, station, cast)

missing_offsets <- anti_join(tdr_all_casts, tdr_with_offsets, 
                             by = c("cruise", "station", "cast"))

cat("TDR casts with NO offset:\n")
print(missing_offsets, n = Inf)

## ------------------------------------------ ##
##  Visual QC: cruises with offset > 0      ----
## ------------------------------------------ ##
offsets_final %>%
  group_by(cruise) %>%
  summarise(max_offset = max(abs(offset_m)), .groups = "drop") %>%
  filter(max_offset > 0) %>%
  arrange(desc(max_offset)) %>%
  pull(cruise)

offsets_final %>%
  mutate(corrected_max_depth_m = tdr_max_depth_m + offset_m) %>%
  ggplot(aes(x = depth_target, y = corrected_max_depth_m, color = cruise)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Corrected TDR max depth vs target depth",
       x = "target depth (m)", y = "corrected TDR max depth (m)") +
  theme_minimal()

offsets_final %>%
  distinct(cruise, offset_m) %>%
  mutate(cruise = fct_reorder(cruise, offset_m)) %>%
  ggplot(aes(x = cruise, y = offset_m, fill = offset_m > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("too deep", "too shallow"), name = "direction") +
  labs(title = "TDR offset by cruise", x = NULL, y = "offset (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tdr_serials_cruise <- tdr_data %>%
  count(cruise, serial_number) %>%
  group_by(cruise) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(cruise, serial_number)

offsets_final %>%
  distinct(cruise, offset_m) %>%
  left_join(tdr_serials_cruise, by = "cruise") %>%
  left_join(tdr_data %>% group_by(cruise) %>% 
              summarize(start_date = min(date_time_utc)), 
            by = "cruise") %>%
  mutate(cruise = fct_reorder(cruise, start_date)) %>%
  ggplot(aes(x = cruise, y = offset_m, fill = serial_number)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "TDR offset by cruise", 
       x = NULL, y = "offset (m)", fill = "TDR serial") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

offsets_final %>%
  mutate(corrected_min = tdr_min_depth_m + offset_m) %>%
  ggplot(aes(x = tdr_min_depth_m, y = corrected_min, color = cruise)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Surface min depth: before vs after offset correction",
       x = "raw TDR min depth (m)", y = "corrected min depth (m)") +
  theme_minimal()

cast_times <- tdr_data %>%
  group_by(cruise, station, cast) %>%
  summarize(cast_start = min(date_time_utc), .groups = "drop")

raw_check <- offsets_final %>%
  left_join(tdr_serials_cruise, by = "cruise") %>%
  left_join(cast_times, by = c("cruise", "station", "cast")) %>%
  arrange(cast_start)

raw_check %>%
  filter(serial_number == "11871") %>%
  ggplot(aes(x = cast_start, y = tdr_min_depth_m)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Raw TDR minimum recorded depth over time (serial 11871)",
       x = NULL, y = "min depth recorded (m)") +
  theme_minimal()

raw_check %>%
  ggplot(aes(x = cast_start, y = tdr_min_depth_m, color = serial_number)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Raw TDR minimum recorded depth over time",
       x = NULL, y = "min depth recorded (m)", 
       color = "TDR serial", fill = "TDR serial") +
  theme_minimal()

## ------------------------------------------ ##
##  Finalize col names and order  ----
## ------------------------------------------ ##
offsets_final <- offsets_final %>%
  select(cruise, station, cast,
         tdr_max_depth_m, px_max_depth_m, ctd_bongo_max_depth_m,
         offset_m)

## ------------------------------------------ ##
##  Export offsets                   ----
## ------------------------------------------ ##

out_file <- here("data", "processed",
                 paste0("tdr_offsets_", Sys.Date(), ".csv"))
write_csv(offsets_final, out_file)
basename(out_file)
write_csv(offsets_final, here("data", "processed", "nes-lter-bongo-tdr-offsets.csv"))

# save file with colnames
tibble(column = names(offsets_final)) %>%
  write_csv(here("data", "processed", "tdr-offsets-column-headers.csv"))

################################################################################
# go to -----------> 04_instrument_coverage.R
################################################################################