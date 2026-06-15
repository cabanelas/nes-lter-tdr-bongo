###############################################################
##  NES-LTER Bongo TDR: TDR-CTD Offset Analysis
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: cross-ref with Pxsensor + CTD to get TDR depth offsets as needed
##           For casts where TDR was attached to CTD (bench tests),
##           get CTD max depth from NES-LTER API and compare to TDR max depth 
##
##  Input:  data/processed/tdr_data_no_offset_DATE.RDS
##          data/processed/tdr_ctd_tests.csv        (from 02_tdr_tidy.R)
##          data/raw/tdr_offsets.csv  
##          data/raw/all-nes-lter-bongologs-20260526.csv
##               from nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
##          data/processed/px_data_bongo_DATE.RDS   (from 02_px_sensor_tidy.R)
##          data/processed/ctd_bongo_data_DATE.RDS  (from 02_ctd_bongo_tidy.R)
##  NES-LTER API 2
##    https://github.com/WHOIGit/nes-lter-api-2/wiki
##    https://nes-lter-api.whoi.edu/api/docs#/
##
##  Output: data/processed/bench_test_offsets.csv
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
# this was created a while ago

# created in nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
meta <- read_csv(file.path("data", "raw",
                           "all-nes-lter-bongologs-20260526.csv"))

## ------------------------------------------ ##
##  1. CTD-TDR bench tests 
## ------------------------------------------ ##
# on a couple of cruises tdr attached to shipboard CTD

## ------------------------------------------ ##
##  1a. Adjust time on tdr ctd test data
## ------------------------------------------ ##
# similar to what was done in 02_tdr_tidy.R line ~507
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

bench_test_offsets <- tdr_ctd_test_maxdepth %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast")) %>%
  mutate(depth_offset_m = ctd_max_depth_m - tdr_max_depth_m)

bench_test_offsets %>%
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

## AT46 cross-check: compare bench test vs existing manual offset
# has manual (via logsheet notes) and TDR test offset
offsets %>% filter(cruise == "AT46") %>% arrange(station)
bench_test_offsets %>% filter(cruise == "AT46") %>% select(-c(tdr_start, tdr_end))

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
  #mutate(cast = str_remove(cast, "^B")) 

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
  arrange(desc(median))

tdr_depth_summary %>%
  group_by(cruise, cast) %>%
  summarise(
    min    = min(tdr_min_depth_m),
    .groups = "drop"
  ) %>%
  filter(min > 2 | min < -2) %>%
  arrange(desc(min)) %>% print(n=180)

## plot suspicious cruises
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

walk(suspicious_cruises, function(cr) {
  p <- tdr_data %>%
    filter(cruise == cr) %>%
    left_join(tdr_depth_summary %>% select(cruise, station, cast, tdr_min_depth_m),
              by = c("cruise", "station", "cast")) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
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

other_cruises <- unique(tdr_data$cruise[!tdr_data$cruise %in% suspicious_cruises])

walk(other_cruises, function(cr) {
  p <- tdr_data %>%
    filter(cruise == cr) %>%
    left_join(tdr_depth_summary %>% select(cruise, station, cast, tdr_min_depth_m),
              by = c("cruise", "station", "cast")) %>%
    ggplot(aes(x = date_time, y = depth_m)) +
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
  #filter(cruise %in% suspicious_cruises) %>%        
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
    values = c("too deep"     = "tomato",
               "too negative" = "orange",
               "ok"           = "steelblue"),
    name = "surface depth"
  ) +
  facet_wrap(~ cruise, scales = "free_x") +
  labs(x = "cast", y = "Surface min depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

## ------------------------------------------ ##
##  Offsets1: TDR compared to PX or CTD depth
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
##  Offsets2: Bench tests check
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
        summarise(label_time = min(date_time), .groups = "drop"),
      by = c("cruise", "station", "cast")
    )
  
  p <- tdr_data %>%
    filter(cruise == cr) %>%
    left_join(tdr_depth_summary, by = c("cruise", "station", "cast")) %>%
    filter(down_up == "downcast") %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 0.3) +
    geom_text(
      data = label_df, aes(x = label_time, y = tdr_min_depth_m,
      label = round(tdr_min_depth_m, 1)),
      vjust = 0.5, hjust = -1, size = 4.5, color = "tomato"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    scale_y_reverse(limits = c(25, -5)) +
    facet_wrap(~ paste(station, cast), scales = "free") +
    labs(
      title = paste("Bench test — downcast —", cr,
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
  ~cruise,  ~offset_m, ~offset_applies_to, ~notes,
  "AR92",   4.57,      "all_casts",        "bench test offset; median min depth -4.61 matches bench closely",
  "EN712",  1.94,      "all_casts",        "bench test offset; median min depth -1.62 matches bench closely",
  "EN715",  2,         "all_casts",        "midpoint of bench offset (3.23) and implied offset (2.09); no PX data available",
  "EN720",  2.9,      "all_casts",        "midpoint of bench offset (3.09) and implied offset (2.78); no PX data available"
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
##  Offsets3: Surface min depth
## ------------------------------------------ ##
## Casts with no instrument reference AND suspicious min depth

offsets3_candidates <- offsets1 %>%
  filter(
    is.na(calculated_offset_m),
    !cruise %in% bench_cruises,
    paste(cruise, station, cast) %in%
      paste(suspicious_casts$cruise, suspicious_casts$station, suspicious_casts$cast)
  ) %>%
  select(cruise, station, cast, tdr_min_depth_m)

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
        summarise(label_time = min(date_time), .groups = "drop"),
      by = c("cruise", "station", "cast")
    )
  
  p <- tdr_data %>%
    filter(cruise == cr,
           paste(station, cast) %in% 
             paste(candidates_cr$station, candidates_cr$cast),
           down_up == "downcast") %>%
    ggplot(aes(x = date_time, y = depth_m)) +
    geom_line(linewidth = 1.3) +
    geom_text(
      data = label_df,
      aes(x = label_time, y = tdr_min_depth_m,
          label = round(tdr_min_depth_m, 1)),
      vjust = -0.5, hjust = -0.8, size = 3.8, color = "tomato"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    scale_y_reverse(limits = c(20, -5)) +
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
# 2018 = EN608   = 1.3m offset
# 2018 = EN617   = 1m offset
##
# 2019 = EN627   = 1.5m
# 2019 = EN644   = 4m = not equal; 2 tdr need to check
##
# 2020 = EN649   = GOOD = 0m
# 2020 = EN655   = L2-L5 = 6m based on logsheet
# 2020 = EN657   = GOOD = 0m
##
# 2021 = :(
##
# 2022 = AT46    = 
# L1 = 10 
# for rest of stations just bring min depth to 0
# L11 , L9, L7, 4m??
# 2022 = EN687   = GOOD = 0m
##
# 2023 = HRS2303 = ~5 maybe bring down to 0
# 2023 = EN706   = ctd avail need offset
# 2023 = AR77    = GOOD = 0m 
##
# 2024 = EN712   = -1.1 to -2
# 2024 = EN715   = -1.6 to -2.5
# The depth of the CTD was 135.7 m and the depth of the TDR was 132.7 m, so the offset was 3 m. 
# 2024 = EN720   = -2.5 to -2.9
# The maximum TDR depth while mounted on the rosette was 197.91 m while the CTD (Seabird reading) depth was 201 m so the TDR had an offset of 3.09 m. The TDR used during this cruise was SN: C11871. 
# 2024 = AE2426  = px sensor? -3.1 to -3.5
##
# 2025 = EN727   =  The TDR used during this cruise was SN: C11871. 
# 2025 = AR88    = surf -3.9 to -4.5
# 2025 = AR92    = surf -4.4 to -4.9
# 2025 = AR95    = surf -5.1 to -5.3
##
# 2026 = AR99    = -5.6 to -6
# 2026 = ***need to add HRS2601***

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
  offsets1 %>% filter(!is.na(calculated_offset_m)),  # PX + CTD bongo (automated)
  offsets2,                                           # bench test cruise-wide
  offsets3,                                           # surface min depth
  offsets_clean                                       # clean surface, no ref
) %>%
  ## add bench_offset_m as reference column
  left_join(
    bench_test_offsets %>% select(cruise, bench_offset_m = depth_offset_m),
    by = "cruise"
  ) %>%
  mutate(
    needs_review = case_when(
      offset_source == "surface_min"                                      ~ TRUE,
      offset_source == "bench_test_cruise_wide" & grepl("PENDING", notes) ~ TRUE,
      !is.na(bench_offset_m) &
        !is.na(calculated_offset_m) &
        abs(bench_offset_m - calculated_offset_m) > 1                    ~ TRUE,
      !is.na(manual_offset_m) &
        !is.na(calculated_offset_m) &
        manual_offset_m != calculated_offset_m                           ~ TRUE,
      TRUE                                                                ~ FALSE
    ),
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
         calculated_offset_m, offset_source,
         needs_review, notes,
         bench_offset_m, manual_offset_m,
         tdr_max_depth_m, px_max_depth_m,
         ctd_bongo_max_depth_m, tdr_min_depth_m) %>%
  arrange(cruise, station, cast)

## quick summaries
offsets_draft %>% count(offset_source, needs_review)

offsets_draft %>%
  filter(needs_review) %>%
  select(cruise, station, cast, calculated_offset_m, offset_source, notes) %>%
  print(n = Inf)

## manual vs calculated comparison
offsets_draft %>%
  filter(!is.na(manual_offset_m)) %>%
  select(cruise, station, cast,
         calculated_offset_m, manual_offset_m,
         offset_source, notes) %>%
  print(n = Inf)

## ------------------------------------------ ##
##  MANUAL CHECKS ---- HERE
## ------------------------------------------ ##

# 2025 = AR92 = use bench test val instead of px_tdr calculated 
# AT46 L8 no tdr data delete from offsets_draft 
# AT46 L1 maybe use surface val seems deeper than rest of cruise 
# the rest seem good
# wondering about EN644 L1, L2 and EN655 L1 

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

## ------------------------------------------ ##
##  HERE!!* 
## ------------------------------------------ ##
# which have 2 depths; any with 3 or more???? 
# check against logsheets? 

## ------------------------------------------ ##
##  Export offsets                   ----
## ------------------------------------------ ##

out_file <- here("data", "processed",
                 paste0("tdr_offsets_draft_", Sys.Date(), ".csv"))
write_csv(offsets_draft, out_file)
message("Written: ", basename(out_file))

################################################################################
# go to -----------> 04.R
################################################################################