###############################################################
##  NES-LTER Bongo TDR: TDR-CTD Offset Analysis
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: cross-ref with elog + CTD, apply depth offsets as needed
## For casts where TDR was attached to CTD (bench tests),
##           pull CTD max depth from NES-LTER API and compare to
##           TDR max depth to identify any depth offsets needed.
##  Input:  data/processed/tdr_ctd_tests.csv (from 02_tdr_tidy.R)
##          data/raw/tdr_offsets.csv  
##          data/raw/all-nes-lter-bongologs-20260526.csv
##  NES-LTER API 2
##    https://github.com/WHOIGit/nes-lter-api-2/wiki
##    https://nes-lter-api.whoi.edu/api/docs#/
##
##  Output: data/processed/tdr_ctd_offset_check.csv
###############################################################

library(tidyverse)
library(here)

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

# created in nes-lter-tow-meta-v3.Rproj; 01_merge_bongo_logs.R
meta <- read_csv(file.path("data", "raw",
                           "all-nes-lter-bongologs-20260526.csv"))

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

## ------------------------------------------ ##
##  NES-LTER API2: Lookup missing metadata
## ------------------------------------------ ##
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

tdr_maxdepth <- tdr_ctd_test %>%
  group_by(cruise, station, cast) %>%
  summarise(
    tdr_max_depth_m = max(depth_m, na.rm = TRUE),
    tdr_start       = min(date_time, na.rm = TRUE),
    tdr_end         = max(date_time, na.rm = TRUE),
    .groups = "drop"
  )

## ------------------------------------------ ##
##  NES-LTER API: CTD data                ----
## ------------------------------------------ ##

BASE_URL <- "https://nes-lter-api.whoi.edu/api"

# not sure if i should keep here or in helpers.R
safe_read_csv <- function(url) {
  tryCatch(
    read_csv(url, show_col_types = FALSE),
    error = function(e) { message("  FAILED: ", url); NULL }
  )
}

ctd_maxdepth <- tdr_maxdepth %>%
  distinct(cruise, cast) %>%
  pmap_dfr(function(cruise, cast) {
    cr       <- tolower(cruise)
    ctd_cast <- str_remove(cast, "^B")   # strip B prefix for API
    url      <- paste0(BASE_URL, "/ctd/cast/", cr, "/", ctd_cast, ".csv")
    message("  fetching: ", url)
    df <- safe_read_csv(url)
    if (is.null(df)) return(NULL)
    df %>%
      summarise(ctd_max_depth_m = max(depsm, na.rm = TRUE)) %>%
      mutate(cruise = toupper(cruise), cast = cast)  # keep original cast (with B) for joining back
  })

tdr_ctd_offset_check <- tdr_maxdepth %>%
  left_join(ctd_maxdepth, by = c("cruise", "cast")) %>%
  mutate(depth_offset_m = ctd_max_depth_m - tdr_max_depth_m)

## ------------------------------------------ ##
##  Add meta context to offset check        ----
## ------------------------------------------ ##
meta_context <- meta %>%
  mutate(across(c(cruise, station, cast), as.character)) %>%
  filter(cruise %in% tdr_ctd_offset_check$cruise) %>%
  distinct(cruise, station, cast, depth_bottom, depth_target)

tdr_ctd_offset_check <- tdr_ctd_offset_check %>%
  mutate(cast_stripped = str_remove(cast, "^B")) %>%
  left_join(meta_context, by = c("cruise", "station", "cast_stripped" = "cast")) %>%
  select(-cast_stripped)

## ------------------------------------------ ##
##  AT46 cross-reference with manual offsets ----
## ------------------------------------------ ##
offsets %>%
  filter(cruise == "AT46") %>%
  arrange(offset_m, station) 
## compare to what CTD bench test gives us
tdr_ctd_offset_check %>%
  filter(cruise == "AT46") %>%
  select(cruise, station, cast, tdr_max_depth_m, ctd_max_depth_m, 
         depth_offset_m) 

## ------------------------------------------ ##
##  Expand cruise-wide offsets to all casts ----
## ------------------------------------------ ##
## for cruises NOT in manual offsets: AR92, EN712, EN715, EN720
## use the bench test depth_offset_m for all casts on that cruise
new_offsets <- tdr_ctd_offset_check %>%
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


## ------------------------------------------ ##
##  PXsensor offsets
## ------------------------------------------ ##
## ------------------------------------------ ##
##  CTD on bongo 
## ------------------------------------------ ##

## ------------------------------------------ ##
##  TDR offsets comments in meta
## ------------------------------------------ ##
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