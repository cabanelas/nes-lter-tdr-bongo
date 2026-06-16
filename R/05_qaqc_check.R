###############################################################
##  NES-LTER Bongo: Final QA/QC structural consistency check
##  Project: nes-lter-tdr-bongo
##  Script:  05_qaqc_check.R
###############################################################

library(tidyverse)
library(here)

## ------------------------------------------ ##
##  Load most recent exported CSVs (publish-ready files) ----
## ------------------------------------------ ##
tdr_file <- sort(list.files(here("data", "processed"),
                            pattern = "^tdr_data.*\\.csv$",
                            full.names = TRUE)) %>% tail(1)
tdr_data <- read_csv(tdr_file, show_col_types = FALSE)

ctd_file <- sort(list.files(here("data", "processed"),
                            pattern = "^ctd_bongo_data.*\\.csv$",
                            full.names = TRUE)) %>% tail(1)
ctd_bongo_data <- read_csv(ctd_file, show_col_types = FALSE)

px_file <- sort(list.files(here("data", "processed"),
                           pattern = "^px_data_bongo.*\\.csv$",
                           full.names = TRUE)) %>% tail(1)
px_data_bongo_final <- read_csv(px_file, show_col_types = FALSE)

offsets_file <- sort(list.files(here("data", "processed"),
                                pattern = "^tdr_offsets_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                                full.names = TRUE)) %>% tail(1)
offsets_final <- read_csv(offsets_file, show_col_types = FALSE)

cat("Files loaded:\n")
cat(" TDR:    ", basename(tdr_file), "\n")
cat(" CTD:    ", basename(ctd_file), "\n")
cat(" PX:     ", basename(px_file), "\n")
cat(" Offsets:", basename(offsets_file), "\n\n")

tables <- list(tdr = tdr_data, ctd = ctd_bongo_data, px = px_data_bongo_final)

## 1. shared columns: types should match across all three
shared_cols <- c("cruise", "station", "cast", "date_time", "depth_m", "temp_C", "down_up", "note_code")

cat("\n== shared column types ==\n")
tables %>%
  imap_dfr(~ tibble(table = .y, column = names(.x), class = map_chr(.x, ~ class(.)[1]))) %>%
  filter(column %in% shared_cols) %>%
  pivot_wider(names_from = table, values_from = class) %>%
  print(n = Inf)

## 2. down_up vocabulary
cat("\n== down_up values ==\n")
tables %>% imap_dfr(~ tibble(table = .y, value = unique(.x$down_up))) %>%
  arrange(value, table) %>% print()

## 3. note_code vocabulary (non-NA)
cat("\n== note_code values ==\n")
tables %>% imap_dfr(~ tibble(table = .y, value = unique(.x$note_code))) %>%
  filter(!is.na(value)) %>% arrange(value, table) %>% print()

## 4. depth_m range + sign convention
cat("\n== depth_m range ==\n")
tables %>% imap_dfr(~ tibble(table = .y,
                             min = min(.x$depth_m, na.rm = TRUE),
                             max = max(.x$depth_m, na.rm = TRUE))) %>% print()

## 5. date_time timezone
cat("\n== date_time timezone ==\n")
tables %>% imap_dfr(~ tibble(table = .y, tz = attr(.x$date_time, "tzone"))) %>% print()

## 6. offsets_final — structure + completeness
cat("\n== offsets_final columns ==\n")
print(names(offsets_final))
stopifnot(all(!is.na(offsets_final$offset_m)))
cat("offset_m: no NAs present\n")

cat("\n== TDR casts missing an offset ==\n")
anti_join(tdr_data %>% distinct(cruise, station, cast),
          offsets_final %>% distinct(cruise, station, cast),
          by = c("cruise", "station", "cast")) %>%
  print(n = Inf)
