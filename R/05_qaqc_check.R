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
tdr_data <- read_csv(here("data", "processed", "nes-lter-bongo-tdr.csv"), 
                     show_col_types = FALSE)
ctd_bongo_data <- read_csv(here("data", "processed", "nes-lter-bongo-ctd.csv"), 
                           show_col_types = FALSE)
px_data_bongo_final <- read_csv(here("data", "processed", "nes-lter-bongo-px.csv"), 
                                show_col_types = FALSE)
offsets_final <- read_csv(here("data", "processed", "nes-lter-bongo-tdr-offsets.csv"),
                          show_col_types = FALSE)

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

tdr_cols <- read_csv(here("data", "processed", "tdr-column-headers.csv"), show_col_types = FALSE)
ctd_cols <- read_csv(here("data", "processed", "ctd-column-headers.csv"), show_col_types = FALSE)
px_cols  <- read_csv(here("data", "processed", "px-column-headers.csv"),  show_col_types = FALSE)
offset_cols <- read_csv(here("data", "processed", "tdr-offsets-column-headers.csv"),  show_col_types = FALSE) 

tdr_cols
ctd_cols
px_cols
offset_cols

## ------------------------------------------ ##
##  Session info ----
## ------------------------------------------ ##
writeLines(capture.output(sessionInfo()), here("session_info.txt"))

################################################################################
# THE END
################################################################################