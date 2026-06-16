###############################################################
##  NES-LTER Bongo: Instrument Coverage Summary & Heatmap
##  Project: nes-lter-tdr-bongo
##  Script:  04_instrument_coverage.R
##  Author:  Alexandra Cabanelas
##
##  Purpose: Build a unified cast-level instrument availability table
##           across TDR, CTD, and PX sensor data, and
##           produce a coverage heatmap and summary plots.
##
##  Input:   data/processed/tdr_data_no_offset.csv     (02_tdr_tidy.R)
##           data/processed/ctd_bongo_data.csv         (02_ctd_bongo_tidy.R)
##           data/processed/px_data_bongo.csv          (02_px_sensor_tidy.R)
##           data/raw/all-nes-lter-bongologs-20260526.csv
##
##  Output:  data/processed/nes_lter_bongo_instrument_coverage.csv
##           figures/instrument_coverage_heatmap.pdf
##           figures/instrument_coverage_summary.pdf
###############################################################

# EN655 = L9B15 hit bottom = no sample = tdr cast but no sample
# EN712  = L6B5 hit bottom = no sample = tdr cast but no sample
# for the tdr data can use note code and note detail cols

## confirmed bad: sensor malfunction
## cross-checked raw px_data max depth vs logsheet target depth
## AR95 L6  B11:  px max 17m,  logsheet target 90m
## AR99 L2  B3:   px max 7m,   logsheet target 39m
## AR99 L9  B5:   px max 14m,  logsheet target 200m
## EN727 L9 B11:  px max 21m,  logsheet target ~200m

## ------------------------------------------ ##
##  Packages                   ----
## ------------------------------------------ ##
library(tidyverse)
library(here)

## ------------------------------------------ ##
##  Load processed instrument data    ----
## ------------------------------------------ ##
tdr_file <- sort(list.files(here("data", "processed"),
                            pattern = "^tdr_data_no_offset_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
basename(tdr_file)
tdr <- readRDS(tdr_file)

ctd_file <- sort(list.files(here("data", "processed"),
                            pattern = "^ctd_bongo_data_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
basename(ctd_file)
ctd <- readRDS(ctd_file)

px_file <- sort(list.files(here("data", "processed"),
                            pattern = "^px_data_bongo_\\d{4}-\\d{2}-\\d{2}\\.rds$",
                            full.names = TRUE)) %>% tail(1)
basename(px_file)
px <- readRDS(px_file)

## ------------------------------------------ ##
##  Full tow metadata    ----
## ------------------------------------------ ##
## Options:
##   (a) all-nes-lter-bongologs CSV
##   (b) the EDI zooplankton abundance package cast inventory
## one row per cruise/station/cast that actually had a bongo tow,
## regardless of whether instrument data exists

## --- bongo logsheets for tow meta ---
all_tows <- read_csv(here("data", "raw",
                          "all-nes-lter-bongologs-20260526.csv"), 
                     show_col_types = FALSE) %>%
  mutate(cast = paste0("B", cast)) %>%
  filter(!is.na(cast), !is.na(depth_TDR)) %>%
  filter(!(cruise == "EN617" & station == "L11" & cast %in% c("B25A", "B25B"))) %>% 
  distinct(cruise, station, cast) 
  #filter(grepl("^[BR]", cast))  # bongo and ring net tows only

## ------------------------------------------ ##
##  Cast-level summary per instrument  ----
## ------------------------------------------ ##

## helper: summarise one instrument df to cast level
summarise_instrument <- function(df, inst_prefix) {
  df %>%
    group_by(cruise, station, cast) %>%
    summarise(
      max_depth_m  = max(depth_m,  na.rm = TRUE),
      n_obs        = n(),
      has_downcast = any(down_up == "downcast", na.rm = TRUE),
      has_upcast   = any(down_up == "upcast",   na.rm = TRUE),
      note_code    = first(note_code),
      .groups      = "drop"
    ) %>%
    rename_with(~ paste0(inst_prefix, "_", .), -c(cruise, station, cast))
}

tdr_sum <- summarise_instrument(tdr, "tdr")
ctd_sum <- summarise_instrument(ctd, "ctd")
px_sum  <- summarise_instrument(px,  "px")

## ------------------------------------------ ##
##  Join to full tow universe       ----
## ------------------------------------------ ##
coverage <- all_tows %>%
  left_join(tdr_sum, by = c("cruise", "station", "cast")) %>%
  left_join(ctd_sum, by = c("cruise", "station", "cast")) %>%
  left_join(px_sum,  by = c("cruise", "station", "cast")) %>%
  mutate(
    tdr_available = !is.na(tdr_n_obs),
    ctd_available = !is.na(ctd_n_obs),
    px_available  = !is.na(px_n_obs)
  )

## quick check
coverage %>%
  count(tdr_available, ctd_available, px_available) %>%
  arrange(desc(tdr_available), desc(ctd_available), desc(px_available)) %>%
  print()

## ------------------------------------------ ##
##  Assign status codes per instrument  ----
## ------------------------------------------ ##
## status hierarchy:
##   "good"       = data present, complete downcast + upcast
##   "incomplete" = data present but missing downcast or upcast
##   "note"       = data present but has a note_code flag
##   "missing"    = no data recovered
##   "na"         = instrument not deployed on this cruise

## cruises where each instrument was deployed
tdr_cruises <- unique(tdr$cruise)
ctd_cruises <- unique(ctd$cruise)
px_cruises  <- unique(px$cruise)

assign_status <- function(available, has_down, has_up, note, cruise, deployed_cruises) {
  case_when(
    !cruise %in% deployed_cruises          ~ "na",
    !available                             ~ "missing",
    !is.na(note)                           ~ "note",
    !has_down | !has_up                    ~ "incomplete",
    TRUE                                   ~ "good"
  )
}

coverage <- coverage %>%
  mutate(
    tdr_status = assign_status(tdr_available, tdr_has_downcast, tdr_has_upcast,
                               tdr_note_code, cruise, tdr_cruises),
    ctd_status = assign_status(ctd_available, ctd_has_downcast, ctd_has_upcast,
                               ctd_note_code, cruise, ctd_cruises),
    px_status  = assign_status(px_available,  px_has_downcast,  px_has_upcast,
                               px_note_code,  cruise, px_cruises)
   )
  # mutate(across(ends_with("_status"), ~ if_else(. == "na", "missing", .)))

coverage %>%
  count(tdr_status) %>% print()
coverage %>%
  count(ctd_status) %>% print()
coverage %>%
  count(px_status)  %>% print()

## ------------------------------------------ ##
##  Export coverage table          ----
## ------------------------------------------ ##
# write_csv(coverage,
#           here("data", "processed", "nes_lter_bongo_instrument_coverage.csv"))

## ------------------------------------------ ##
##  Heatmap helper setup            ----
## ------------------------------------------ ##
status_colors <- c(
  good       = "#1D9E75",  # teal
  note       = "#EF9F27",  # amber — incomplete/flagged but present
  incomplete = "#FAC775",  # light amber — missing downcast or upcast
  missing    = "#B4B2A9",  # gray — no data recovered
  na         = "#F1EFE8"   # very light gray — instrument not deployed
)

status_labels <- c(
  good       = "Complete",
  note       = "Flagged (late start / early end / patched)",
  incomplete = "Incomplete (missing downcast or upcast)",
  missing    = "No data",
  na         = "Station not sampled"
)

## station order
station_levels <- c(paste0("L", 1:11), "MVCO")

## cruise order (chronological)
cruise_levels <- c(
  "EN608","EN617","EN627","EN644",
  "EN649","EN655","EN657","EN661","EN668",
  "AT46","EN687","EN695",
  "HRS2303","EN706","AR77",
  "EN712","EN715","EN720","AE2426",
  "EN727","AR88","AR92","AR95","AR99"
)

## ------------------------------------------ ##
##  1. Heatmap (one panel per instrument)  ----
## ------------------------------------------ ##
heatmap_df <- coverage %>%
  filter(grepl("^B", cast)) %>%
  filter(!is.na(cruise)) %>%
  filter(station %in% station_levels) %>%
  filter(cruise %in% cruise_levels) %>%
  select(cruise, station, cast, tdr_status, ctd_status, px_status) %>%
  pivot_longer(cols = ends_with("_status"),
               names_to  = "instrument",
               values_to = "status") %>%
  mutate(
    instrument = str_remove(instrument, "_status") %>% str_to_upper(),
    instrument = factor(instrument, levels = c("TDR", "CTD", "PX")),
    station    = factor(station, levels = station_levels),
    cruise     = factor(cruise,  levels = rev(cruise_levels)),
    status     = factor(status,  levels = names(status_colors))
  ) %>%
  complete(cruise, station, instrument,
           fill = list(status = factor("na", levels = names(status_colors))))

pdf(here("figures", "instrument_coverage_heatmap.pdf"),
    width = 14, height = 8)

ggplot(heatmap_df, aes(x = station, y = cruise, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_manual(values  = status_colors,
                    labels  = status_labels,
                    name    = NULL,
                    na.value = "#F1EFE8") +
  facet_wrap(~instrument, ncol = 3) +
  labs(title    = "NES-LTER Bongo",
       subtitle = "One cell per bongo tow; ring net tows excluded",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y      = element_text(size = 8),
    strip.text       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    panel.grid       = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(color = "white")))

coverage %>%
  filter(grepl("^B", cast), station %in% station_levels, cruise %in% cruise_levels) %>%
  count(cruise, station) %>%
  filter(n > 1) %>%
  arrange(cruise, station) %>%
  print(n = Inf)

dev.off()
message("Saved: instrument_coverage_heatmap.pdf")

## ------------------------------------------ ##
##  2. Summary bar: tows per cruise      ----
## ------------------------------------------ ##
summary_df <- coverage %>%
  filter(grepl("^B", cast)) %>%
  select(cruise, tdr_status, ctd_status, px_status) %>%
  pivot_longer(cols = ends_with("_status"),
               names_to  = "instrument",
               values_to = "status") %>%
  filter(status != "na") %>%   # only cruises where instrument was deployed
  mutate(
    instrument = str_remove(instrument, "_status") %>% str_to_upper(),
    instrument = factor(instrument, levels = c("TDR", "CTD", "PX")),
    cruise     = factor(cruise, levels = cruise_levels),
    status     = factor(status, levels = names(status_colors))
  ) %>%
  count(cruise, instrument, status)

pdf(here("figures", "instrument_coverage_summary.pdf"),
    width = 14, height = 6)

ggplot(summary_df, aes(x = cruise, y = n, fill = status)) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = status_colors,
                    labels = status_labels,
                    name   = NULL) +
  facet_wrap(~instrument, ncol = 1, scales = "free_y") +
  labs(title = "Cast counts by instrument and data status",
       x = NULL, y = "Number of casts") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    strip.text      = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

dev.off()
message("Saved: instrument_coverage_summary.pdf")

## ------------------------------------------ ##
##  3. Quick console summary           ----
## ------------------------------------------ ##
cat("\n=== Instrument coverage summary ===\n")

cat("\nTDR:\n")
coverage %>%
  filter(tdr_status != "na") %>%
  count(cruise, tdr_status) %>%
  pivot_wider(names_from = tdr_status, values_from = n, values_fill = 0) %>%
  print(n = Inf)

cat("\nCTD:\n")
coverage %>%
  filter(ctd_status != "na") %>%
  count(cruise, ctd_status) %>%
  pivot_wider(names_from = ctd_status, values_from = n, values_fill = 0) %>%
  print(n = Inf)

cat("\nPX sensor:\n")
coverage %>%
  filter(px_status != "na") %>%
  count(cruise, px_status) %>%
  pivot_wider(names_from = px_status, values_from = n, values_fill = 0) %>%
  print(n = Inf)

rm(tdr_sum, ctd_sum, px_sum, heatmap_df, summary_df)
