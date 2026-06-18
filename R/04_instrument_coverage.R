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

## confirmed bad PX casts
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
  filter(!is.na(datetime_UTC_start)) %>%
  filter(!(cruise == "EN617" & station == "L11" & cast %in% c("B25A", "B25B"))) %>% 
  distinct(cruise, station, cast)

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
##   good       = data present, complete downcast + upcast
##   incomplete = data present but missing downcast or upcast
##   note       = data present but has a note_code flag
##   missing    = no data recovered
##   na         = instrument not deployed on this cruise

## cruises where each instrument was deployed
tdr_cruises <- unique(tdr$cruise)
ctd_cruises <- unique(ctd$cruise)
px_cruises  <- unique(px$cruise)

assign_status <- function(available, has_down, has_up, note) {
  is_flagged_note <- !is.na(note) & note != "typo_corrected"
  case_when(
    !available          ~ "missing",
    is_flagged_note     ~ "note",
    !has_down | !has_up ~ "incomplete",
    TRUE                ~ "good"
  )
}

coverage <- coverage %>%
  mutate(
    tdr_status = assign_status(tdr_available, tdr_has_downcast, tdr_has_upcast, tdr_note_code),
    ctd_status = assign_status(ctd_available, ctd_has_downcast, ctd_has_upcast, ctd_note_code),
    px_status  = assign_status(px_available,  px_has_downcast,  px_has_upcast,  px_note_code)
  )

coverage %>%
  count(tdr_status) %>% print()
coverage %>%
  count(ctd_status) %>% print()
coverage %>%
  count(px_status)  %>% print()

# stations that get sampled more than once within a cruise
coverage %>%
  group_by(cruise, station) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(cruise, station) %>%
  select(cruise, station, cast, tdr_status, ctd_status, px_status) %>%
  print(n = Inf)

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
station_levels <- c("MVCO", paste0("L", 1:11))

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

# to add symbols on plot for special cases
notes_df <- coverage %>%
  filter(grepl("^B", cast), !is.na(cruise),
         station %in% station_levels, cruise %in% cruise_levels) %>%
  select(cruise, station, cast, tdr_note_code, ctd_note_code, px_note_code) %>%
  pivot_longer(cols = ends_with("_note_code"),
               names_to = "instrument", values_to = "note_code") %>%
  mutate(
    instrument = str_remove(instrument, "_note_code") %>% str_to_upper(),
    instrument = factor(instrument, levels = c("TDR", "CTD", "PX")),
    station    = factor(station, levels = station_levels),
    cruise     = factor(cruise, levels = rev(cruise_levels))
  ) %>%
  filter(note_code %in% c("no_max_depth", "hit_bottom"))

pdf(here("figures", "instrument_coverage_heatmap.pdf"),
    width = 14, height = 8)

ggplot(heatmap_df, aes(x = station, y = cruise, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_point(data = notes_df,
             aes(x = station, y = cruise, shape = note_code),
             inherit.aes = FALSE, size = 2, color = "black") +
  scale_shape_manual(
    values = c(no_max_depth = 8, hit_bottom = 16),
    labels = c(no_max_depth = "No max. depth recorded",
               hit_bottom    = "No zooplankton sample"),
    name = NULL
  ) +
  scale_fill_manual(values  = status_colors,
                    labels  = status_labels,
                    name    = NULL,
                    na.value = "#F1EFE8") +
  facet_wrap(~instrument, ncol = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(size = 9, angle = 45, hjust = 1, 
                                    color = "black"),
    axis.text.y      = element_text(size = 8, color = "black"),
    strip.text       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.box       = "vertical",
    panel.grid       = element_blank()
  ) +
  guides(fill  = guide_legend(nrow = 2, override.aes = list(color = "white")),
         shape = guide_legend(nrow = 1))

dev.off()

# simpler without symbols
ggplot(heatmap_df, aes(x = station, y = cruise, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_manual(values  = status_colors,
                    labels  = status_labels,
                    name    = NULL,
                    na.value = "#F1EFE8") +
  facet_wrap(~instrument, ncol = 3) +
  labs(title    = "NES-LTER Bongo",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(size = 9, angle = 45, hjust = 1, 
                                    color = "black"),
    axis.text.y      = element_text(size = 8, color = "black"),
    strip.text       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    panel.grid       = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(color = "white")))

## simplified available / not available version
status_colors_simple <- c(
  available     = "#1D9E75",  # green - data present (regardless of flags)
  not_available = "#B4B2A9",  # gray - no data recovered
  na            = "#F1EFE8"   # very light gray - station not sampled
)

status_labels_simple <- c(
  available     = "Available",
  not_available = "Not available",
  na            = "Station not sampled"
)

heatmap_df_simple <- heatmap_df %>%
  mutate(status = case_when(
    status %in% c("good", "note", "incomplete") ~ "available",
    status == "missing" ~ "not_available",
    TRUE ~ as.character(status)
  )) %>%
  mutate(status = factor(status, levels = names(status_colors_simple)))

pdf(here("figures", "instrument_coverage_heatmap_available.pdf"),
    width = 14, height = 8)

ggplot(heatmap_df_simple, aes(x = station, y = cruise, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_point(data = notes_df,
             aes(x = station, y = cruise, shape = note_code),
             inherit.aes = FALSE, size = 2, color = "black") +
  scale_shape_manual(
    values = c(no_max_depth = 8, hit_bottom = 16),
    labels = c(no_max_depth = "No max. depth recorded",
               hit_bottom    = "No zooplankton sample"),
    name = NULL
  ) +
  scale_fill_manual(values  = status_colors_simple,
                    labels  = status_labels_simple,
                    name    = NULL,
                    na.value = "#F1EFE8") +
  facet_wrap(~instrument, ncol = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(size = 9, angle = 45, hjust = 1, 
                                    color = "black"),
    axis.text.y      = element_text(size = 8, color = "black"),
    strip.text       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.box       = "vertical",
    panel.grid       = element_blank()
  ) +
  guides(fill  = guide_legend(nrow = 1, override.aes = list(color = "white")),
         shape = guide_legend(nrow = 1))

dev.off()

## ------------------------------------------ ##
##  1b. Heatmap add years to y axis  ----
## ------------------------------------------ ##
## year lookup for faceting (order still comes from cruise_levels above)
cruise_years <- read_csv(here("data", "raw", "all-nes-lter-bongologs-20260526.csv"),
                         show_col_types = FALSE) %>%
  filter(cruise %in% cruise_levels) %>%
  group_by(cruise) %>%
  summarize(year = year(min(datetime_UTC_start, na.rm = TRUE)), .groups = "drop")

year_levels <- cruise_years$year[match(cruise_levels, cruise_years$cruise)] %>% unique()

heatmap_df2 <- heatmap_df %>%
  left_join(cruise_years, by = "cruise") %>%
  mutate(year = factor(year, levels = year_levels))

notes_df2 <- notes_df %>%
  left_join(cruise_years, by = "cruise") %>%
  mutate(year = factor(year, levels = year_levels))

ggplot(heatmap_df2, aes(x = station, y = cruise, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_point(data = notes_df2,
             aes(x = station, y = cruise, shape = note_code),
             inherit.aes = FALSE, size = 2, color = "black") +
  scale_shape_manual(
    values = c(no_max_depth = 8, hit_bottom = 16),
    labels = c(no_max_depth = "No max. depth recorded",
               hit_bottom    = "No zooplankton sample)"),
    name = NULL
  ) +
  scale_fill_manual(values  = status_colors,
                    labels  = status_labels,
                    name    = NULL,
                    na.value = "#F1EFE8") +
  facet_grid(rows = vars(year), cols = vars(instrument),
             scales = "free_y", space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x       = element_text(size = 9, angle = 45, hjust = 1, color = "black"),
    axis.text.y       = element_text(size = 8, color = "black"),
    strip.text.x      = element_text(size = 11, face = "bold"),
    strip.text.y.left = element_text(size = 9, angle = 90),
    strip.placement   = "outside",
    strip.background.y = element_blank(),
    legend.position   = "bottom",
    legend.text       = element_text(size = 9),
    legend.box        = "vertical",
    panel.grid        = element_blank(),
    panel.spacing.y   = unit(0, "pt"),
    panel.border      = element_rect(color = "grey50", fill = NA, linewidth = 0.3)
  ) +
  guides(fill  = guide_legend(nrow = 2, override.aes = list(color = "white")),
         shape = guide_legend(nrow = 1))

## ------------------------------------------ ##
##  2. Summary bar: tows per cruise      ----
## ------------------------------------------ ##
summary_df <- coverage %>%
  filter(grepl("^B", cast)) %>%
  filter(cruise %in% cruise_levels) %>%
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

dev.off()

pdf(here("figures", "instrument_coverage_summary.pdf"),
    width = 14, height = 6)

ggplot(summary_df, aes(x = cruise, y = n, fill = status)) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = status_colors,
                    labels = status_labels,
                    name   = NULL) +
  facet_wrap(~instrument, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Number of casts") +
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

## ------------------------------------------ ##
##  Export coverage table          ----
## ------------------------------------------ ##
write_csv(coverage,
          here("data", "processed", "nes_lter_bongo_instrument_coverage.csv"))

################################################################################
# go to -----------> 05_qaqc_check.R
################################################################################