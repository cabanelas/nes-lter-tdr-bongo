
## ------------------------------------------ ##
##  Bin to 1-m depth intervals          ----
## ------------------------------------------ ##
# average temp within each 1-m bin per cast x down_up
# drop upcast rows shallower than 2 m (surface tail noise)

tdr_binned <- bin_by_depth(tdr_trim)

message("Binned rows: ", nrow(tdr_binned))
message("Depth bins range: ", min(tdr_binned$depth_bin), " – ",
        max(tdr_binned$depth_bin), " m")

## ------------------------------------------ ##
##  Binned profile plots               ----
## ------------------------------------------ ##

pdf(here("figures", "binned_profiles_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = avg_temp_C, y = depth_bin, color = down_up)) +
    geom_path() +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(
      values = c(downcast = "steelblue", upcast = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Binned T profiles —", cr),
         x = "Avg temp (°C)", y = "Depth bin (m)") +
    theme_minimal() +
    theme(strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

pdf(here("figures", "binned_depth_time_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = date_time, y = depth_bin, color = avg_temp_C)) +
    geom_point(size = 1.2) +
    scale_y_reverse() +
    scale_color_viridis_c(option = "plasma", name = "°C") +
    facet_wrap(~label, scales = "free_x") +
    labs(title = paste("Depth vs time (colored by temp) —", cr),
         x = NULL, y = "Depth bin (m)") +
    theme_minimal() +
    theme(axis.text.x     = element_blank(),
          strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

pdf(here("figures", "binned_temp_spread_check.pdf"),
    width = 14, height = 10)

for (cr in sort(unique(tdr_binned$cruise))) {
  p <- tdr_binned %>%
    filter(cruise == cr) %>%
    mutate(label = paste(station, cast)) %>%
    ggplot(aes(x = avg_temp_C, y = depth_bin,
               color = down_up, group = down_up)) +
    geom_path(linewidth = 0.6) +
    geom_point(size = 0.8, alpha = 0.6) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(
      values = c(downcast = "steelblue", upcast = "firebrick"),
      name = NULL
    ) +
    facet_wrap(~label, scales = "free") +
    labs(title = paste("Down vs upcast temp —", cr),
         x = "Avg temp (°C)", y = "Depth bin (m)") +
    theme_minimal() +
    theme(strip.text      = element_text(size = 6),
          legend.position = "bottom")
  print(p)
}

dev.off()

tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(max_depth = max(depth_bin), .groups = "drop") %>%
  ggplot(aes(x = reorder(paste(station, cast), max_depth), y = max_depth)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~cruise, scales = "free_x") +
  labs(title = "Max depth per cast by cruise",
       x = NULL, y = "Max depth bin (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        strip.text  = element_text(size = 7))

tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(max_temp = max(avg_temp_C), .groups = "drop") %>%
  ggplot(aes(x = reorder(paste(station, cast), max_temp), y = max_temp)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~cruise, scales = "free_x") +
  labs(title = "Max temp per cast by cruise",
       x = NULL, y = "Max temp bin (C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        strip.text  = element_text(size = 7))

## ------------------------------------------ ##
##  Cast QC summary                    ----
## ------------------------------------------ ##

cast_qc <- tdr_binned %>%
  group_by(cruise, station, cast) %>%
  summarise(
    max_depth_m     = max(depth_bin,      na.rm = TRUE),
    n_depth_bins    = n_distinct(depth_bin),
    n_downcast_bins = sum(down_up == "downcast"),
    n_upcast_bins   = sum(down_up == "upcast"),
    date_time_start = min(date_time,      na.rm = TRUE),
    date_time_end   = max(date_time,      na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    duration_min = as.numeric(difftime(date_time_end, date_time_start,
                                       units = "mins"))
  )

print(cast_qc, n = 20)