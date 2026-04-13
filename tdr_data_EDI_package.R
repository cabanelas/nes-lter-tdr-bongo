###############################################################
###############################################################
#########         NES Bongo TDR files     #####################
###############################################################
## by: Alexandra Cabanelas
###############################################################
# this script is to prep tdr data for data package

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##

library(tidyverse)
library(here)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
tdr <- read.csv(file.path("output","allTDRdata.csv"),
         header = T) #created in tdr_files_merge script

# need to add EN608, EN627, EN644
# EN668 was CTD only, no TDR 

EN608 <- read.csv(file.path("output",
                            "EN608_csv",
                            "EN608_allTDRcasts.csv"),
                header = T)

EN627 <- read.csv(file.path("output",
                            "EN627_csv",
                            "EN627_allTDRcasts.csv"),
                  header = T)

EN644 <- read.csv(file.path("output",
                            "EN644_csv",
                            "EN644_allTDRcasts.csv"),
                  header = T)

other_tdr <- rbind(EN608, EN627, EN644)

offset <- read.csv(file.path("raw",
                             "tdr_offsets.csv"),
                   header = T)

# fix other_tdr colnames
other_tdr <- other_tdr %>%
  select(-Index) %>%
  rename(
    dateTime = DateTime,
    temp_C = Temperature,
    depth_m = Depth,
    cruise = cruise,
    station = station,
    cast = cast
  ) 

# the seconds have decimals, so remove
tdr$dateTime <- tdr$dateTime %>% 
  ymd_hms() %>%
  floor_date(unit = "second")

sapply(tdr, class)
sapply(other_tdr, class)

# Convert dateTime in tdr back to character
tdr$dateTime <- as.character(tdr$dateTime)

merged_df <- rbind(other_tdr, 
                   tdr)

# bin depth into 1 meter intervals
merged_df1 <- merged_df %>%
  mutate(depth_interval = floor(depth_m))

# Convert dateTime to POSIXct
merged_df1 <- merged_df1 %>%
  mutate(dateTime = as.POSIXct(dateTime, 
                               format = "%Y-%m-%d %H:%M:%S", 
                               tz = "UTC"))

all_data_sub <- merged_df1 %>%
  group_by(cruise, station, cast) %>% # Group by cruise, station, and cast
  mutate(max_depth = max(depth_m), # Calculate the maximum depth for each group
         down_up = ifelse(row_number() <= which.max(depth_m), "downcast", "upcast")) %>%
  ungroup()

# aggregate data by depth_interval
aggregated_df <- all_data_sub %>%
  group_by(depth_interval, cruise, station, cast, down_up) %>%
  summarize(
    avg_temp_C = mean(temp_C, na.rm = TRUE),
    new_dateTime = median(dateTime, na.rm = TRUE),
    max_depth = first(max_depth)  # Retain the max_depth
  )

##good to here
tdr_all <- aggregated_df %>%
  select(cruise, station, cast, depth_interval, avg_temp_C,
         new_dateTime, down_up, max_depth) %>%
  rename(
    dateTime = new_dateTime
  )

tdr_all <- tdr_all %>%
  mutate(dateTime = as.character(dateTime))

#now i have to fix things like multiple casts in one profile
#write.csv(tdr_all, "output/all_tdr_data_morecruises.csv")







############################################# ONE OPTION
library(zoo)  # For rollmean function
library(ggplot2)

# Assuming 'all_data_sub' is your data frame after binning depth
# Aggregate data by depth_interval and apply moving average
aggregated_df9 <- all_data_sub %>%
  group_by(cruise, station, cast, depth_interval, down_up) %>%
  summarize(
    avg_temp_C = mean(temp_C, na.rm = TRUE),
    new_dateTime = median(dateTime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cruise, station, cast, depth_interval, down_up) %>%
  group_by(cruise, station, cast, down_up) %>%
  mutate(avg_temp_C_smoothed = rollmean(avg_temp_C, k = 5, fill = NA, align = "center")) %>%
  ungroup()

#aggregated_df9 <- aggregated_df9 %>%
#  mutate(avg_temp_C_smoothed = coalesce(avg_temp_C_smoothed, avg_temp_C))


# Plot with smoothed temperature values
ggplot(aggregated_df9 %>%
         filter(cruise == "AR77", station == "L3"),
       aes(x = avg_temp_C_smoothed, y = depth_interval, color = down_up, linetype = down_up)) +
  geom_line() +
  geom_text(aes(label = round(avg_temp_C, 1)), hjust = -0.3, vjust = 0.5, size = 3) +
  scale_y_reverse() +  # Reverse y-axis for depth
  scale_x_continuous(position = "top") +
  labs(
    title = "Smoothed Temperature vs Depth",
    x = "Average Temperature (°C)",
    y = "Depth (m)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("downcast" = "blue", "upcast" = "red")) +  # Customize colors if needed
  scale_linetype_manual(values = c("downcast" = "solid", "upcast" = "dashed"))  # Customize line types if needed

##############################################################

# plot
# Create unique combinations of cruise, station, and cast
unique_combinations <- aggregated_df %>%
  select(cruise, station, cast) %>%
  distinct()

# Loop through each combination and create a plot
for (i in seq_len(nrow(unique_combinations))) {
  # Extract the current combination
  combo <- unique_combinations[i, ]
  
  # Subset data for the current combination
  subset_data <- aggregated_df %>%
    filter(cruise == combo$cruise,
           station == combo$station,
           cast == combo$cast)
  
  # Create the plot
  p <- ggplot(subset_data, aes(x = avg_temp_C, #dateTime, 
                               y = depth_interval, #avg_temp_C
                               color = down_up)) +
    geom_line() +
    labs(title = paste("Cruise:", combo$cruise, 
                       "Station:", combo$station, 
                       "Cast:", combo$cast),
         x = "Average Temperature (°C)",
         y = "Depth (m)") +
    scale_y_reverse() +
    scale_x_continuous(position = "top") + 
    theme_minimal() +
    theme(
      axis.title.x.top = element_text(vjust = 0.5),
      axis.text.x.top = element_text(vjust = 0.5)
    )
  #need to add fix x axis labels
  
  print(p)
}
