###############################################################
#########         NES Bongo TDR files     #####################
###############################################################
## by: Alexandra Cabanelas
###############################################################
# this script gathers and merges tdr files across all cruises
# goal is to have 1 csv file with cleaned tdr profiles

# tdr files must be saved as csv for this to read them

#####################################EN706 most likely bad -- CHECK
## ------------------------------------------ ##
# includes the following cruises (16): 
# 2018 = EN617
# 2019 = 
# 2020 = EN649, EN655, EN657
# 2021 = 
# 2022 = AT46, EN687
# 2023 = HRS2303, EN706, AR77
# 2024 = EN712, EN715, EN720, AE2426
# 2025 = EN727, AR88, AR92, **need to add AR95**
## ------------------------------------------ ##

# .DAT files only, csv created in a separate R script: EN644, EN627, EN608

# no TDR data for the following cruises: EN661, AR63*, AR38, AR32, EN715, EN695, EN668 (CTD only)
# CTD data available for: EN668 (no TDR) and EN706

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(here)

library(readr)
library(stringr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

raw_dir <- here("raw") # directory containing the tdr data folders

# search .csv files within the raw directory and subdirectories
file_paths <- list.files(raw_dir, 
                         pattern = "\\.csv$", 
                         full.names = TRUE, 
                         recursive = TRUE) #searches subdirectories
# ignore offsets file
file_paths <- file_paths[basename(file_paths) != "tdr_offsets.csv"]

# function to process each CSV file
process_file <- function(file_path) {
  file_name <- basename(file_path)  # extract the base name of the file (without path)
  print(paste("Processing:", file_name))
  
  # split the file name based on delimiters: _ and -
  name_parts <- unlist(strsplit(file_name, "[-_]"))
  
  # extract cruise, station, and cast based on their positions
  cruise_name <- name_parts[2]  # second position = cruise 
  station <- name_parts[3]  # 3rd position = station
  cast <- str_remove(name_parts[4], "\\.csv$") #4th position = cast and remove .csv extension
  
  print(paste("Cruise:", cruise_name, "Station:", station, "Cast:", cast))
  
  # detect file encoding, due to special characters/non-ASCII text
  enc_guess <- guess_encoding(file_path)
  encoding <- enc_guess$encoding[1] #sometimes ISO-8859-1 or UTF-8
  print(paste("Detected encoding:", encoding))
  
  # read the CSV file with the detected encoding
  data <- tryCatch({
    if (grepl("\\.csv$", file_path)) {
      read_csv(file_path, locale = locale(encoding = encoding))
    } else {
      stop("Unsupported file type: ", file_path)
    }
  }, error = function(e) {
    message("Error reading file: ", file_path)
    message("Error: ", e)
    return(NULL)
  })
  
  if (is.null(data)) {
    return(NULL)
  }
  
  print("Data read successfully")
  
  # add cruise name, station, and cast cols
  data <- data %>%
    mutate(cruise = cruise_name,
           station = station,
           cast = cast)
  
  return(data)
}

# process and combine into a single df
if (length(file_paths) > 0) {
  all_data <- lapply(file_paths, process_file) %>% bind_rows()
  print(all_data)  
} else {
  print("No files found matching the pattern.")
}

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##
unique(all_data$cruise)
unique(all_data$station)
unique(all_data$cast)

# standardize column names -- 
#clean_names <- function(df) {
#  colnames(df) <- make.names(colnames(df), unique = TRUE)
  # make.names = ensures valid variables names (no spaces, etc)
#  df
#}

# --- clean data by handling special characters in temp columns -----
clean_data <- function(df) {
  df <- df %>%
    # temp col 2 diff names (misnamed due to encoding issues), coalesce 
    mutate(`Temp(°C)` = coalesce(`Temp(°C)`, `Temp(¡C)`)) %>%
    select(-`Temp(¡C)`)
  df
}

all_data <- clean_data(all_data)

# --- rename columns -----
all_data <- all_data %>%
  rename(`date_time` = `Date & Time`,
         `temp_C` = `Temp(°C)`,
         `depth_m` = `Depth(m)`) %>%
  # capitalize cruise names to maintain consistency 
  mutate(cruise = toupper(cruise),
         # fix dateTime column 
         date_time = as.POSIXct(as.Date(date_time, origin = "1899-12-30"), 
                                tz = "UTC")
  )
colnames(all_data)
unique(all_data$cruise)

# --- create df with TDR-CTD test only -----
tdr_test <- all_data %>%
  filter(station == "TDRCTD") %>%
  # --- clean cols -----
  mutate(
    # add new column with a fixed tag for all rows
    comments = "tdr_ctd_test",
    # extract station info
    station = str_extract(cast, "L\\d+"),
    # extract cast info
    cast = str_extract(cast, "B\\d+")
  )

# --- filter to exclude TDR-CTD test -----
all_data <- all_data %>%
  filter(station != "TDRCTD")

# --- fix station and cast entries -----
all_data <- all_data %>%
  mutate(
    # fix station names: add L, remove leading zeros
    station = ifelse(grepl("^0[1-9]$", station), 
                     paste0("L", station), station),
    station = gsub("^L0*(\\d)$", "L\\1", station),
    # remove leading zeros from cast (B01 → B1)
    cast = gsub("^B0*(\\d+)$", "B\\1", cast),
    # add "B" prefix to cast 
    cast = ifelse(!grepl("^B", cast), paste0("B", cast), cast)
  )
unique(all_data$station)
unique(all_data$cast)


# --- look for duplicate casts within one cruise, station, cast name -----
# I think the best way to clean would be to base it off start-end times 
# from elog or bongo log sheet
# need to go to the zoopl inventory package folder NES_TOW_data script 
# and export the correct times to match here.... 






#remove negative values = above surface
all_data <- all_data %>% filter(depth_m >= 0)

#write.csv(all_data, "output/allTDRdata.csv")

max_depth_per_group_notround <- all_data %>%
  group_by(cruise, station, cast) %>%
  summarize(max_depth = max(depth_m, na.rm = TRUE))

#write.csv(max_depth_per_group_notround, "output/max_depth_TDR_notrounded.csv")

## ------------------------------------------ ##
# add upcast/downcast column
all_data_sub <- all_data %>%
  group_by(cruise, station, cast) %>% # Group by cruise, station, and cast
  mutate(max_depth = max(depth_m), # Calculate the maximum depth for each group
         down_up = ifelse(row_number() <= which.max(depth_m), "downcast", "upcast")) %>%
  ungroup() #%>%
  #select(-max_depth) #removes max depth column 

## ------------------------------------------ ##
# bin/average by depth
all_data_sub2 <- all_data_sub %>%
  group_by(cruise, station, cast, down_up, dateTime) %>%
  mutate(int_depth = floor(depth_m)) %>% # Round down to the nearest meter
  group_by(cruise, station, cast, down_up, dateTime, int_depth) %>%
  summarize(avg_temp_C = mean(temp_C, na.rm = TRUE)) %>% # Calculate average temperature
  ungroup() %>%
  distinct(cruise, station, cast, down_up, int_depth, .keep_all = TRUE)


## ------------------------------------------ ##
#            Plots -----
## ------------------------------------------ ##

plot_list <- list()
for (cruise in cruise_list) {
  cruise_data <- all_data_sub2 %>% filter(cruise == !!cruise)
  
  p <- ggplot(data = cruise_data, aes(x = dateTime, y = int_depth)) +
    geom_point() +
    geom_line() +
    facet_wrap(~station, scales = "free_x") +
    scale_y_reverse() +
    labs(title = paste("Cruise:", cruise), y = "Depth (m)", x = "DateTime") +
    theme_minimal()
  
  print(p)
}


for (cruise in cruise_list) {
  cruise_data <- all_data_sub2 %>% filter(cruise == !!cruise)
  
  p <- ggplot(data = cruise_data, aes(x = dateTime)) +
    geom_point(aes(y = int_depth, color = "Depth"), alpha = 0.5) +
    geom_line(aes(y = int_depth, color = "Depth"), alpha = 0.5) +
    geom_line(aes(y = avg_temp_C, color = "Temperature"), alpha = 0.5) + # No need to multiply by -10
    facet_wrap(~station, scales = "free_x") +
    scale_y_reverse(name = "Depth (m)", sec.axis = sec_axis(~., 
                                                            name = "Temperature (°C)",
                                                            trans = ~.)) + # No reverse transformation
    labs(title = paste("Cruise:", cruise), x = "DateTime") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("blue", "red"), labels = c("Depth", "Temperature"))
  
  print(p) 
}



plot_list <- list()
for (cruise in cruise_list) {
  cruise_data <- all_data_sub2 %>% filter(cruise == !!cruise)
  
  p <- ggplot(data = cruise_data, aes(x = avg_temp_C, y = int_depth, color = down_up)) +
    geom_point() +
    geom_line() +
    facet_wrap(~station, scales = "free_x") +
    scale_y_reverse() +
    labs(title = paste("Cruise:", cruise), y = "Depth (m)", x = "DateTime") +
    theme_minimal()
  
  print(p)
}


## ------------------------------------------ ##
#            Get MAX depth -----
## ------------------------------------------ ##

max_depth_per_group <- all_data_sub2 %>%
  group_by(cruise, station, cast) %>%
  summarize(max_depth = max(int_depth, na.rm = TRUE))

#write.csv(max_depth_per_group, "max_depth_tdr.csv")


###### LEFT OFF HERE.... NEED TO MAKE SURE FILES DONT NEED FIX

###########################################################
# clean files that have more than 1 cast
## NEED TO FIX EN617 L11 B25A and B25B in same file
## B25A timestamp 08:20-08:50
## B25B timestamp 09:16-09:53


## ------------------------------------------ ##
#   Plot profiles to check any discrepancies -----
## ------------------------------------------ ##
pdf("tdr-plots.pdf")

all_data %>%
  group_by(cruise, station, cast) %>%
  do(plots = ggplot(data = ., aes(x = temp_C, y = depth_m)) +
       geom_point() +
       geom_line() +
       scale_y_reverse() + 
       labs(title = paste("Cruise:", unique(.$cruise), 
                          "Station:", unique(.$station), 
                          "Cast:", unique(.$cast)), 
            x = "Temperature (°C)", 
            y = "Depth (m)")) -> plot_list

# Extract and print plots
#plot_list$plots
# Save each plot to the PDF
for (i in 1:nrow(plot_list)) {
  print(plot_list$plots[[i]])
}

# Close the PDF device
dev.off()


## ------------------------------------------ ##

