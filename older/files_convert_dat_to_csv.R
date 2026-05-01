###############################################################
#########         NES Bongo TDR files     #####################
###############################################################
## by: Alexandra Cabanelas
###############################################################
# converting .DAT files to csv for TDR files that were never
# saved as csv
# cruises: EN627, EN644. EN608 

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(dplyr)
library(here)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
# --- EN627 -----
en627_folder <- here("raw","EN627_TDR")

# list all .dat files in the directory
dat_files <- list.files(path = en627_folder, pattern = "\\.dat$", 
                        full.names = TRUE)

# function to read a single .dat file, skipping metadata lines
read_dat_file <- function(file) {
  # read the data, skipping the first 15 lines (adjust if necessary)
  df <- read.table(file, skip = 15, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  
  # set the column names
  colnames(df) <- c("Index", "DateTime", "Temperature", "Depth")
  
  # replace commas with dots and convert to numeric
  df$Temperature <- as.numeric(gsub(",", ".", df$Temperature))
  df$Depth <- as.numeric(gsub(",", ".", df$Depth))
  
  # convert DateTime to POSIXct with milliseconds included
  df$DateTime <- as.POSIXct(df$DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  
  # extract cruise, station, and cast information from the file name
  file_name <- basename(file)
  parts <- unlist(strsplit(file_name, "-"))
  cruise <- parts[2]
  station <- parts[3]
  cast <- parts[4]
  
  # remove the .dat extension from cast
  cast <- gsub("\\.dat$", "", cast)
  
  # add cruise, station, and cast columns
  df$cruise <- cruise
  df$station <- station
  df$cast <- cast
  
  return(df)
}

# Read all .dat files and store them in a list
dat_list <- lapply(dat_files, read_dat_file)

# Convert list of dfs into a single df
combined_df <- do.call(rbind, dat_list)

tail(combined_df)

#Issues with L2 and L3 being together in one TDR file
# need to distinguish + separate the different casts
#L02 B07 timestamp 02.02.2019 08:30:01,000 to 02.02.2019 08:37:01,000
#L03 B08 timestamp 02.02.2019 10:43:01,000 to 02.02.2019 10:50:01,000

# Function to split data frame based on timestamps
split_data_by_timestamp <- function(df) {
  # Define the start and end timestamps for the two measurements
  start_L02_B07 <- as.POSIXct("2019-02-02 08:30:01.000", tz = "UTC")
  end_L02_B07 <- as.POSIXct("2019-02-02 08:37:01.000", tz = "UTC")
  start_L03_B08 <- as.POSIXct("2019-02-02 10:43:01.000", tz = "UTC")
  end_L03_B08 <- as.POSIXct("2019-02-02 10:50:01.000", tz = "UTC")
  
  # Filter rows based on timestamps
  df_L02_B07 <- df[df$DateTime >= start_L02_B07 & df$DateTime <= end_L02_B07, ]
  df_L03_B08 <- df[df$DateTime >= start_L03_B08 & df$DateTime <= end_L03_B08, ]
  
  return(list(df_L02_B07, df_L03_B08))
}

split_dat_list <- lapply(dat_list, split_data_by_timestamp)

# Convert list of dataframes into a single dataframe
combined_df_L02_B07 <- do.call(rbind, lapply(split_dat_list, `[[`, 1))
combined_df_L03_B08 <- do.call(rbind, lapply(split_dat_list, `[[`, 2))

# Replace station and cast names in combined_df_L02_B07
combined_df_L02_B07$station <- gsub("&.*", "", combined_df_L02_B07$station)
combined_df_L02_B07$cast <- gsub("&.*", "", combined_df_L02_B07$cast)

# Replace station and cast names in combined_df_L03_B08
combined_df_L03_B08$station <- "L03"
combined_df_L03_B08$cast <- "B08"

# remove the merged file/casts from df and add them back separately 
combined_df1 <- combined_df %>%
  filter(station != "L02&03")

# add L2 and L3 to entire df
combined_df1 <- rbind(combined_df1, combined_df_L02_B07, combined_df_L03_B08)

# FINAL COLUMNS

class(combined_df1$DateTime)
#"POSIXct" "POSIXt" 

########################
#### to inspect data frames separately can ... 

file_names <- gsub("\\.dat$", "", basename(dat_files))

# Assign each data frame to a variable named after the original file name
for (i in seq_along(dat_list)) {
  assign(file_names[i], dat_list[[i]])
}

# view one of the data frames by name
print(file_names)  # see the variable names created
head(get(file_names[1]))  # view the first few rows of the first data frame

## save each as a csv so we have the non .dat file too
# Loop through each dataframe and save it as a CSV file
#for (i in seq_along(dat_list)) {
  # Extract the dataframe and file name
#  df <- dat_list[[i]]
#  file_name <- file_names[i]
  
  # Define the file path
#  file_path <- paste0(file_name, ".csv")
  
  # Save the dataframe as a CSV file
#  write.csv(df, file = file_path, row.names = FALSE)
#}
# also save L2 and L3 separately 
#write.csv(combined_df_L02_B07, "1$28C9447-EN627-L02-B07.csv")
#write.csv(combined_df_L03_B08, "1$28C9447-EN627-L03-B08.csv")

# save all casts from that cruise to use in tdr_files_merge_v2.R
#write.csv(combined_df1, "EN627_allTDRcasts.csv")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
rm(list = ls())

#    -----       EN644
en644_folder <- here("raw","EN644_TDR")

# List all .dat files in the directory
dat_files <- list.files(path = en644_folder, pattern = "\\.dat$", full.names = TRUE)

# Function to read a single .dat file, skipping metadata lines
read_dat_file <- function(file) {
  # Read the data, skipping the first 15 lines (adjust if necessary)
  df <- read.table(file, skip = 15, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  
  # Manually set the column names
  colnames(df) <- c("Index", "DateTime", "Temperature", "Depth")
  
  # Replace commas with dots and convert to numeric
  df$Temperature <- as.numeric(gsub(",", ".", df$Temperature))
  df$Depth <- as.numeric(gsub(",", ".", df$Depth))
  
  # Convert DateTime to POSIXct with milliseconds included
  df$DateTime <- as.POSIXct(df$DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  
  # Extract cruise, station, and cast information from the file name
  file_name <- basename(file)
  parts <- unlist(strsplit(file_name, "-"))
  cruise <- parts[2]
  station <- parts[3]
  cast <- parts[4]
  
  # Remove the .dat extension from cast
  cast <- gsub("\\.dat$", "", cast)
  
  # Add cruise, station, and cast as new columns
  df$cruise <- cruise
  df$station <- station
  df$cast <- cast
  
  return(df)
}

# Read all .dat files and store them in a list
dat_list <- lapply(dat_files, read_dat_file)

# Convert list of dataframes into a single dataframe
combined_df <- do.call(rbind, dat_list)
# Example to view the combined dataframe
tail(combined_df)

# Remove leading zeros from station column
combined_df$station <- sub("^L0*", "L", combined_df$station)

# Remove leading zeros from cast column
combined_df$cast <- sub("^B0*", "B", combined_df$cast)

## save each as a csv so we have the non .dat file too
file_names <- gsub("\\.dat$", "", basename(dat_files))

# Assign each data frame to a variable named after the original file name
for (i in seq_along(dat_list)) {
  assign(file_names[i], dat_list[[i]])
}

# view one of the data frames by name
print(file_names)  # see the variable names created
head(get(file_names[1]))

# Loop through each dataframe and save it as a CSV file
#for (i in seq_along(dat_list)) {
# Extract the dataframe and file name
#  df <- dat_list[[i]]
#  file_name <- file_names[i]

# Define the file path
#  file_path <- paste0(file_name, ".csv")

# Save the dataframe as a CSV file
#  write.csv(df, file = file_path, row.names = FALSE)
#}

#write.csv(combined_df, "EN644_allTDRcasts.csv")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
rm(list = ls())

#    -----       EN608 broken file  
en608_folder <- here("raw","EN608_TDR")

# List all .dat files in the directory
dat_files <- list.files(path = en608_folder, full.names = TRUE)

# Function to read a single .dat file, skipping metadata lines
read_dat_file <- function(file) {
  # Read the data, skipping the first 15 lines (adjust if necessary)
  df <- read.table(file, skip = 15, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  
  # Manually set the column names
  colnames(df) <- c("Index", "DateTime", "Temperature", "Depth")
  
  # Replace commas with dots and convert to numeric
  df$Temperature <- as.numeric(gsub(",", ".", df$Temperature))
  df$Depth <- as.numeric(gsub(",", ".", df$Depth))
  
  # Convert DateTime to POSIXct with milliseconds included
  df$DateTime <- as.POSIXct(df$DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  
  # Extract cruise, station, and cast information from the file name
  file_name <- basename(file)
  parts <- unlist(strsplit(file_name, "-"))
  cruise <- parts[2]
  station <- parts[3]
  cast <- parts[4]
  
  # Remove the .dat extension from cast
  cast <- gsub("\\.dat$", "", cast)
  
  # Add cruise, station, and cast as new columns
  df$cruise <- cruise
  df$station <- station
  df$cast <- cast
  
  return(df)
}

# Read all .dat files and store them in a list
dat_list <- lapply(dat_files, read_dat_file)

# Convert list of dataframes into a single dataframe
combined_df <- do.call(rbind, dat_list)
# Example to view the combined dataframe
tail(combined_df)

# Remove leading zeros from station column
combined_df$station <- sub("^L0*", "L", combined_df$station)

# Remove leading zeros from cast column
combined_df$cast <- sub("^B0*", "B", combined_df$cast)

## save each as a csv so we have the non .dat file too
file_names <- gsub("\\..*$", "", basename(dat_files))

# Assign each data frame to a variable named after the original file name
for (i in seq_along(dat_list)) {
  assign(file_names[i], dat_list[[i]])
}

# view one of the data frames by name
print(file_names)  # see the variable names created
head(get(file_names[1]))

# Loop through each dataframe and save it as a CSV file
#for (i in seq_along(dat_list)) {
# Extract the dataframe and file name
#  df <- dat_list[[i]]
#  file_name <- file_names[i]

# Define the file path
#  file_path <- paste0(file_name, ".csv")

# Save the dataframe as a CSV file
#  write.csv(df, file = file_path, row.names = FALSE)
#}

#write.csv(combined_df, "EN608_allTDRcasts.csv")
