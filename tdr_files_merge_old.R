###############################################################
###############################################################
#########         NES Bongo TDR files     #####################
###############################################################
## by: Alexandra Cabanelas
###############################################################
# this script gathers and merges tdr files across all cruises
# goal is to have 1 csv file with cleaned tdr profiles

#EN706 most likely bad
# includes the following cruises: EN687, EN657, EN655, EN649, EN617, AT46, AR77,
# EN706, 
# .DAT files only, csv created in a separate R script: EN644, EN627, EN608

# no TDR data for the following cruises: - EN661, AR63*, AR38, AR32, EN715, EN695
# CTD data available for: EN668 and EN706

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##

library(dplyr)
library(here)
library(readr)
library(stringr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

# Directory containing the folders
raw_dir <- here("raw")

## ------------------------------------------ ##

# List only .csv files in the subdirectories
# This will search for all .csv files within the raw directory and its subdirectories
file_paths <- list.files(raw_dir, pattern = "\\.csv$", 
                         full.names = TRUE, 
                         recursive = TRUE)

# Function to process each CSV file
process_file <- function(file_path) {
  file_name <- basename(file_path)  # Extract the base name of the file (without path)
  print(paste("Processing file:", file_name))
  
  # Split the file name based on delimiters (_ and -)
  name_parts <- unlist(strsplit(file_name, "[-_]"))
  
  # Extract cruise name, station, and cast based on their positions
  cruise_name <- name_parts[2]  # second position = cruise name
  station <- name_parts[3]  # 3rd position = station
  cast <- str_remove(name_parts[4], "\\.csv$") #4th position = cast and remove .csv extension
  
  print(paste("Cruise:", cruise_name, "Station:", station, "Cast:", cast))
  
  # Detect file encoding, due to special characters/non-ASCII text
  enc_guess <- guess_encoding(file_path)
  encoding <- enc_guess$encoding[1]
  print(paste("Detected encoding:", encoding))
  
  # Read the CSV file with the detected encoding
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
  
  # Add new columns for cruise name, station, and cast
  data <- data %>%
    mutate(cruise = cruise_name,
           station = station,
           cast = cast)
  
  return(data)
}

# Process each file and combine into a single data frame if files are found
if (length(file_paths) > 0) {
  all_data <- lapply(file_paths, process_file) %>% bind_rows()
  print(all_data)  # Print the combined data
} else {
  print("No files found matching the pattern.")
}

# Standardize column names 
clean_names <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  df
}

# Clean the data by handling temperature columns (special characters)
clean_data <- function(df) {
  df <- df %>%
    mutate(`Temp(°C)` = coalesce(`Temp(°C)`, `Temp(¡C)`)) %>%
    select(-`Temp(¡C)`)
  df
}

# Apply the cleaning function to your data
all_data <- clean_data(all_data)

# Capitalize cruise names to maintain consistency 
all_data$cruise <- toupper(all_data$cruise)

# remove "99C9447-AT46_CTD_test_L2.csv" and other tests....
all_data <- all_data %>%
  filter(cast != "B99C9447-AT46_CTD_test_L2.csv")

# Replace leading zeros with "L" and convert single-digit numbers to L# format
all_data <- all_data %>%
  mutate(station = ifelse(grepl("^0[1-9]$", station), paste0("L", station), station),
         station = gsub("^L0*(\\d)$", "L\\1", station))

#may not need this part
#all_data <- all_data %>% 
#  mutate(
#    station = case_when(
#      station != "MVCO" & !grepl("^L", station) ~ paste0("L", station),
#      TRUE ~ station
#    )
#  )

#not sure if needed
#all_data$cast <- sub("^0+", "", all_data$cast)

# Remove leading zeros from cast numbers (e.g., B01 -> B1)
all_data$cast <- gsub("^B0*(\\d+)$", "B\\1", all_data$cast)

# Add "B" to cast entries missing the "B" prefix (if necessary)
all_data$cast <- ifelse(!grepl("^B", all_data$cast), paste0("B", all_data$cast), all_data$cast)

# Rename columns
all_data <- all_data %>%
  rename(`dateTime` = `Date & Time`,
         `temp_C` = `Temp(°C)`,
         `depth_m` = `Depth(m)`)

colnames(all_data)

## ------------------------------------------ ##
# clean files that have more than 1 cast


## ------------------------------------------ ##
# add upcast/downcast column
en649 <- en649 %>%
  group_by(cruise, station, cast) %>% # Group by cruise, station, and cast
  mutate(max_depth = max(depth_m), # Calculate the maximum depth for each group
         down_up = ifelse(row_number() <= which.max(depth_m), "downcast", "upcast")) %>%
  ungroup() #%>%
#select(-max_depth) #removes max depth column 




## ------------------------------------------ ##
# bin/average by depth
en649 <- en649 %>%
  group_by(cruise, station, cast, down_up) %>%
  mutate(int_depth = floor(depth_m)) %>% # Round down to the nearest meter
  group_by(cruise, station, cast, down_up, int_depth) %>%
  summarize(avg_temp_C = mean(temp_C, na.rm = TRUE)) %>% # Calculate average temperature
  ungroup()






###########################################################

## NEED TO FIX EN617 L11 B25A and B25B in same file
## B25A timestamp 08:20-08:50
## B25B timestamp 09:16-09:53

max_depth_index <- which.max(data$Depth)

# Create a new column 'down_up' and initialize it with NA values
data$down_up <- NA

# Assign 'downcast' to rows from the first row to the maximum depth index
data$down_up[1:max_depth_index] <- 'downcast'

# Assign 'upcast' to rows from the row after the maximum depth index to the last row
data$down_up[(max_depth_index + 1):nrow(data)] <- 'upcast'

