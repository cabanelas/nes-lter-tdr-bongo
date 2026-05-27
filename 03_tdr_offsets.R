###############################################################
##  NES-LTER Bongo TDR: Merge & Process All Cruises
##  Project: nes-lter-tdr-bongo
##  Script:  03_tdr_offsets.R
##  Author:  Alexandra Cabanelas
##
##  Purpose:cross-ref with elog + CTD to build
## offset table, apply corrections

##  Input:  data/processed/tdr_ctd_tests.csv (created in 02)
##           data/raw/tdr_offsets.csv  


## ------------------------------------------ ##
##  5. Join depth offsets                  ----
## ------------------------------------------ ##
#### MOVE THIS TO LATER/FURTHER DOWN  ##########
#### WHAT ABOUT STEP 4     

# offset_m = the instrument depth offset for a given cast
# add depth_offset column
# Corrected depth = depth_m - offset_m 

### NEED TO CHECK AND ADD OFFSETS TO CRUISES 
# AR77; EN712, EN715, EN720, AE2426; EN727, AR88, AR92, AR95; AR99

# offsets <- read.csv(here("data", "raw", "tdr_offsets.csv"),
#                     stringsAsFactors = FALSE) %>%
#   mutate(across(c(cruise, station, cast), as.character))
# 
# all_data <- all_data %>%
#   left_join(offsets, by = c("cruise", "station", "cast")) %>%
#   mutate(
#     depth_offset = replace_na(offset_m, 0),  # 0 if no offset recorded
#     depth_m_raw  = depth_m,                  # preserve original
#     depth_m      = depth_m - depth_offset    # corrected depth
#   ) %>%
#   select(-offset_m) %>%
#   # after correction, remove any rows that went negative
#   filter(depth_m >= 0)
# 
# # report which casts received a non-zero offset
# offsets_applied <- all_data %>%
#   filter(depth_offset != 0) %>%
#   distinct(cruise, station, cast, depth_offset)
# 
# message("Depth offsets applied to ", nrow(offsets_applied), " cast(s):")
# print(offsets_applied)