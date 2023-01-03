### EQI ###

# Load libraries
library(tidycensus)
library(tigris)
library(dplyr)
library(mapview)
library(sf)

# Set Census API key
census_api_key("YourCensusAPIKey", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

blocks <- blocks(state = 06, year = 2020)

eqi_blocks <- blocks %>%
  filter(ALAND20 > 0) %>%
  st_drop_geometry() %>%
  select(GEOID20, ALAND20, POP20) %>%
  mutate(bg_id = substr(GEOID20, 1, 12))


### DEMOGRAPHIC OVERLAY ###

# Set global parameters
year = 2020
state = 06
geography = "block group"
product = "acs5"

setwd("C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Data")

### Income data ###
# State median HH income
state_income <- get_acs(
  geography = "state",
  variables = "B19013_001",
  year = year,
  state = state,
  survey = product
)

state_cutoff <- .8 * state_income$estimate

# Median HH income
hh_income <- get_acs(
  geography = geography,
  variables = "B19013_001",
  year = year,
  state = state,
  geometry = FALSE
) %>%
  select(GEOID, estimate)

# HH size
hh_size <- get_acs(
  geography = geography,
  variables = "B25010_001",
  year = year,
  state = state,
  survey = product,
  geometry = FALSE
) %>%
  mutate(rounded_hh_size = round(estimate, digits = 0)) %>%
  select(GEOID, rounded_hh_size)

# Read in income limits data
income_limits <- read.csv("2020_IncomeLimits.csv")

# Income calcs
income_df <- merge(hh_income,
                   hh_size,
                   by = "GEOID",
                   all = T) %>%
  mutate(COUNTYFP = as.numeric(substr(GEOID, 3, 5)))

income_df <- merge(income_df,
                   income_limits,
                   by = "COUNTYFP",
                   all.x = T)

income_df <- income_df %>%
  mutate(local_low_income_threshold = case_when(
    is.na(rounded_hh_size) ~ NA_integer_,
    rounded_hh_size == 1 ~ One,
    rounded_hh_size == 2 ~ Two,
    rounded_hh_size == 3 ~ Three,
    rounded_hh_size == 4 ~ Four,
    rounded_hh_size == 5 ~ Five,
    rounded_hh_size == 6 ~ Six,
    rounded_hh_size == 7 ~ Seven,
    rounded_hh_size >= 8 ~ Eight,
    TRUE ~ NA_integer_
  )) %>%
  mutate(localized_income_screen = case_when(
    estimate <= local_low_income_threshold ~ 1,
    TRUE ~ 0)) %>%
  mutate(state_income_screen = case_when(
    estimate <= state_cutoff ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(income_screen = case_when(
    localized_income_screen == 1 | state_income_screen == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  select(GEOID, estimate, rounded_hh_size, local_low_income_threshold, localized_income_screen, state_income_screen, income_screen) %>%
  rename("median_hh_income" = "estimate")

###

### Race/ethnicity data ###

# Block decennial census redistricting data
redistricting_data <- read.csv("state_06_PL94_2020_Legacy_BLOCK_P12345H1_DOJ.csv") %>%
  mutate(GEOID20 = substr(GEOID, 10, 24)) %>%
  mutate(block_pct_non_white = (1 - (NH_Wht / Population.P1))) %>%
  select(GEOID20, block_pct_non_white)
redistricting_data$block_pct_non_white[redistricting_data$block_pct_non_white == "NaN"] <- NA

# Block group ACS race/ethnicity data
acs_white <- get_acs(
  geography = geography,
  variables = "B03002_003",
  year = year,
  state = state,
  geometry = FALSE) %>%
  select(GEOID, estimate) %>%
  rename("white_population" = "estimate")

acs_total <- get_acs(
  geography = geography,
  variables = "B03002_001",
  year = year,
  state = state,
  geometry = FALSE) %>%
  select(GEOID, estimate) %>%
  rename("total_population" = "estimate")

acs_race_ethnicity <- merge(acs_white,
                            acs_total,
                            by = "GEOID",
                            all = T) %>%
  mutate(bg_pct_non_white = 1 - (white_population / total_population)) %>%
  select(GEOID, bg_pct_non_white)

### Assemble the demographic overlay ###
demographic_overlay <- merge(eqi_blocks,
                             income_df,
                             by.x = "bg_id",
                             by.y = "GEOID",
                             all.x = T)

demographic_overlay <- merge(demographic_overlay,
                             redistricting_data,
                             by = "GEOID20",
                             all.x = T)

demographic_overlay <- merge(demographic_overlay,
                             acs_race_ethnicity,
                             by.x = "bg_id",
                             by.y = "GEOID",
                             all.x = T)


### Traffic Exposure ###

setwd("C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Data/TIMS_CSVS")
files = dir()

# Function to filter csv by category and only keep relevant variables
combineFiles = function(filename) {
  data_ped = read.csv(file=filename, header=T, as.is=T, na.strings=c("NA")) %>%
    filter(!is.na(POINT_X)) %>%
    filter(!is.na(POINT_Y))
}

# For loop to go through each csv file, filter it, and combine into one file
out = NULL
for (i in files) {
  data_temp = combineFiles(i)
  out = rbind.data.frame(out, data_temp)
  print(i)
}

write.csv(out, "C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Data/TIMS_RAW.csv", na = "")

rm(data_temp, out, i, files, combineFiles)

# Read in safety data cleaned in ArcGIS
setwd("C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Data")

safety_data_all <- read.csv("Crash_Score_ALL.csv") %>%
  select(GEOID20, SUM_accident_score) %>%
  rename("crash_score_all" = "SUM_accident_score") %>%
  filter(crash_score_all > 0)
safety_data_all$GEOID20 <- as.character(paste0("0", safety_data_all$GEOID20))
safety_data_all <- merge(safety_data_all,
                         eqi_blocks,
                         by = "GEOID20",
                         all.x = T) %>%
  mutate(crash_density_all = crash_score_all / (ALAND20 * 3.86102e-7)) %>%
  mutate(crash_percentile_all = percent_rank(crash_density_all)) %>%
  select(GEOID20, crash_score_all, crash_density_all, crash_percentile_all)

safety_data_keep <- read.csv("Crash_Score_KEEP.csv") %>%
  select(GEOID20, SUM_accident_score) %>%
  rename("crash_score_local" = "SUM_accident_score") %>%
  filter(crash_score_local > 0)
safety_data_keep$GEOID20 <- as.character(paste0("0", safety_data_keep$GEOID20))
safety_data_keep <- merge(safety_data_keep,
                         eqi_blocks,
                         by = "GEOID20",
                         all.x = T) %>%
  mutate(crash_density_local = crash_score_local / (ALAND20 * 3.86102e-7)) %>%
  mutate(crash_percentile_local = percent_rank(crash_density_local)) %>%
  select(GEOID20, crash_score_local, crash_density_local, crash_percentile_local)

# Merge with demographic overlay
demographic_overlay_temp <- demographic_overlay %>%
  select(GEOID20, ALAND20)

crash_indicator <- merge(demographic_overlay_temp,
                         safety_data_all,
                         by = "GEOID20",
                         all.x = T)

crash_indicator <- merge(crash_indicator,
                         safety_data_keep,
                         by = "GEOID20",
                         all.x = T)

crash_indicator <- crash_indicator %>%
  select(GEOID20, crash_score_all, crash_score_local, crash_density_all, crash_density_local, crash_percentile_all, crash_percentile_local)


# Read in AADT exposure data from ArcGIS
aadt_raw <- read.csv("AADT_12062022.csv")
aadt_raw$GEOID <- as.character(paste0("0", aadt_raw$GEOID))
aadt_raw[is.na(aadt_raw)] <- 0

aadt_cleaned <- aadt_raw %>%
  mutate(p_50_m = AADT10) %>%
  mutate(p_100_m = AADT9 - AADT10) %>%
  mutate(p_150_m = AADT8 - AADT9) %>%
  mutate(p_200_m = AADT7 - AADT8) %>%
  mutate(p_250_m = AADT6 - AADT7) %>%
  mutate(p_300_m = AADT5 - AADT6) %>%
  mutate(p_350_m = AADT4 - AADT5) %>%
  mutate(p_400_m = AADT3 - AADT4) %>%
  mutate(p_450_m = AADT2 - AADT3) %>%
  mutate(P_500_m = AADT1 - AADT2) %>%
  mutate(p_50m_w = p_50_m * 1) %>%
  mutate(p_100m_w = p_100_m * .5) %>%
  mutate(p_150m_w = p_150_m * .33) %>%
  mutate(p_200m_w = p_200_m * .25) %>%
  mutate(p_250m_w = p_250_m * .2) %>%
  mutate(p_300m_w = p_300_m * .16) %>%
  mutate(p_350m_w = p_350_m * .14) %>%
  mutate(p_400m_w = p_400_m * .125) %>%
  mutate(p_450m_w = p_450_m * .111) %>%
  mutate(p_500m_w = P_500_m * .1) %>%
  mutate(weighted_aadt_score =
           p_50m_w +
           p_100m_w +
           p_150m_w +
           p_200m_w +
           p_250m_w +
           p_300m_w +
           p_350m_w +
           p_400m_w +
           p_450m_w +
           p_500m_w) %>%
  select(GEOID, weighted_aadt_score) %>%
  filter(weighted_aadt_score > 0) %>%
  mutate(traffic_proximity_and_volume_percentile = percent_rank(weighted_aadt_score))

traffic_exposure_indicator <- merge(demographic_overlay_temp,
                                    aadt_cleaned,
                                    by.x = "GEOID20",
                                    by.y = "GEOID",
                                    all.x = T) %>%
  select(GEOID20, weighted_aadt_score, traffic_proximity_and_volume_percentile)

### Access to Destinations Screen
setwd("C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Data")
# Auto work
access_auto_work <- read.csv("AUTO_WORK_45min.csv")
access_auto_work$GEOID20 <- as.character(paste0("0", access_auto_work$GEOID20))
access_auto_work <- access_auto_work %>%
  rename("access_auto_work" = "MEAN") %>%
  select(GEOID20, access_auto_work)

# Multimodal work
access_multimodal_work <- read.csv("MULTIMODAL_WORK_45min.csv")
access_multimodal_work$GEOID20 <- as.character(paste0("0", access_multimodal_work$GEOID20))
access_multimodal_work <- access_multimodal_work %>%
  rename("access_multimodal_work" = "MEAN") %>%
  select(GEOID20, access_multimodal_work)

# Auto Non-Work
access_auto_nonwork <- read.csv("AUTO_POI_45min.csv")
access_auto_nonwork$GEOID20 <- as.character(paste0("0", access_auto_nonwork$GEOID20))
access_auto_nonwork <- access_auto_nonwork %>%
  rename("access_auto_nonwork" = "MEAN") %>%
  select(GEOID20, access_auto_nonwork)

# Multimodal Non-Work
access_multimodal_nonwork <- read.csv("MULTIMODAL_POI_45min.csv")
access_multimodal_nonwork$GEOID20 <- as.character(paste0("0", access_multimodal_nonwork$GEOID20))
access_multimodal_nonwork <- access_multimodal_nonwork %>%
  rename("access_multimodal_nonwork" = "MEAN") %>%
  select(GEOID20, access_multimodal_nonwork)

# Work ratio
access_work <- merge(access_auto_work,
                     access_multimodal_work,
                     by = "GEOID20",
                     all = T) %>%
  mutate(access_ratio_work = access_multimodal_work / access_auto_work)

access_work$access_ratio_work[access_work$access_auto_work == 0] <- 0
access_work$access_ratio_work[access_work$access_multimodal_work > access_work$access_auto_work] <- 0
access_work[is.na(access_work)] <- 0

# Non_Work ratio
access_nonwork <- merge(access_auto_nonwork,
                     access_multimodal_nonwork,
                     by = "GEOID20",
                     all = T) %>%
  mutate(access_ratio_nonwork = access_multimodal_nonwork / access_auto_nonwork)

access_nonwork$access_ratio_nonwork[access_nonwork$access_auto_nonwork == 0] <- 0
access_nonwork$access_ratio_nonwork[access_nonwork$access_multimodal_nonwork > access_nonwork$access_auto_nonwork] <- 0
access_nonwork[is.na(access_nonwork)] <- 0

access <- merge(access_work,
                access_nonwork,
                by = "GEOID20",
                all = T)

### Final EQI Assembly
EQI <- blocks %>%
  filter(ALAND20 > 0) %>%
  st_drop_geometry() %>%
  select(GEOID20)

EQI <- merge(EQI,
             demographic_overlay,
             by = "GEOID20",
             all.x = T)

EQI <- merge(EQI,
             crash_indicator,
             by = "GEOID20",
             all.x = T)

EQI <- merge(EQI,
             traffic_exposure_indicator,
             by = "GEOID20",
             all.x = T)

EQI <- merge(EQI,
             access,
             by = "GEOID20",
             all.x = T)

write.csv(EQI, "C:/Users/YourUsername/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/Outputs/EQI_12142022.csv", na = "")





                          