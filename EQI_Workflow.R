### EQI 1.0 ###

# Load libraries
library(tidycensus)
library(tigris)
library(dplyr)
library(mapview)
library(sf)

blocks <- blocks(state = 06, year = 2020)

eqi_blocks <- blocks %>%
  filter(ALAND20 > 0) %>%
  st_drop_geometry() %>%
  select(GEOID20, ALAND20, POP20) %>%
  mutate(bg_id = substr(GEOID20, 1, 12))


### DEMOGRAPHIC OVERLAY ###

# Set global parameters
year = 2021
state = 06
geography = "block group"
product = "acs5"

setwd("")

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
income_limits <- read.csv("2021_IncomeLimits.csv")

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

### Assemble the demographic overlay ###
demographic_overlay <- merge(eqi_blocks,
                             income_df,
                             by.x = "bg_id",
                             by.y = "GEOID",
                             all.x = T)


### Traffic Exposure ###

setwd("")
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

write.csv(out, "")

rm(data_temp, out, i, files, combineFiles)

# Read in safety data cleaned in ArcGIS
setwd("")

safety_data_keep <- read.csv("") %>%
  select(GEOID20, SUM_Weight) %>%
  rename("crash_score_local" = "SUM_Weight") %>%
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
                         safety_data_keep,
                         by = "GEOID20",
                         all.x = T)

crash_indicator <- crash_indicator %>%
  select(GEOID20, crash_score_local, crash_density_local, crash_percentile_local)


# Read in AADT exposure data from ArcGIS
aadt_raw <- read.csv("")
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
setwd("")

# PED RATIO
ped_ratio <- read.csv("")
ped_ratio$GEOID20 <- as.character(paste0("0", ped_ratio$GEOID20))
ped_ratio <- ped_ratio %>%
  rename("PED_RATIO" = "MEAN") %>%
  select(GEOID20, PED_RATIO)

# BIKE RATIO
bike_ratio <- read.csv("")
bike_ratio$GEOID20 <- as.character(paste0("0", bike_ratio$GEOID20))
bike_ratio <- bike_ratio %>%
  rename("BIKE_RATIO" = "MEAN") %>%
  select(GEOID20, BIKE_RATIO)

# TRANSIT RATIO
transit_ratio_jobs <- read.csv("")
transit_ratio_jobs$GEOID20 <- as.character(paste0("0", transit_ratio_jobs$GEOID20))
transit_ratio_jobs <- transit_ratio_jobs %>%
  rename("TRANSIT_RATIO_JOBS" = "MEAN") %>%
  select(GEOID20, TRANSIT_RATIO_JOBS)

transit_ratio_pois <- read.csv("")
transit_ratio_pois$GEOID20 <- as.character(paste0("0", transit_ratio_pois$GEOID20))
transit_ratio_pois <- transit_ratio_pois %>%
  rename("TRANSIT_RATIO_POIs" = "MEAN") %>%
  select(GEOID20, TRANSIT_RATIO_POIs)


blocks_access <- eqi_blocks
access <- merge(blocks_access,
                ped_ratio,
                by = "GEOID20",
                all.X = T)
access <- merge(access,
                bike_ratio,
                by = "GEOID20",
                all.X = T)
access <- merge(access,
                transit_ratio_jobs,
                by = "GEOID20",
                all.X = T)

access <- merge(access,
                transit_ratio_pois,
                by = "GEOID20",
                all.X = T)

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

EQI$PED_RATIO[is.na(EQI$PED_RATIO)] <- 0
EQI$BIKE_RATIO[is.na(EQI$BIKE_RATIO)] <- 0
EQI$TRANSIT_RATIO_JOBS[is.na(EQI$TRANSIT_RATIO_JOBS)] <- 0
EQI$TRANSIT_RATIO_POIs[is.na(EQI$TRANSIT_RATIO_POIs)] <- 0

EQI <- EQI %>%
  select(GEOID20, median_hh_income, rounded_hh_size, local_low_income_threshold, localized_income_screen, state_income_screen,income_screen, crash_score_local, crash_density_local,
         crash_percentile_local, weighted_aadt_score, traffic_proximity_and_volume_percentile, PED_RATIO, BIKE_RATIO, TRANSIT_RATIO_JOBS, TRANSIT_RATIO_POIs)

write.csv(EQI, "", na = "")                       
