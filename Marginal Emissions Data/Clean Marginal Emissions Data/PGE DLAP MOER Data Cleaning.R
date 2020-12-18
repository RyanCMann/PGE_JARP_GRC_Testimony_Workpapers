#### Script Description Header ####

# File Name: PGE DLAP MOER Data Cleaning.R
# File Location: "~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Marginal Emissions Data/Clean Marginal Emissions Data"
# Project: PG&E JARP GRC Testimony
# Description: Cleans marginal emissions data for use in PG&E JARP analysis.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Marginal Emissions Data/Clean Marginal Emissions Data")
Code_WD <- getwd()

setwd("../Raw Marginal Emissions Data")
Data_WD <- getwd()


#### Load and Clean WattTime SGIP Historical GHG Data ####
# Data is for PG&E DLAP
# Source: sgipsignal.com/sgipmoer/?ba=SGIP_CAISO_PGE

WattTime_Files <- list.files()

Raw_WattTime_Joined <- data.frame(timestamp = character(), MOER = numeric(), MOER.version = numeric(), 
                                  frequency = numeric(), stringsAsFactors = F)

for(WattTime_File in WattTime_Files){
  
  Raw_WattTime_Single <- read.csv(WattTime_File)
  
  Raw_WattTime_Joined <- rbind(Raw_WattTime_Joined, Raw_WattTime_Single)
  
}

rm(Raw_WattTime_Single, WattTime_File, WattTime_Files)

Clean_WattTime_Joined <- Raw_WattTime_Joined %>%
  mutate(dttm = as.POSIXct(gsub("T", " ", substr(timestamp, 1, 16)), tz = "UTC")) %>%
  mutate(dttm = with_tz(dttm, tz = "America/Los_Angeles")) %>%
  rename(moer = MOER) %>%
  select(dttm, moer) %>%
  filter(lubridate::year(dttm) == 2019) %>%
  group_by(dttm) %>%
  summarize(moer = mean(moer)) # Remove duplicates by averaging.

rm(Raw_WattTime_Joined)

# 3 datapoints are missing
Clean_PGE_SGIP_MOER <- data.frame(dttm = seq.POSIXt(from = as.POSIXct("2019-01-01 00:00", tz = "America/Los_Angeles"),
                                                         to = as.POSIXct("2019-12-31 23:55", tz = "America/Los_Angeles"),
                                                         by = "5 min")) %>%
  left_join(Clean_WattTime_Joined, by = "dttm") %>%
  mutate(moer = ifelse(is.na(moer), lag(moer, 12*24*7), moer)) # Fill with data from prior week.

rm(Clean_WattTime_Joined)


#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(Clean_PGE_SGIP_MOER, "Clean_PGE_SGIP_MOER.rds")