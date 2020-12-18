#### Script Description Header ####

# File Name: CAISO 2019 DLAP_PGAE-APND RT5M LMP Data Cleaning.R
# File Location: "~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Wholesale Price Data/Clean Wholesale RT5M LMP Data"
# Project: PG&E JARP GRC Testimony
# Description: Cleans 2019 wholesale RT5M data for use in PG&E JARP analysis.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Wholesale Price Data/Clean Wholesale RT5M LMP Data")
Code_WD <- getwd()

setwd("../Raw Wholesale RT5M LMP Data")
Data_WD <- getwd()


#### Load and Clean CAISO RT5M Wholesale Price Data ####
# Data is for PG&E DLAP, Real-Time 5-Minute Market
# Source: http://oasis.caiso.com/mrioasis/logon.do

CAISO_RT5M_Files <- list.files(pattern = ".csv")

Raw_CAISO_RT5M_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), VALUE = numeric(), stringsAsFactors = F)
  
for(CAISO_RT5M_File in CAISO_RT5M_Files){
  
  Raw_CAISO_RT5M_Single <- read.csv(CAISO_RT5M_File) %>%
    filter(XML_DATA_ITEM == "LMP_PRC") %>%
    select(INTERVALSTARTTIME_GMT, VALUE)
  
  Raw_CAISO_RT5M_Joined <- rbind(Raw_CAISO_RT5M_Joined, Raw_CAISO_RT5M_Single)
  
}

rm(Raw_CAISO_RT5M_Single, CAISO_RT5M_File, CAISO_RT5M_Files)

Clean_2019_DLAP_PGAE_APND_RT5M_LMP <- Raw_CAISO_RT5M_Joined %>%
  mutate(dttm = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(dttm = with_tz(dttm, tz = "America/Los_Angeles")) %>%
  mutate(LMP_RT5M = VALUE/1000) %>% # Convert from $/MWh to $/kWh
  select(dttm, LMP_RT5M) %>%
  arrange(dttm)

rm(Raw_CAISO_RT5M_Joined)

#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(Clean_2019_DLAP_PGAE_APND_RT5M_LMP, "Clean_2019_DLAP_PGAE_APND_RT5M_LMP.rds")