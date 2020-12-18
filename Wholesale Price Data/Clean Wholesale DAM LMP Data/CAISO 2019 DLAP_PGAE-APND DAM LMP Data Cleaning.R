#### Script Description Header ####

# File Name: CAISO 2019 DLAP_PGAE-APND DAM LMP Data Cleaning.R
# File Location: "~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Wholesale Price Data/Clean Wholesale DAM LMP Data"
# Project: PG&E JARP GRC Testimony
# Description: Cleans 2019 wholesale DAM data for use in PG&E JARP analysis.

#### Load Packages
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Wholesale Price Data/Clean Wholesale DAM LMP Data")
Code_WD <- getwd()

setwd("../Raw Wholesale DAM LMP Data")
Data_WD <- getwd()

#### Load and Clean CAISO DAM Wholesale Price Data ####
# Data is for PG&E DLAP, Day-Ahead Market
# Source: http://oasis.caiso.com/mrioasis/logon.do

CAISO_DAM_Files <- list.files(pattern = ".csv")

Raw_CAISO_DAM_Joined <- data.frame(INTERVALSTARTTIME_GMT = character(), MW = numeric(), stringsAsFactors = F)
  
for(CAISO_DAM_File in CAISO_DAM_Files){
  
  Raw_CAISO_DAM_Single <- read.csv(CAISO_DAM_File) %>%
    filter(XML_DATA_ITEM == "LMP_PRC") %>%
    select(INTERVALSTARTTIME_GMT, MW)
  
  Raw_CAISO_DAM_Joined <- rbind(Raw_CAISO_DAM_Joined, Raw_CAISO_DAM_Single)
  
}

rm(Raw_CAISO_DAM_Single, CAISO_DAM_File, CAISO_DAM_Files)

Clean_2019_DLAP_PGAE_APND_DAM_LMP <- Raw_CAISO_DAM_Joined %>%
  mutate(dttm = as.POSIXct(gsub("T", " ", substr(INTERVALSTARTTIME_GMT, 1, 16)), tz = "UTC")) %>%
  mutate(dttm = with_tz(dttm, tz = "America/Los_Angeles")) %>%
  mutate(LMP_DAM = MW/1000) %>% # Convert from $/MWh to $/kWh
  select(dttm, LMP_DAM) %>%
  arrange(dttm)

rm(Raw_CAISO_DAM_Joined)

#### Save Final Dataset ####
setwd(Code_WD)
saveRDS(Clean_2019_DLAP_PGAE_APND_DAM_LMP, "Clean_2019_DLAP_PGAE_APND_DAM_LMP.rds")