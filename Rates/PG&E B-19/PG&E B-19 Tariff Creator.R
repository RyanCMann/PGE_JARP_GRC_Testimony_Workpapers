#### Script Description Header ####

# File Name: PG&E B-19 Tariff Creator.R
# File Location: "~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Rates/PG&E B-19"
# Project: PG&E JARP GRC Testimony
# Description: Creates PG&E B-19 retail rate for use in PG&E JARP modeling.

#### User Inputs ####

# This script generates a time-series vector for the user's choice of year.
# This value must be input by the user to match the desired year to be modeled.

Data_Year <- 2019

Data_Timestep_Length <- 5 # Timestep length, in minutes


#### Load Packages ####
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/Rates/PG&E B-19")
Code_WD <- getwd()

Clean_Rate_Data_WD <- file.path(Code_WD, Data_Year,paste0(Data_Timestep_Length, "-Minute Data"))


#### Calculate PG&E B-19 Energy Rate Values ####
# Values are for 2020-11-01 PG&E B-19, Secondary Voltage, in $USD. (https://www.pge.com/tariffs/assets/pdf/tariffbook/ELEC_SCHEDS_B-19.pdf)
# Note: Power factor charges are not included in this analysis. 

Summer_Peak_Rate <- 0.16504
Summer_Partial_Peak_Rate <- 0.13529
Summer_Off_Peak_Rate <- 0.11424

Winter_Peak_Rate <- 0.14614
Winter_Off_Peak_Rate <- 0.11416
Winter_Super_Off_Peak_Rate <- 0.07125

Start_Date_Time <- as.POSIXct(paste0(Data_Year, "-01-01 00:00:00"), tz = "America/Los_Angeles")
End_Date_Time <- as.POSIXct(paste0(Data_Year, "-12-31 23:55:00"), tz = "America/Los_Angeles")

PGE_B19 <- data.frame(dttm = seq.POSIXt(Start_Date_Time, End_Date_Time, by = paste(Data_Timestep_Length, "min")))


# Holidays for 2019
# Source: https://www.PGE.com/whenmatters
# The Time-of-Use holidays are:
# New Year's Day (January 1)
# President's Day (third Monday in February)
# Memorial Day (last Monday in May)
# Independence Day (July 4)
# Labor Day (first Monday in September)
# Veterans Day (November 11)
# Thanksgiving Day (fourth Thursday in November)
# Christmas Day (December 25).
# When a holiday listed above falls on Sunday, the following Monday is recognized. No change will be made for holidays falling on Saturday.

New_Years_Day <- "2019-01-01"
Presidents_Day <- "2019-02-18"
Memorial_Day <- "2019-05-27"
Independence_Day <- "2019-07-04"
Labor_Day <- "2019-09-02"
Veterans_Day <- "2019-11-11"
Thanksgiving_Day <- "2019-11-28"
Christmas_Day <- "2019-12-25"

Holidays_List <- as.Date(c(New_Years_Day, Presidents_Day, Memorial_Day, Independence_Day, Labor_Day, Veterans_Day, Thanksgiving_Day, Christmas_Day), tz = "America/Los_Angeles")

rm(New_Years_Day, Presidents_Day, Memorial_Day, Independence_Day, Labor_Day, Veterans_Day, Thanksgiving_Day, Christmas_Day)

# When a holiday listed above falls on Sunday, the following Monday is recognized. No change will be made for holidays falling on Saturday.

for(Holiday_Iter in seq_along(Holidays_List)){
  if(weekdays(Holidays_List[Holiday_Iter]) == "Sunday"){
    Holidays_List = c(Holidays_List, Holidays_List[Holiday_Iter] + 1)
  }
}
rm(Holiday_Iter)

PGE_B19 <- PGE_B19 %>%
  mutate(Month = month(dttm)) %>%
  mutate(Season = ifelse(Month %in% c(6:9), "Summer", "Winter")) %>% # Dates between June 1 and September 30 are considered "Summer"
  mutate(Weekday_Weekend_Holiday = ifelse(weekdays(dttm) %in% c("Saturday", "Sunday") |
                                            as.Date(dttm, tz = "America/Los_Angeles") %in% Holidays_List, "Weekend/Holiday", "Weekday")) %>% # Monday-Friday = Weekday, Saturday & Sunday = Weekend
  mutate(Hour_Decimal = hour(dttm) + minute(dttm)/60) # ex. 8:30 am = 8.5

rm(Holidays_List)

# Summer Peak
# Summer Peak = June-September, 4:00 pm-9:00 pm.

PGE_B19 <- PGE_B19 %>%
  mutate(Summer_Peak_Binary = ifelse(Season == "Summer" & 
                                       Hour_Decimal >= (12+4) & Hour_Decimal < (12+9), 1, 0))

# Summer Partial-Peak
# Summer Partial Peak = 2:00 pm to 4:00 pm, 9:00 pm to 11:00 pm.

PGE_B19 <- PGE_B19 %>%
  mutate(Summer_Partial_Peak_Binary = ifelse(Season == "Summer" & 
                                               ((Hour_Decimal >= (12+2) & Hour_Decimal < (12+4)) |
                                                  (Hour_Decimal >= (12 + 9) & Hour_Decimal < (12 + 11))), 1, 0))

# Summer Off-Peak
# Summer Off Peak = 11:00 pm to 2:00 pm.
# In other words, if it's summer and not peak or partial peak, it's off-peak.

PGE_B19 <- PGE_B19 %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_Peak_Binary == 0 &
                                           Summer_Partial_Peak_Binary == 0, 1, 0))

# Winter Peak
# Winter Peak = October-May, 4:00 pm-9:00 pm.

PGE_B19 <- PGE_B19 %>%
  mutate(Winter_Peak_Binary = ifelse(Season == "Winter" & 
                                       Hour_Decimal >= (12+4) & Hour_Decimal < (12+9), 1, 0))

# Winter Super Off-Peak
# Winter Super Off-Peak = March-May, 9:00 am - 2:00 pm

PGE_B19 <- PGE_B19 %>%
  mutate(Winter_Super_Off_Peak_Binary = ifelse(Season == "Winter" &
                                                 month(dttm) %in% c(3:5) &
                                                 Hour_Decimal >= 9 & Hour_Decimal < (12+2), 1, 0))

# Winter Off-Peak
# Winter Off Peak = 9:00 pm to 4:00 pm in October-February, 
# 9:00 pm to 9:00 am and 2:00 pm to 4:00 pm in March-May weekdays.
# In other words, if it's winter and not peak or super off-peak, it's off-peak.

PGE_B19 <- PGE_B19 %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           Winter_Peak_Binary == 0 &
                                           Winter_Super_Off_Peak_Binary == 0, 1, 0))



# PG&E Daylight Savings Time Adjustment - Shift Time Period Definitions back 1 Hour in March/April and October/November

# In PG&E Schedule B-19, the peak/partial-peak/off-period time definitions do not shift in sync with Daylight Savings Time.
# As a result, they must be adjusted back by one hour during the time period "between the second Sunday in march and the first Sunday in April,
# and for the period between the last Sunday in October and the first Sunday in November."

# Note: the second date in each sequence is the day before the First Sunday in April/November. 

March_April_DST_Adjustment_Period_2019 <- seq.Date(as.Date("2019-03-10", tz = "America/Los_Angeles"), as.Date("2019-04-06", tz = "America/Los_Angeles"), by = "1 day")

October_November_DST_Adjustment_Period_2019 <- seq.Date(as.Date("2019-10-27", tz = "America/Los_Angeles"), as.Date("2019-11-02", tz = "America/Los_Angeles"), by = "1 day")

DST_Adjustment_Period_Dates <- c(March_April_DST_Adjustment_Period_2019, October_November_DST_Adjustment_Period_2019)

rm(March_April_DST_Adjustment_Period_2019, October_November_DST_Adjustment_Period_2019)


# Shift All Binary Period Flags Back by 1 Hour

# Number of timesteps in 1 hour = (60 minutes/Data_Timestep_Length)
PGE_B19 <- PGE_B19 %>%
  mutate(DST_Adjustment_Flag = ifelse(as.Date(dttm, tz = "America/Los_Angeles") %in% DST_Adjustment_Period_Dates, 1, 0)) %>%
  mutate(Summer_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Summer_Peak_Binary, n = (60/Data_Timestep_Length)), Summer_Peak_Binary)) %>% 
  mutate(Summer_Partial_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Summer_Partial_Peak_Binary, n = (60/Data_Timestep_Length)), Summer_Partial_Peak_Binary)) %>% 
  mutate(Summer_Off_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Summer_Off_Peak_Binary, n = (60/Data_Timestep_Length)), Summer_Off_Peak_Binary)) %>% 
  mutate(Winter_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Winter_Peak_Binary, n = (60/Data_Timestep_Length)), Winter_Peak_Binary)) %>%
  mutate(Winter_Super_Off_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Winter_Super_Off_Peak_Binary, n = (60/Data_Timestep_Length)), Winter_Super_Off_Peak_Binary)) %>%
  mutate(Winter_Off_Peak_Binary = ifelse(DST_Adjustment_Flag ==1, lag(Winter_Off_Peak_Binary, n = (60/Data_Timestep_Length)), Winter_Off_Peak_Binary))


# Check that all timesteps are accounted for, and that no timestep has a value of 1 in multiple columns

PGE_B19_Vector_Check <- PGE_B19 %>%
  mutate(Check_Sum = Summer_Peak_Binary + Summer_Partial_Peak_Binary + Summer_Off_Peak_Binary + Winter_Peak_Binary + Winter_Super_Off_Peak_Binary + Winter_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).

rm(PGE_B19_Vector_Check)


#### Energy Rates ####

PGE_B19 <- PGE_B19 %>%
  mutate(Energy_Rates = (Summer_Peak_Binary * Summer_Peak_Rate) + 
           (Summer_Partial_Peak_Binary * Summer_Partial_Peak_Rate) +
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Rate) +
           (Winter_Peak_Binary * Winter_Peak_Rate) +
           (Winter_Super_Off_Peak_Binary * Winter_Super_Off_Peak_Rate) +
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Rate))

rm(Summer_Peak_Rate, Summer_Partial_Peak_Rate, Summer_Off_Peak_Rate, Winter_Peak_Rate, Winter_Off_Peak_Rate, Winter_Super_Off_Peak_Rate)


#### Save Final Dataset ####
setwd(Clean_Rate_Data_WD)
saveRDS(PGE_B19, "PGE_B19.rds")