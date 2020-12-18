#### Script Description Header ####

# File Name: PG&E JARP GRC Testimony Workpapers.R
# File Location: "~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/PG&E Data Request 1 to JARP")
# Project: PG&E JARP GRC Testimony
# Description: Work papers for JARP's prepared testimony, in response to PG&E's Data Request #1 to JARP.

#### Load Packages ####
library(tidyverse)
library(lubridate)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/PGE_JARP_GRC_Testimony_Workpapers/PG&E Data Request 1 to JARP")
Code_WD <- getwd()

setwd("../")
Main_WD <- getwd()

setwd(file.path(Main_WD, "Rates"))
Rates_WD <- getwd()

setwd(file.path(Main_WD, "Wholesale Price Data", "Clean Wholesale RT5M LMP Data"))
Wholesale_RT5M_Price_WD <- getwd()

setwd(file.path(Main_WD, "Wholesale Price Data", "Clean Wholesale DAM LMP Data"))
Wholesale_DAM_Price_WD <- getwd()

setwd(file.path(Main_WD, "Marginal Emissions Data", "Clean Marginal Emissions Data"))
Emissions_WD <- getwd()


#### Load Retail Rate Data ####

setwd(Rates_WD)
PGE_B19 <- readRDS(file.path(Rates_WD, "PG&E B-19", "2019", "5-Minute Data", "PGE_B19.rds"))


#### Load Wholesale Real-Time 5-Minute Market Price Data ####

setwd(Wholesale_RT5M_Price_WD)
Clean_2019_DLAP_PGAE_APND_RT5M_LMP <- readRDS("Clean_2019_DLAP_PGAE_APND_RT5M_LMP.rds")


#### Load Wholesale Day-Ahead Market Price Data ####

setwd(Wholesale_DAM_Price_WD)
Clean_2019_DLAP_PGAE_APND_DAM_LMP <- readRDS("Clean_2019_DLAP_PGAE_APND_DAM_LMP.rds")

Clean_2019_DLAP_PGAE_APND_DAM_LMP_5_min <-  approx(Clean_2019_DLAP_PGAE_APND_DAM_LMP$dttm,
                                                   Clean_2019_DLAP_PGAE_APND_DAM_LMP$LMP_DAM,
                                                   Clean_2019_DLAP_PGAE_APND_RT5M_LMP$dttm,
                                                   method = "constant",
                                                   f = 0,
                                                   rule = 2,
                                                   ties = mean)

Clean_2019_DLAP_PGAE_APND_LMP <- Clean_2019_DLAP_PGAE_APND_RT5M_LMP %>%
  mutate(LMP_DAM = Clean_2019_DLAP_PGAE_APND_DAM_LMP_5_min$y)

rm(Clean_2019_DLAP_PGAE_APND_RT5M_LMP, Clean_2019_DLAP_PGAE_APND_DAM_LMP, Clean_2019_DLAP_PGAE_APND_DAM_LMP_5_min)


#### Load Marginal Emissions Data ####

setwd(Emissions_WD)
Clean_PGE_SGIP_MOER <- readRDS("Clean_PGE_SGIP_MOER.rds")


#### Join Datasets Together ####

Joined_CAISO_WT_PGE <- PGE_B19 %>%
  select(dttm, Energy_Rates) %>%
  left_join(Clean_2019_DLAP_PGAE_APND_LMP, by = "dttm") %>%
  left_join(Clean_PGE_SGIP_MOER, by = "dttm")

rm(PGE_B19, Clean_2019_DLAP_PGAE_APND_LMP, Clean_PGE_SGIP_MOER)

setwd(Code_WD)
write.csv(Joined_CAISO_WT_PGE, "Joined_CAISO_WT_PGE.csv", row.names = F)


#### Count Real-Time 5-Minute Wholesale Price Spikes by Hour of Day ####

Top_600_Price_Spike_Count <- Joined_CAISO_WT_PGE %>%
  top_n(600, LMP_RT5M) %>%
  mutate(Hour_Beginning = lubridate::hour(dttm)) %>%
  group_by(Hour_Beginning) %>%
  summarize(Price_Spike_Count = n())

write.csv(Top_600_Price_Spike_Count, "Top_600_Price_Spike_Count.csv", row.names = F)

# Price Spikes Outside Peak Period (4:00 pm - 9:00 pm)
Price_Spikes_Outside_Peak_Period <- Top_600_Price_Spike_Count %>%
  filter(!(Hour_Beginning %in% seq(16, 20)))
Price_Spikes_Outside_Peak_Period_Percentage <- sum(Price_Spikes_Outside_Peak_Period$Price_Spike_Count)/600 # 273/600 = 45.5%
rm(Price_Spikes_Outside_Peak_Period, Price_Spikes_Outside_Peak_Period_Percentage)

# Price Spikes Outside Current Critical Peak Pricing Window (2:00 pm - 6:00 pm)
Price_Spikes_Outside_Current_CPP <- Top_600_Price_Spike_Count %>%
  filter(!(Hour_Beginning %in% seq(14, 17)))
Price_Spikes_Outside_Current_CPP_Percentage <- sum(Price_Spikes_Outside_Current_CPP$Price_Spike_Count)/600 # 273/600 = 78.3%
rm(Price_Spikes_Outside_Current_CPP, Price_Spikes_Outside_Current_CPP_Percentage)

# Price Spikes Outside Future Critical Peak Pricing Window (5:00 pm - 8:00 pm)
Price_Spikes_Outside_Future_CPP <- Top_600_Price_Spike_Count %>%
  filter(!(Hour_Beginning %in% seq(17, 19)))
Price_Spikes_Outside_Future_CPP_Percentage <- sum(Price_Spikes_Outside_Future_CPP$Price_Spike_Count)/600 # 273/600 = 51.3%
rm(Price_Spikes_Outside_Future_CPP, Price_Spikes_Outside_Future_CPP_Percentage)
rm(Top_600_Price_Spike_Count)


#### Plot LMPs, MOERs, and TOU Retail Rates for Selected Dates ####

Selected_Plot_Date <- as.Date("2019-03-11", tz = "America/Los_Angeles")
# Selected_Plot_Date <- as.Date("2019-07-22", tz = "America/Los_Angeles")

Joined_CAISO_WT_PGE_Long_Filtered <- Joined_CAISO_WT_PGE %>%
  filter(as.Date(dttm, tz = "America/Los_Angeles") == Selected_Plot_Date) %>%
  gather(key = "Datastream", value = "Value", LMP_RT5M, moer, Energy_Rates)
  
ggplot(Joined_CAISO_WT_PGE_Long_Filtered, aes(x = dttm, y = Value)) + 
  geom_line(aes(color = Datastream)) +
  facet_grid(Datastream ~ ., scales = "free_y", switch = "y", labeller = as_labeller(c(LMP_RT5M = "LMP ($/kWh)", moer = "Emissions (kg CO2/kWh)", Energy_Rates = "B-19 Secondary Rate ($/kWh)"))) +
  xlab("Date-Time") +
  ylab(NULL) +
  ggtitle("PG&E TOU Rate, LMP, and Emissions") +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(legend.position = "none") +
  scale_x_datetime(limits = c(Joined_CAISO_WT_PGE_Long_Filtered$dttm[1], 
                              Joined_CAISO_WT_PGE_Long_Filtered$dttm[nrow(Joined_CAISO_WT_PGE_Long_Filtered)]),
                   expand = c(0, 0))

ggsave(filename = file.path(Code_WD, paste0("PG&E LMP Emissions TOU Comparison Plot ", as.character(Selected_Plot_Date), ".png")),
       width = 11, height = 8.5, units = "in")

rm(Joined_CAISO_WT_PGE_Long_Filtered)


#### Plot PG&E DLAP RT5M vs. DAM LMPs ####

Selected_Plot_Date <- as.Date("2019-07-22", tz = "America/Los_Angeles")

Joined_CAISO_WT_PGE_Long_Filtered <- Joined_CAISO_WT_PGE %>%
  filter(as.Date(dttm, tz = "America/Los_Angeles") == Selected_Plot_Date) %>%
  gather(key = "Datastream", value = "Value", LMP_RT5M, LMP_DAM)

ggplot(Joined_CAISO_WT_PGE_Long_Filtered, aes(x = dttm, y = Value)) + 
  geom_line(aes(color = Datastream)) +
  xlab("Date-Time") +
  ylab("LMP ($/kWh)") +
  ggtitle("PG&E Day-Ahead vs. Real-Time LMP Comparison") +
  scale_color_discrete(name = "Legend", labels = c("Day-Ahead Market", "Real-Time 5-Minute Market")) +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_x_datetime(limits = c(Joined_CAISO_WT_PGE_Long_Filtered$dttm[1], 
                              Joined_CAISO_WT_PGE_Long_Filtered$dttm[nrow(Joined_CAISO_WT_PGE_Long_Filtered)]),
                   expand = c(0, 0))

ggsave(filename = file.path(Code_WD, paste0("PG&E Day-Ahead vs. Real-Time LMP Comparison ", as.character(Selected_Plot_Date), ".png")),
       width = 11, height = 8.5, units = "in")

rm(Joined_CAISO_WT_PGE_Long_Filtered)