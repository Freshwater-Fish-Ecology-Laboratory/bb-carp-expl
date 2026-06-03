#Processing trap set data from 2021 (Fraser Lake only)
library(tidyverse)

#Read data file and filter to cod traps
traps_2021 <- read_csv("carp_lk/data/raw/Burbot trap set data.csv")%>%
  filter(`Method` == "CT")

colnames(traps_2021) <- c("trap_id", "zone", "start_month", "start_datetime", "end_datetime", "start_time", "end_time", "method", "Lat", "Lon", "habitat", "sample_design", "species", "num_fish", "count_burbot", "bait_type", "bait_wt", "depth", "temp", "comment", "trap_duration")

traps_2021 <- traps_2021 %>%
  mutate(lake_id = "Fraser Lake",
         trap_duration = as.numeric(trap_duration),
         cpue = count_burbot/trap_duration)
  
# Calculate mean and SD of burbot CPUE for Fraser Lake 2021:
mean_cpue <- mean(traps_2021$count_burbot, na.rm = TRUE) # mean CPUE = 0.007
sd_cpue <- sd(traps_2021$count_burbot, na.rm = TRUE) # SD = 0.016

# #Calculate total number of hours of cod trap effort
sum(traps_2021$trap_duration, na.rm = TRUE) # 5165.56 hours
# 
# #total number of burbot captured
sum(traps_2021$count_burbot) # 42 burbot



