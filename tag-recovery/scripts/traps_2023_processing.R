#################################################################
# Code to process data from 2023 Carp Lake burbot trap-setting 
# Liz Hirsch
# Dec 5, 2023
#################################################################
library(tidyverse)
traps_2023 <- read_csv("carp_lk/data/raw/2023_burbot_trap_sets.csv")

traps_2023 <- rename(traps_2023, lake_id = `Lake name`, trap_id = `Trap waypoint`, 
                     start_date = `Start date`, start_time = `Start time`, end_date = `End date`, 
                     end_time = `End time`, lat = `Lat`, lon = `Lon`, depth = `Depth (m)`, temp = `Temperature (degrees C)`, 
                     species = `Species`, num_fish = `Count fish`, floy_id = `Burbot Floy number`, 
                     length = `Burbot length (cm)`, weight = `Burbot weight (g)`, comment = `Comments`)

#identify burbot catches
species <- traps_2023$species
num_fish <- traps_2023$num_fish
traps_2023 <- mutate(traps_2023, bb_count = ifelse(species == "BB", num_fish, 0))

traps_2023 <- mutate(traps_2023, start_dt = as.POSIXct(paste(traps_2023$start_date, traps_2023$start_time), format="%m/%d/%y %H:%M:%S"),
                     end_dt = as.POSIXct(paste(traps_2023$end_date, traps_2023$end_time), format="%m/%d/%y %H:%M:%S"),
                     trap_dur = difftime(end_dt, start_dt), 
                     cpue = bb_count / as.numeric(trap_dur, na.rm = TRUE))
traps_2023 <- traps_2023 %>%
  mutate(bb_count = if_else(is.na(bb_count), 0, bb_count))

mean(traps_2023$bb_count)
sd(traps_2023$bb_count) 


# Summary stats for Carp Lake 2023:
total_bb_2023 <- sum(traps_2023$bb_count, na.rm = TRUE)
trap_hrs_2023 <- as.numeric(sum(traps_2023$trap_dur, na.rm = TRUE))
carp_cpue_2023 <- total_bb_2023 / trap_hrs_2023
carp_cpue_2023

ggplot(traps_2023, aes(x = as.character(traps_2023$zone), y = cpue)) +
  geom_boxplot() +
  xlab("Zone") + 
  ylab("CPUE in burbot weight (g) /trap hour") +
  ggtitle("Carp Lake CPUE by zone, 2023")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank())

