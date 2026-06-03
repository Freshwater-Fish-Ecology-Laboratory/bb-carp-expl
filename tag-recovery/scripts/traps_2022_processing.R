#Code to process burbot trap set data and catch rates from 2022 fieldwork
library(tidyverse)

traps_2022 <- read_csv("carp_lk/data/raw/2022_burbot_trap_sets.csv")
#Rename columns
traps_2022 <- rename(traps_2022, lake_id = `Lake name`, trap_id = `Trap waypoint`, 
                        start_date = `Start date`, start_time = `Start time`, end_date = `End date`, 
                        end_time = `End time`, lat = `Lat`, lon = `Lon`, depth = `Depth (m)`, temp = `Temperature (degrees C)`, 
                        species = `Species`, num_fish = `Count fish`, floy_id = `Burbot Floy number`, 
                        length = `Burbot length (cm)`, weight = `Burbot weight (g)`, bait_type = `Bait type`,
                        bait_wt = `Bait weight (g)`, comment = `Comments`)

#Create a column for burbot count (do not want to count non-burbot species!)

traps_2022$num_fish <- as.numeric(traps_2022$num_fish)

traps_2022 <- mutate(traps_2022, bb_count = ifelse(species == "BB", num_fish, 0))

traps_2022 <- mutate(traps_2022, start_dt = as.POSIXct(paste(traps_2022$start_date, traps_2022$start_time), format="%m/%d/%y %H:%M:%S"),
         end_dt = as.POSIXct(paste(traps_2022$end_date, traps_2022$end_time), format="%m/%d/%y %H:%M:%S"),
         trap_dur = difftime(end_dt, start_dt), 
         cpue = bb_count / as.numeric(trap_dur, na.rm = TRUE))

#add a - sign in front of all longitudes
traps_2022$lon = traps_2022$lon*(-1)

# Calculate CPUE for each trap for each lake, then get mean and SD of CPUE by lake.
fraser <- traps_2022 %>%
  mutate(bb_count = if_else(is.na(bb_count), 0, bb_count)) %>%
  filter(lake_id == "Fraser Lake")

mean(fraser$bb_count) # mean CPUE = 0.185
sd(fraser$bb_count) # SD = 0.462

carp_spr <- traps_2022 %>%
  mutate(bb_count = if_else(is.na(bb_count), 0, bb_count)) %>%
  filter(lake_id == "Carp Lake") %>%
  filter(between(start_date, '5/01/22', '7/01/22'))

mean(carp_spr$bb_count) # mean CPUE = 0.030
sd(carp_spr$bb_count) # SD = 0.038

carp_fall <- traps_2022 %>%
  mutate(bb_count = if_else(is.na(bb_count), 0, bb_count)) %>%
  filter(lake_id == "Carp Lake") %>%
  filter(between(start_date, '10/20/22', '10/30/22'))

mean(carp_fall$bb_count) # mean CPUE = 0.030
sd(carp_fall$bb_count) # SD = 0.038



cluculz <- traps_2022 %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue)) %>%
  filter(lake_id == "Cluculz Lake")

mean(cluculz$cpue) # mean CPUE = 0.0009
sd(cluculz$cpue) # SD = 0.006





####### Total burbot, total trap hrs, and CPUE for each lake in 2022 #######

#Carp Lake - FALL 2022
#carp_traps_2022$start_date <- as.Date(carp_traps_2022$start_date, format = "%m/%d/%y")
#carp_traps_2022$end_date <- as.Date(carp_traps_2022$end_date, format = "%m/%d/%y")

carp_traps_2022_fall <- traps_2022 %>%
  filter(lake_id == "Carp Lake") %>%
  filter(between(start_date, '10/20/22', '10/30/22'))
# YAY :)

trap_h_fall <- sum(carp_traps_2022_fall$trap_dur, na.rm = TRUE) # 1618.883 hours
bb_fall <- sum(carp_traps_2022_fall$bb_count, na.rm = TRUE) # 23 burbot
cpue_fall <- bb_fall/as.numeric(trap_h_fall) # CPUE = 0.014

# Carp Lake - SPRING 2022
carp_traps_2022_spr <- traps_2022 %>%
  filter(lake_id == "Carp Lake") %>%
  filter(between(start_date, '5/01/22', '7/01/22'))
trap_h_spr <- sum(carp_traps_2022_spr$trap_dur, na.rm = TRUE) # 3155.45 hours
bb_spr <- sum(carp_traps_2022_spr$bb_count, na.rm = TRUE) # 95 burbot
cpue_spr <- bb_spr/as.numeric(trap_h_spr) # CPUE = 0.030


total_bb_carp <- sum(traps_2022$bb_count, na.rm = TRUE)
total_trap_hrs_carp <- sum(traps_2022$trap_dur, na.rm = TRUE)
total_trap_hrs_carp <- as.numeric(sum(traps_2022$trap_dur, na.rm = TRUE))
carp_cpue <- total_bb_carp / total_trap_hrs_carp
carp_cpue # 0.025 burbot per hour of trap set

#Cluculz Lake
cluculz_traps_2022 <- filter(traps_2022, lake_id == "Cluculz Lake")
total_bb_cluculz <- sum(cluculz_traps_2022$bb_count, na.rm = TRUE)
total_trap_hrs_cluculz <- as.numeric(sum(cluculz_traps_2022$trap_dur, na.rm = TRUE))
total_trap_hrs_cluculz

#Fraser Lake
fraser_traps_2022 <- filter(traps_2022, lake_id == "Fraser Lake")
total_bb_fr <- sum(fraser_traps_2022$bb_count, na.rm = TRUE)
total_trap_hrs_fr <- as.numeric(sum(fraser_traps_2022$trap_dur, na.rm = TRUE))
total_trap_hrs_fr
fraser_cpue <- total_bb_fr / total_trap_hrs_fr
fraser_cpue # 0.0065 burbot per hour of trap set

