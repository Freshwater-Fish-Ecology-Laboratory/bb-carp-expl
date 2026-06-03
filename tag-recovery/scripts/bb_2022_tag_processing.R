#Code for processing burbot tagging & biological data from spring-summer 2022 field season
#Liz Hirsch
#Aug. 11 2022

library(tidyverse)

#read file
bb_tagging_2022 <- read_csv("carp_lk/data/raw/BB_tagging_2022.csv")

#Rename columns
bb_tagging_2022 <- rename(bb_tagging_2022, lake_id = Lake, tag_date = `Date tagged`, trap_id = `Trap ID`, 
       depth = `Trap depth (m)`, temp = `Trap temperature`, acoustic_id = `Acoustic tag number`,
       surg_time = `Surgery duration (min)`, glove_setting = `Electric glove setting (mA)`, pit_id = `PIT tag number`,
       floy_id = `Floy tag number`, floy_type = `Floy reward amount`, 
       length = `Total length (cm)`, weight = `Weight (g)`)

#will need some code to call floy_type 100 "high reward" and floy_type 0 "standard"

#Split tagged fish by lake
fraser_bb <- filter(bb_tagging_2022, lake_id == "Fraser Lake")

carp_bb <- filter(bb_tagging_2022, lake_id == "Carp Lake")
#remove non-target species and non-tagged burbot from Carp Lake catches:
carp_bb <- carp_bb[!is.na(carp_bb$floy_id),]

cluculz_bb <- filter(bb_tagging_2022, lake_id == "Cluculz Lake")


