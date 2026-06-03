#Code to import and process data from 2009 burbot open-water trapping at Norman Lake
#Liz Hirsch
#November 16 2022
library(readr)
library(dplyr)
library(ggplot2)
library(hms)
library(anytime)
library(mgcv)

traps_norman_2009 <- read_csv("simulations/data/trapping_norman_2009.csv")
traps_norman_2009 <- traps_norman_2009 %>% rename(trap_id = `Trap #`, date_set = `Date Set`, time_set = `Time Set`,
                            date_pulled = `Date Pulled`, time_pulled = `Time Pulled`, utm_zone = `UTM Zone`,
                            utm_easting = `UTM Easting`, utm_northing = `UTM Northing`, trap_type = `Trap Type`,
                            open_closed_funnel = `Open/ Closed Funnel (OF/CF)`, bait = `Bait`, depth = `Depth m`,
                            temp = `Temp C`, secchi = `Secchi Depth`, bb_presence = `Burbot Captured Y/N`, burbot_count = `No. Burbot Captured`,
                            length = `Len (mm)`, weight = `Wt (kg)`, girth = `Girth (MM)`, tag_applied = `Tag applied (Y/N)`,
                            tag_num_col = `Tag No. / Colour...21`, recap = `Recap (Y/N)`, recap_tag_num_col = `Tag No. / Colour...23`, fate = `Fate (D/L)`,
                            sacrificed = `Sacrificed (Y/N)`, sacr_code = `Sacrificied Code`, tag_mortality = `Taggged Mortality`, comment = `Comments`,
                            species1 = `Sp....29`, num_live1 = `No.L...30`, num_dead1 = `No.D...31`, length1_1 = `Len...32`,
                            len1_2 = `Len...33`, len1_3 = `Len...34`, len1_4 = `Len...35`, len1_5 = `Len...36`,
                            species2 = `Sp....37`, num_live2 = `No.L...38`, num_dead2 = `No.D...39`, length2_1 = `Len...40`,
                            len2_2 = `Len...41`, len2_3 = `Len...42`, len2_4 = `Len...43`, len2_5 = `Len...44`, species3 = `Sp....45`, 
                            num_live3 = `No.L...46`, num_dead3 = `No.D...47`, length3_1 = `Len...48`,
                            len3_2 = `Len...49`, len3_3 = `Len...50`, len3_4 = `Len...51`, len3_5 = `Len...52`, species4 = `Sp....53`,
                            num_live4 = `No.L...54`, num_dead4 = `No.D...55`, length4_1 = `Len...56`,
                            len4_2 = `Len...57`, len4_3 = `Len...58`, len4_4 = `Len...59`, len4_5 = `Len...60`, species5 = `Sp....61`,
                            num_live5 = `No.L...62`, num_dead5 = `No.D...63`, length5_1 = `Len...64`,
                            len5_2 = `Len...65`, len5_3 = `Len...66`) %>%
  subset(!is.na(trap_id)) %>%
  mutate(burbot_count = as.numeric(burbot_count)) %>%
  mutate(burbot_count = if_else(is.na(burbot_count), 0, burbot_count))
                            
#in this dataset, there are separate columns for each of multiple species that could've been captured, then for each species there are multiple columns for
#the length of each individual fish. I labelled them len1_1, len1_2, len1_3 etc for Species 1, Individual 1, 2, 3...

#format dates and times
#dates
traps_norman_2009$date_set <- as.POSIXct(traps_norman_2009$date_set, format = "%d-%b-%y")
traps_norman_2009$date_pulled <- as.POSIXct(traps_norman_2009$date_pulled, format = "%d-%b-%y")
#time set
traps_norman_2009$time_set <- as.POSIXct(traps_norman_2009$time_set, format="%H%M")
traps_norman_2009$time_set <- format(traps_norman_2009$time_set, "%H:%M")
traps_norman_2009$time_set <- as.POSIXct(traps_norman_2009$time_set, format = "%H:%M")
traps_norman_2009$time_set <- as_hms(traps_norman_2009$time_set)
#time pulled 
traps_norman_2009$time_pulled <- as.POSIXct(traps_norman_2009$time_pulled, format="%H%M")
traps_norman_2009$time_pulled <- format(traps_norman_2009$time_pulled, "%H:%M")
traps_norman_2009$time_pulled <- as.POSIXct(traps_norman_2009$time_pulled, format = "%H:%M")
traps_norman_2009$time_pulled <- as_hms(traps_norman_2009$time_pulled)

traps_norman_2009 <- traps_norman_2009 %>%
  mutate(date_time_set = with(traps_norman_2009, 
                              anytime(paste(traps_norman_2009$date_set, traps_norman_2009$time_set))))
traps_norman_2009 <- traps_norman_2009 %>%
  mutate(date_time_pulled = with(traps_norman_2009,
                                 anytime(paste(traps_norman_2009$date_pulled, traps_norman_2009$time_pulled))))

#separate data by trap type:
codtraps_norman_2009 <- filter(traps_norman_2009, traps_norman_2009$trap_type == "Cod Trap")
hooptraps_norman_2009 <- filter(traps_norman_2009, traps_norman_2009$trap_type == "Hoop Trap")
trapnets_norman_2009 <- filter(traps_norman_2009, traps_norman_2009$trap_type == "Trap Net")

(bb_codtraps <- sum(codtraps_norman_2009$burbot_count, na.rm = TRUE)) # 74 burbot
(bb_hooptraps <- sum(hooptraps_norman_2009$burbot_count, na.rm = TRUE)) # 24 burbot
(bb_trapnets <- sum(trapnets_norman_2009$burbot_count, na.rm = TRUE)) # 3 burbot

traps_norman_2009 <- traps_norman_2009 %>%
  mutate(trap_duration = as.numeric(difftime(traps_norman_2009$date_time_pulled, 
                                             traps_norman_2009$date_time_set, units = "hours"))) %>%
  mutate(cpue = burbot_count / trap_duration)

codtraps_norman_2009 <- codtraps_norman_2009 %>% 
  mutate(trap_duration = as.numeric(difftime(codtraps_norman_2009$date_time_pulled, 
                                             codtraps_norman_2009$date_time_set, units = "hours"))) %>%
  mutate(cpue = burbot_count / trap_duration)

#CPUE overall for Norman Lake 2009:
cpue_norman_2009 <- sum(traps_norman_2009$burbot_count, na.rm = TRUE) / sum(traps_norman_2009$trap_duration, na.rm = TRUE)
cpue_norman_2009

# Create a table of CPUE by trap type for Norman Lake 2009:
summary_tbl <- traps_norman_2009 %>%
  filter(trap_type == "Cod Trap" | trap_type == "Hoop Trap" | trap_type == "Trap Net") %>%
  group_by(trap_type) %>% 
  rename(`Trap Type` = trap_type) %>%
  summarize("Number of traps set" = n_distinct(time_set),
            "Total trapping effort (hours)" = round(sum((as.numeric(trap_duration)), na.rm = TRUE), digits = 0),
            "Total burbot caught" = sum(burbot_count, na.rm = TRUE),
            "Mean CPUE" = mean(burbot_count, na.rm = TRUE),
            "SD of CPUE" = sd(burbot_count, na.rm = TRUE))
            
(summary_table <- gt(summary_tbl) %>%
    fmt_number(
      decimals = 4, drop_trailing_zeros = TRUE) %>%
    tab_header(title = "Burbot capture effort at Norman Lake, 2009"))

