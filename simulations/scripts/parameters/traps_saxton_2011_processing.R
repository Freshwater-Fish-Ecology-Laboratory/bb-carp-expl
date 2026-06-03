#Code to process open-water cod trap and hoop trap data from Saxton Lake, spring-fall 2011
#Liz Hirsch
#November 25, 2022

library(tidyverse)
library(anytime)
library(hms)
library(gt)

#2011 open water trapping data
traps_saxton_2011 <- read_csv("simulations/data/traps_saxton_2011.csv")

traps_saxton_2011 <- traps_saxton_2011 %>%
  rename(trap_id = `Trap #`, date_set = `Date Set (mm/dd/yy)`, time_set = `Time Set`, date_pulled = `Date Pulled (mm/dd/yy)`,
                           time_pulled = `Time Pulled`, utm_zone = `UTM Zone`, utm_easting = `UTM Easting`, utm_northing = `UTM Northing`, trap_type = `Trap Type`,
                           bait = `Bait`, depth = `Depth m`, temp = `Temp C`, secchi = `Secchi Depth m`, bb_presence = `Burbot Captured Y/N`, burbot_count = `No. Burbot Captured`,
                           length = `Len (mm)`, weight = `Wt (kg)`, girth = `Girth (mm)`, tag_applied = `Tag Applied (Y/N)`, tag_num_col = `Tag No. / Colour...20`,
                           recap = `Recap (Y/N)`, recap_tag = `Tag No. / Colour...22`, fate = `Fate (D/L)`, barotrauma = `Decompression Stress (Y/N)`, treatment = `Treatment`,
                           sacrificed = `Sacrificed (Y/N)`, sacr_code = `Sacrificed Code`, comment = `Comments`) %>%
  mutate(burbot_count = as.numeric(burbot_count, na.rm = TRUE)) %>%
  mutate(date_set = as.POSIXct(date_set, format = "%d-%b-%y"), 
         date_pulled = as.POSIXct(date_pulled, format = "%d-%b-%y"))

# Format trap set and pull times 
traps_saxton_2011$time_set <- as.POSIXct(traps_saxton_2011$time_set, format="%H%M")
traps_saxton_2011$time_set <- format(traps_saxton_2011$time_set, "%H:%M")
traps_saxton_2011$time_set <- as.POSIXct(traps_saxton_2011$time_set, format = "%H:%M")
#try with hms package
traps_saxton_2011$time_set <- as_hms(traps_saxton_2011$time_set)
#IT WORKS!!
traps_saxton_2011$time_pulled <- as.POSIXct(traps_saxton_2011$time_pulled, format="%H%M")
traps_saxton_2011$time_pulled <- format(traps_saxton_2011$time_pulled, "%H:%M")
traps_saxton_2011$time_pulled <- as.POSIXct(traps_saxton_2011$time_pulled, format = "%H:%M")
traps_saxton_2011$time_pulled <- as_hms(traps_saxton_2011$time_pulled)

#Now add a date_time column
traps_saxton_2011 <- traps_saxton_2011 %>%
  mutate(date_time_set = with(traps_saxton_2011, anytime(paste(traps_saxton_2011$date_set, traps_saxton_2011$time_set))), 
         date_time_pulled = with(traps_saxton_2011, anytime(paste(traps_saxton_2011$date_pulled, traps_saxton_2011$time_pulled))), 
         trap_duration = difftime(date_time_pulled, date_time_set, units = "hours"),
         cpue = (burbot_count)/as.numeric(trap_duration))

########
#filter by trap method
codtraps_saxton_2011 <- filter(traps_saxton_2011, trap_type == "Cod Trap")
hooptraps_saxton_2011 <- filter(traps_saxton_2011, trap_type == "Hoop Trap L")

#number of sets with each trap type
nrow(hooptraps_saxton_2011) # 185 hoop traps
nrow(codtraps_saxton_2011) # 229 cod traps 
#number of burbot
sum(hooptraps_saxton_2011$burbot_count, na.rm = TRUE) # 15 caught with hoop traps
sum(codtraps_saxton_2011$burbot_count, na.rm = TRUE) # 25 caught with cod traps

# overall number of burbot captures per trap, by trap type:
(hooptraps_bb_per_set_2011 <- sum(hooptraps_saxton_2011$burbot_count, na.rm = TRUE) / nrow(hooptraps_saxton_2011)) # 0.081 burbot/hoop trap 
(codtraps_bb_per_set_2011 <- sum(codtraps_saxton_2011$burbot_count, na.rm = TRUE) / nrow(codtraps_saxton_2011)) # 0.109 burbot/cod trap
##########

# SUMMARY TABLE
summary_tbl <- traps_saxton_2011 %>%
  filter(trap_type == "Cod Trap" | trap_type == "Hoop Trap L") %>%
  group_by(trap_type) %>% 
  summarize("Number of traps set" = n_distinct(trap_id),
            "Trap hours" = round(sum((as.numeric(trap_duration)), na.rm = TRUE), digits = 0),
            "Total burbot caught" = sum(burbot_count, na.rm = TRUE),
            "Mean CPUE" = mean(burbot_count),
            "SD of CPUE" = sd(burbot_count))

(summary_table <- gt(summary_tbl) %>%
    fmt_number(
      decimals = 3, drop_trailing_zeros = TRUE) %>%
    tab_header(title = "Burbot capture effort at Saxton Lake 2010-2011"))
  
summary_tbl2 <- traps_saxton_2011 %>%
  filter(trap_type == "Cod Trap" | trap_type == "Hoop Trap L") %>%
  group_by(trap_type) %>% 
  rename(`Trap Type` = trap_type) %>%
  summarize("Number of traps set" = n_distinct(time_set),
            "Total trapping effort (hours)" = round(sum((as.numeric(trap_duration)), na.rm = TRUE), digits = 0),
            "Total burbot caught" = sum(burbot_count, na.rm = TRUE),
            "Mean CPUE" = mean(burbot_count),
            "SD of CPUE" = sd(burbot_count))
(summary_table2 <- gt(summary_tbl2) %>%
  fmt_number(
    decimals = 5, drop_trailing_zeros = TRUE) %>%
  tab_header(title = "Burbot capture effort at Saxton Lake, 2010-2011"))


## Questions/issues: why the 1-year trap duration for some hoop traps... is it an error?
# check that "1 of 2" etc burbot captured means 2 were captured, 1 was tagged

