#Code to process open-water trapping data for Saxton Lake, 2010 spring-fall
#Liz Hirsch 
# updated May 2024

library(tidyverse)
library(anytime)
library(hms)
library(gt)

# 2010 open water trapping data
traps_saxton_2010 <- read_csv("simulations/data/traps_saxton_2010.csv") %>% 
  rename(trap_id = `Trap #`, date_set = `Date Set`, time_set = `Time Set`, date_pulled = `Date Pulled`,
                            time_pulled = `Time Pulled`, utm_zone = `UTM Zone`, utm_easting = `UTM Easting`, utm_northing = `UTM Northing`, trap_type = `Trap Type`,
                            bait = `Bait`, depth = `Depth m`, temp = `Temp C`, secchi = `Secchi Depth m`, bb_presence = `Burbot Captured Y/N`, burbot_count = `No. Burbot Captured`,
                            length = `Len (mm)`, weight = `Wt (kg)`, girth = `Girth (mm)`, tag_applied = `Tag applied (Y/N)`, tag_num_col = `Tag No. / Colour`,
                            recap = `Recap (Y/N)`, recap_tag = `Tag No. / Colour2`, fate = `Fate (D/L)`, barotrauma = `Decompression Stress (Y/N)`, treatment = `Treatment`,
                            sacrificed = `Sacrificed (Y/N)`, sacr_code = `Sacrificied Code`, comment = `Comments`) %>%
  mutate(burbot_count = as.numeric(burbot_count)) %>%
  mutate(burbot_count = if_else(is.na(burbot_count), 0, burbot_count))

#format dates and times
traps_saxton_2010$date_set <- as.POSIXct(traps_saxton_2010$date_set, format = "%d-%b-%y")
traps_saxton_2010$date_pulled <- as.POSIXct(traps_saxton_2010$date_pulled, format = "%d-%b-%y")

traps_saxton_2010$time_set <- as.POSIXct(traps_saxton_2010$time_set, format="%H%M")
traps_saxton_2010$time_set <- format(traps_saxton_2010$time_set, "%H:%M")
traps_saxton_2010$time_set <- as.POSIXct(traps_saxton_2010$time_set, format = "%H:%M")
#try with hms package
traps_saxton_2010$time_set <- as_hms(traps_saxton_2010$time_set)

traps_saxton_2010$time_pulled <- as.POSIXct(traps_saxton_2010$time_pulled, format="%H%M")
traps_saxton_2010$time_pulled <- format(traps_saxton_2010$time_pulled, "%H:%M")
traps_saxton_2010$time_pulled <- as.POSIXct(traps_saxton_2010$time_pulled, format = "%H:%M")
traps_saxton_2010$time_pulled <- as_hms(traps_saxton_2010$time_pulled)

#Now add date_time columns and calculate trap durations in hours
traps_saxton_2010 <- traps_saxton_2010 %>%
  mutate(date_time_set = with(traps_saxton_2010, anytime(paste(date_set, time_set)))) %>%
  mutate(date_time_pulled = with(traps_saxton_2010, anytime(paste(traps_saxton_2010$date_pulled, 
                                                                  traps_saxton_2010$time_pulled)))) %>%
  mutate(trap_duration = as.numeric(difftime(date_time_pulled, date_time_set, units = "hours")),
         cpue = burbot_count / trap_duration)

########
#filter by trap method
codtraps_saxton_2010 <- filter(traps_saxton_2010, trap_type == "Cod Trap")
hooptraps_saxton_2010 <- filter(traps_saxton_2010, trap_type == "Hoop Trap L")

codtraps_bb_per_set_2010 <- sum(codtraps_saxton_2010$burbot_count, na.rm = TRUE) / nrow(codtraps_saxton_2010) # 0.109 burbot/cod trap
hooptraps_bb_per_set_2010 <- sum(hooptraps_saxton_2010$burbot_count, na.rm = TRUE) / nrow(hooptraps_saxton_2010) # 0.081 burbot/hoop trap 
##########

# CPUE table
summary_tbl <- traps_saxton_2010 %>%
  filter(trap_type == "Cod Trap" | trap_type == "Hoop Trap L" | trap_type == "Hoop Trap S" | trap_type == "Trap Net") %>%
  filter(!is.na(date_pulled)) %>%
  group_by(trap_type) %>% 
  rename(`Trap Type` = trap_type) %>%
  summarize("Number of traps set" = n_distinct(time_set),
            "Total trapping effort (hours)" = round(sum((as.numeric(trap_duration)), na.rm = TRUE), digits = 0),
            "Total burbot caught" = sum(burbot_count, na.rm = TRUE),
            "Mean CPUE" = mean(burbot_count),
            "SD of CPUE" = sd(burbot_count))

(summary_table <- gt(summary_tbl) %>%
    fmt_number(
      decimals = 4, drop_trailing_zeros = TRUE) %>%
    tab_header(title = "Burbot capture effort at Saxton Lake, 2010"))

