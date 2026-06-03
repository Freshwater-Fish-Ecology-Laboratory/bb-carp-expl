#code to import and process data for 2007 open water-trapping at Eaglet, Nukko, Cluculz and Norman lakes
#Liz Hirsch
#November 15, 2022
library(dplyr)
library(readr)
library(ggplot2)
library(anytime)
library(mgcv)

#import data
traps_eaglet_2007 <- read_csv("data/raw/open_water_trapping_eaglet.csv")
traps_nukko_2007 <- read_csv("data/raw/open_water_trapping_nukko.csv")
traps_cluculz_2007 <- read_csv("data/raw/open_water_trapping_cluculz.csv")
traps_norman_2007 <- read_csv("data/raw/open_water_trapping_norman.csv")

View(traps_eaglet_2007)
View(traps_nukko_2007)
View(traps_cluculz_2007)
View(traps_norman_2007)
#rename columns etc etc etc
traps_eaglet_2007$`Eaglet Lake - 2007 Open Water Burbot Trapping` <- NULL
traps_eaglet_2007 <- rename(traps_eaglet_2007, event = `event`, trap_id = `Trap No.`, trap_type = `Trap Type`, location = `Location`,
                            utm_zone = `Zone`, utm_northing = `N`, utm_easting = `E`, depth_ft = `Z`, temp = `Temp`, 
                            time_set = `Time Deployed`, time_pulled = `Time Retrieved`, effort = `Effort (hrs:min)`, effort_hr = `Effort (Hrs)`,
                            fish_presence = `Fish (Y/N)`, cottid_ct = `Cottid`, rshiner_ct = `R Shiner`, nsc_ct = `Pikeminnow`,
                            burbot_ct = `Burbot`, pmc_ct = `PMC`)

traps_nukko_2007$`Nukko Lake - 2007 Open Water Burbot Trapping` <- NULL
traps_nukko_2007 <- rename(traps_nukko_2007, event = `Event`, trap_id = `Trap No.`, trap_type = `Trap Type`, depth_ft = `Z (ft)`,
                           location = `Location`, utm_zone = `Zone`, utm_northing = `N`, utm_easting = `E`, temp = `Temp`, time_set = `Time Deployed`,
                           time_pulled = `Time Retrieved`, effort = `Effort (hrs:min)`, effort_hrs = `Effort (Hrs)`, fish_presence = `Fish (Y/N)`,
                           cottid_ct = `Cottid`, rshiner_ct = `R Shiner`, nsc_ct = `Pikeminnow`, burbot_ct = `Burbot`, pmc_ct = `PMC`)

traps_cluculz_2007$`Cluculz Lake - 2007 Open Water Burbot Trapping` <- NULL
traps_cluculz_2007 <- rename(traps_cluculz_2007, event = `Event`, trap_id = `Trap No.`, trap_type = `Trap Type`, location = `Location`,
                             utm_zone = `Zone`, utm_northing = `N`, utm_easting = `E`, depth_ft = `Z (ft)`, temp = `Temp`, 
                             time_set = `Time Deployed`, time_pulled = `Time Retrieved`, effort = `Effort (hrs:min)`, effort_hrs = `Effort (Hrs)`,
                             fish_presence = `Fish (Y/N)`, cottid_ct = `Cottid`, rshiner_ct = `R Shiner`, nsc_ct = `Pikeminnow`,
                             burbot_ct = `Burbot`, pmc_ct = `PMC`)

traps_norman_2007$`Norman Lake - 2007 Open Water Burbot Trapping` <- NULL
traps_norman_2007 <- rename(traps_norman_2007, event = `Event`, trap_id = `Trap No.`, trap_type = `Trap Type`, location = `Location`,
                            utm_zone = `Zone`, utm_northing = `N`, utm_easting = `E`, depth_ft = `Z`, temp = `Temp`, 
                            time_set = `Time Deployed`, time_pulled = `Time Retrieved`, effort = `Effort (hrs:min)`, effort_hrs = `Effort (Hrs)`, 
                            fish_presence = `Fish (Y/N)`, cottid_ct = `Cottid`, rshiner_ct = `R Shiner`, nsc_ct = `Pikeminnow`, 
                            burbot_ct = `Burbot`, pmc_ct = `PMC`)
sum(traps_eaglet_2007$burbot_ct, na.rm = TRUE)
sum(traps_nukko_2007$burbot_ct, na.rm = TRUE)
sum(traps_cluculz_2007$burbot_ct, na.rm = TRUE)
sum(traps_norman_2007$burbot_ct, na.rm = TRUE)

#very sparse data for burbot. 4 burbot captured at Eaglet Lake, 4 at Cluculz Lake, 0 at Nukko and Norman lakes
#filter data by trap type:
ct_eaglet <- filter(traps_eaglet_2007, trap_type == "CT")
g_eaglet <- filter(traps_eaglet_2007, trap_type == "G")

ct_nukko <- filter(traps_nukko_2007, trap_type == "CT")
g_nukko <- filter(traps_nukko_2007, trap_type == "G")

ct_cluculz <- filter(traps_cluculz_2007, trap_type == "CT")
g_cluculz <- filter(traps_cluculz_2007, trap_type == "G")

ct_norman <- filter(traps_norman_2007, trap_type == "CT")
g_norman <- filter(traps_norman_2007, trap_type == "G")


#Overall CPUE calculations for each lake:
cpue_eaglet_2007 <- sum(traps_eaglet_2007$burbot_ct, na.rm = TRUE) / sum(traps_eaglet_2007$effort_hr, na.rm = TRUE)
cpue_eaglet_2007

cpue_nukko_2007 <- sum(traps_nukko_2007$burbot_ct, na.rm = TRUE) / sum(traps_nukko_2007$effort_hrs, na.rm = TRUE)
cpue_nukko_2007

cpue_cluculz_2007 <- sum(traps_cluculz_2007$burbot_ct, na.rm = TRUE) / sum(traps_cluculz_2007$effort_hrs, na.rm = TRUE)
cpue_cluculz_2007

cpue_norman_2007 <- sum(traps_norman_2007$burbot_ct, na.rm = TRUE) / sum(traps_norman_2007$effort_hrs, na.rm = TRUE)
cpue_norman_2007

#Adding CPUE column to datasets
traps_eaglet_2007 <- traps_eaglet_2007 %>%
  mutate(cpue = burbot_ct/effort_hr)

traps_nukko_2007 <- traps_nukko_2007 %>%
  mutate(cpue = burbot_ct/ effort_hrs)

traps_cluculz_2007 <- traps_cluculz_2007 %>%
  mutate(cpue = burbot_ct/effort_hrs)

traps_norman_2007 <- traps_norman_2007 %>%
  mutate(cpue = burbot_ct/effort_hrs)

#Sample sizes of burbot caught are probably way too small to test for significant predictors of catch rate..
#try it out just as an example:
mod_lm = gam(cpue ~ depth_ft, data = traps_eaglet_2007)
summary(mod_lm)
