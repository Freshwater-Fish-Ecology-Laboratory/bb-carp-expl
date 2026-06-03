#Code to summarize burbot catches by spatial distribution (by depth strata and by zone) 
#in Fraser Lake and Carp Lake, combining 2021 and 2022 datasets
#Liz Hirsch
#December 19, 2022 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)

source("carp_lk/codes/processing/traps_2022_processing.R")
source("carp_lk/codes/processing/traps_2021_processing.R")

####Grouping catches by zone 
#CARP LAKE 2022
#Add a zone column for 2022 Carp Lake traps
carp_traps_2022 <- traps_2022 %>% 
  filter(lake_id == "Carp Lake") %>%
  mutate(zone = case_when(start_date == "5/30/22" ~ 2, start_date == "5/31/22" ~ 3,
                          start_date == "6/1/22" ~ 4, start_date == "6/2/22" ~ 1, start_date == "6/20/22" ~ 2,
                          start_date == "6/21/22" ~ 3, start_date == "6/22/22" ~ 4, start_date == "6/23/22" ~ 1,
                          start_date == "10/24/22" ~ 2, start_date == "10/25/22" ~ 3, start_date == "10/26/22" ~ 4,
                          start_date == "10/27/22" ~ 1),
         bb_count = ifelse(is.na(bb_count), 0, bb_count),
         cpue = as.numeric(weight, na.rm = TRUE) / as.numeric(trap_dur, na.rm = TRUE))

#Traps set in each zone of Carp Lake, 2022
count_z1_carp_22 <- length(which(carp_traps_2022$zone == 1))
count_z2_carp_22 <- length(which(carp_traps_2022$zone == 2))
count_z3_carp_22 <- length(which(carp_traps_2022$zone == 3))
count_z4_carp_22 <- length(which(carp_traps_2022$zone == 4))
#########
# 60 traps were set in each of the 4 zones of Carp Lake (20 x 3 sampling sessions)
#########

#FRASER LAKE 2022
fraser_traps_2022 <- fraser_traps_2022 %>%
  mutate(zone = case_when(str_detect(trap_id, "TZ1") ~ 1, str_detect(trap_id, "TZ2") ~2,
                          str_detect(trap_id, "TZ3") ~3, str_detect(trap_id, "TZ4") ~ 4)) %>%
  mutate(bb_count = ifelse(is.na(bb_count), 0, bb_count),
         cpue = as.numeric(fraser_traps_2022$weight, na.rm = TRUE) / as.numeric(trap_dur, na.rm = TRUE))

#How many traps were set in each zone? (Fraser Lake, 2022)
count_z1_fr_22 <- length(which(fraser_traps_2022$zone == 1)) # 58 traps
count_z2_fr_22 <- length(which(fraser_traps_2022$zone == 2)) # 72 traps
count_z3_fr_22 <- length(which(fraser_traps_2022$zone == 3)) # 16 traps
count_z4_fr_22 <- length(which(fraser_traps_2022$zone == 4)) # 16 traps 
##############
# 72 traps set in Z2, 58 in Z1, 16 in Z3 and 16 in Z4.
##############

#Calculate catch rate in each zone, as sum of burbot caught in that zone divided by sum of trap durations
##in that zone
#Step 1 calculate total burbot caught each zone

as.numeric(fraser_traps_2022$bb_count)

z1_fr <- filter(fraser_traps_2022, fraser_traps_2022$zone == 1)
bb_z1_fr_22 <- sum(z1_fr$bb_count, na.rm = TRUE)
bb_z1_fr_22 #14
z2_fr <- filter(fraser_traps_2022, fraser_traps_2022$zone == 2)
bb_z2_fr_22 <- sum(z2_fr$bb_count, na.rm = TRUE)
bb_z2_fr_22 #10
z3_fr <- filter(fraser_traps_2022, fraser_traps_2022$zone == 3)
bb_z3_fr_22 <- sum(z3_fr$bb_count, na.rm = TRUE)
bb_z3_fr_22 #2
z4_fr <- filter(fraser_traps_2022, fraser_traps_2022$zone == 4)
bb_z4_fr_22 <- sum(z4_fr$bb_count, na.rm = TRUE)
bb_z4_fr_22 #4
#Fraser Lake total burbot tagged: 30

#FRASER LAKE 2022 - CPUE BOXPLOT
ggplot(fraser_traps_2022, aes(x = as.character(zone), y = cpue)) +
  geom_boxplot() +
  xlab("Zone") + 
  ylab("CPUE in burbot weight (g) /trap hour") +
  ggtitle("Fraser Lake CPUE by zone, 2022")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank())


#Step 2 calculate total duration of trapping done in each zone
as.numeric(fraser_traps_2022$trap_dur, na.rm = TRUE)
eff_z1_fr <- sum(z1_fr$trap_dur, na.rm = TRUE)
eff_z2_fr <- sum(z2_fr$trap_dur, na.rm = TRUE)
eff_z3_fr <- sum(z3_fr$trap_dur, na.rm = TRUE)
eff_z4_fr <- sum(z4_fr$trap_dur, na.rm = TRUE)
eff_total_fr_22 <- sum(eff_z1_fr, eff_z2_fr, eff_z3_fr, eff_z4_fr)
eff_total_fr_22 #4597.583 hrs

fraser_traps_2022 %>%
  ggplot(aes(x = trap_dur)) +
  geom_histogram(aes(y = ..density..),
                 binwidth=.3,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666")+
  ggtitle("Durations of Fraser Lake burbot trap sets in 2022")

#Total times in hours (Fraser Lake 2022):
#### Zone 1: 2216.967
#### Zone 2: 1719.033
#### Zone 3: 295.3833
#### Zone 4: 366.2
#Fraser Lake total trapping: 4597.5833 hours
#So overall burbot per trap-hour was 0.0065.

#Step 3 divide burbot catches by effort for a CPUE index
z1_fr_cpue_22 <- bb_z1_fr_22 / as.numeric(eff_z1_fr)
z1_fr_cpue_22
#0.0063
z2_fr_cpue_22 <- bb_z2_fr_22 / as.numeric(eff_z2_fr)
z2_fr_cpue_22
#0.0058
z3_fr_cpue_22 <- bb_z3_fr_22 / as.numeric(eff_z3_fr)
z3_fr_cpue_22
#0.0068
z4_fr_cpue_22 <- bb_z4_fr_22 / as.numeric(eff_z4_fr)
z4_fr_cpue_22
#0.011

### Carp Lake CPUE by zones
#Step 1 calculate total burbot caught each zone

# now we have a cpue value for each row (for each trap set). Can calculate means

#CARP LAKE - CPUE BOXPLOT
ggplot(carp_traps_2022, aes(x = as.character(carp_traps_2022$zone), y = cpue)) +
  geom_boxplot() +
  xlab("Zone") + 
  ylab("CPUE in burbot weight (g) /trap hour") +
  ggtitle("Carp Lake CPUE by zone, 2022")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank())


z1_carp <- filter(carp_traps_2022, carp_traps_2022$zone == 1)
bb_z1_carp <- sum(z1_carp$bb_count, na.rm = TRUE) # 23
z2_carp <- filter(carp_traps_2022, carp_traps_2022$zone == 2)
bb_z2_carp <- sum(z2_carp$bb_count, na.rm = TRUE) # 27
z3_carp <- filter(carp_traps_2022, carp_traps_2022$zone == 3)
bb_z3_carp <- sum(z3_carp$bb_count, na.rm = TRUE) # 37
z4_carp <- filter(carp_traps_2022, carp_traps_2022$zone == 4)
bb_z4_carp <- sum(z4_carp$bb_count, na.rm = TRUE) # 31

#Step 2 calculate total duration of trapping done in each zone
as.numeric(carp_traps_2022$trap_dur, na.rm = TRUE)
eff_z1_carp <- sum(z1_carp$trap_dur, na.rm = TRUE) # 1154.55 hr
eff_z2_carp <- sum(z2_carp$trap_dur, na.rm = TRUE) # 1084.217 hr
eff_z3_carp <- sum(z3_carp$trap_dur, na.rm = TRUE) # 1219.45 hr
eff_z4_carp <- sum(z4_carp$trap_dur, na.rm = TRUE) # 1316.117 hr

carp_traps_2022 %>%
  ggplot(aes(x = trap_dur)) +
  geom_histogram(aes(y = ..density..),
                 binwidth=.1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666")+
  ggtitle("Durations of Carp Lake burbot trap sets in 2022")

#Step 3 divide burbot catches by effort for CPUE (Carp Lake)
z1_carp_cpue <- bb_z1_carp / as.numeric(eff_z1_carp)
z1_carp_cpue
# 0.0199
z2_carp_cpue <- bb_z2_carp / as.numeric(eff_z2_carp)
z2_carp_cpue
# 0.025
z3_carp_cpue <- bb_z3_carp / as.numeric(eff_z3_carp)
z3_carp_cpue
# 0.030
z4_carp_cpue <- bb_z4_carp / as.numeric(eff_z4_carp)
z4_carp_cpue
# 0.024

######## FRASER LAKE 2021 ########
traps_2021 <- traps_2021 %>%
  mutate(count_burbot = ifelse(is.na(count_burbot), 0, count_burbot)) %>%
  mutate(cpue = as.numeric(weight, na.rm = TRUE) / as.numeric(trap_dur, na.rm = TRUE))

#zone = case_when(str_detect(trap_id, "TZ1") ~ 1, str_detect(trap_id, "TZ2") ~ 2,
#str_detect(trap_id, "TZ3") ~ 3, str_detect(trap_id, "TZ4") ~ 4, 
#str_detect(start_date, "05/13/2021") ~ 1, str_detect(start_date, "05/17/2021") ~ 2,
#str_detect(start_date, "05/18/2021") ~ 3, str_detect(start_date, "05/19/2021") ~ 4

#CPUE BOXPLOT - FRASER 2021
ggplot(traps_2021, aes(x = as.character(traps_2021$zone), y = traps_2021$cpue)) +
  geom_boxplot() +
  xlab("Zone") + 
  ylab("CPUE in burbot weight (g) /trap hour") +
  ggtitle("Fraser Lake CPUE by zone, 2021") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme_bw() +
  theme(panel.grid = element_blank())


count_z1_fr_21 <- length(which(traps_2021$zone == 1)) # 64 traps
count_z2_fr_21 <- length(which(traps_2021$zone == 2)) # 58 traps
count_z3_fr_21 <- length(which(traps_2021$zone == 3)) # 25 traps
count_z4_fr_21 <- length(which(traps_2021$zone == 4)) # 69 traps

as.numeric(traps_2021$count_burbot)

z1_fr_21 <- filter(traps_2021, traps_2021$zone == 1)
sum(z1_fr_21$count_burbot, na.rm = TRUE) # 12 burbot
z2_fr_21 <- filter(traps_2021, traps_2021$zone == 2)
sum(z2_fr_21$count_burbot, na.rm = TRUE) # 12 burbot
z3_fr_21 <- filter(traps_2021, traps_2021$zone == 3)
sum(z3_fr_21$count_burbot, na.rm = TRUE) # 9 burbot
z4_fr_21 <- filter(traps_2021, traps_2021$zone == 4)
sum(z4_fr_21$count_burbot, na.rm = TRUE) # 9 burbot

#Add total number of burbot caught in 2021 and 2022 across all zones in Fraser Lake 
bb_total_fr <- bb_z1_fr_21 + bb_z2_fr_21 + bb_z3_fr_21 + bb_z4_fr_21 + bb_z1_fr_22 + bb_z2_fr_22 + bb_z3_fr_22 + bb_z4_fr_22
bb_total_fr

bb_total_21 <- bb_z1_fr_21 + bb_z2_fr_21 + bb_z3_fr_21 + bb_z4_fr_21 #42 tagged in 2021 
bb_total_22 <- bb_z1_fr_22 + bb_z2_fr_22 + bb_z3_fr_22 + bb_z4_fr_22 #30 tagged in 2022
traps_2021$trap_dur_hrs

#Step 2 calculate total duration of trapping done in each zone
as.numeric(traps_2021$trap_duration, na.rm = TRUE)
eff_z1_fr_21 <- sum(z1_fr_21$trap_dur, na.rm = TRUE) # 1355 hours
eff_z2_fr_21 <- sum(z2_fr_21$trap_dur, na.rm = TRUE) # 1476 hours
eff_z3_fr_21 <- sum(z3_fr_21$trap_dur, na.rm = TRUE) # 749 hours
eff_z4_fr_21 <- sum(z4_fr_21$trap_dur, na.rm = TRUE) # 1246 hours

traps_2021 %>%
  ggplot(aes(x = trap_duration)) +
  geom_histogram(aes(y = ..density..),
                 binwidth=.5,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666")+
  ggtitle("Duration of Fraser Lake burbot trap sets in 2021"
  )

#Catch-per-unit-effort for each zone of Fraser Lake in 2021
cpue_fr_z1_21 <- bb_z1_fr_21 / eff_z1_fr_21 #0.0083
cpue_fr_z2_21 <- bb_z2_fr_21 / eff_z2_fr_21 #0.0081
cpue_fr_z3_21 <- bb_z3_fr_21 / eff_z3_fr_21 #0.0120
cpue_fr_z4_21 <- bb_z4_fr_21 / eff_z4_fr_21 #0.0060

#Modelling CPUE 
cpue_21 <- sum(traps_2021$count_burbot) / sum(traps_2021$trap_duration)
cpue_21

fraser_traps_2022$trap_dur <- as.numeric(fraser_traps_2022$trap_dur)
fraser_traps_2022$trap_dur
cpue_fr_22 <- sum(fraser_traps_2022$bb_count) / sum(fraser_traps_2022$trap_dur)
cpue_fr_22

carp_traps_2022$trap_dur <- as.numeric(carp_traps_2022$trap_dur)
carp_traps_2022$trap_dur
cpue_carp_22 <- carp_traps_2022$bb_count / carp_traps_2022$trap_dur
cpue_carp_22

####CPUE PLOTS - FRASER LAKE
#covariates : zone, depth, start date (month)
#2021
plot(cpue_21 ~ as.numeric(traps_2021$zone), data = traps_2021, pch = 16)
plot(cpue_21 ~ traps_2021$depth, data = traps_2021, pch = 16)
abline(mod_fr_depth)
plot(cpue_21 ~ traps_2021$start_month, data = traps_2021, pch = 16)
traps_2021$start_date <- as.Date(traps_2021$start_date, "%y/%m/%d %H:%M")

ggplot(data = traps_2021, aes(traps_2021$start_date, cpue_21)) + geom_point()
#2022
plot(cpue_fr_22 ~ fraser_traps_2022$zone, data = fraser_traps_2022)
plot(cpue_fr_22 ~ fraser_traps_2022$depth, data = fraser_traps_2022)
abline(mod_fr22_depth)
fraser_traps_2022$start_date <- as.Date(fraser_traps_2022$start_date, "%m/%d/%y")

ggplot(data = fraser_traps_2022, aes(fraser_traps_2022$start_date, cpue_fr_22)) + geom_point()

###CARP LAKE 2022
plot(cpue_carp_22 ~ carp_traps_2022$zone, data = carp_traps_2022)
plot(cpue_carp_22 ~ carp_traps_2022$depth, data = carp_traps_2022)
abline(mod_carp_depth)
carp_traps_2022$start_date <- as.Date(carp_traps_2022$start_date, "%m/%d/%y")
ggplot(data = carp_traps_2022, aes(carp_traps_2022$start_date, cpue_carp_22)) + geom_point()

#Linear regression CPUE and depth
mod_carp_depth <- lm(cpue_carp_22 ~ carp_traps_2022$depth, data = carp_traps_2022)
summary(mod_carp_depth) # p = 0.01526

mod_fr_depth <- lm(cpue_21 ~ traps_2021$depth, data = traps_2021)
summary(mod_fr_depth) # p = 0.8544

mod_fr22_depth <- lm(cpue_fr_22 ~ fraser_traps_2022$depth, data = fraser_traps_2022)
summary(mod_fr22_depth) # p = 0.082
#Significant effect of depth on catch-per-unit-effort at Carp Lake, 2022, 
#but not at Fraser Lake in either year

#Linear regression CPUE and zone
mod_carp_zone <- lm(cpue_carp_22 ~ carp_traps_2022$zone, data = carp_traps_2022)
summary(mod_carp_zone) # p = 0.7413
mod_fr21_zone <- lm(cpue_21 ~ traps_2021$zone, data = traps_2021)
summary(mod_fr21_zone) # p = 0.7114
mod_fr22_zone <- lm(cpue_fr_22 ~ fraser_traps_2022$zone, data = fraser_traps_2022)
summary(mod_fr22_zone) # p = 0.1069
#No significant effect of zone on catch-per-unit-effort at Fraser Lake or Carp Lake in either year

#CPUE and trapping date
mod_carp_date <- lm(cpue_carp_22 ~ carp_traps_2022$start_date, data = carp_traps_2022)
summary(mod_carp_date)

ggplot(carp_traps_2022,aes(carp_traps_2022$start_date, cpue_carp_22)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) + 
  ggtitle("Catch-per-unit-effort by trapping date, Carp Lake 2022") +
  xlab("Trapping date") + 
  ylab("CPUE (fish per trap-hour)")

ggplot(fraser_traps_2022,aes(fraser_traps_2022$start_date, cpue_fr_22)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +
  ggtitle("Catch-per-unit-effort by trapping date, Fraser Lake 2022") +
  xlab("Trapping date") +
  ylab("CPUE (fish per trap-hour)")

ggplot(traps_2021, aes(traps_2021$start_date, cpue_21)) +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x) + 
  ggtitle("Catch-per-unit-effort by trapping date, Fraser Lake 2021") +
  xlab("Trapping date") + 
  ylab("CPUE (fish per trap-hour)")
