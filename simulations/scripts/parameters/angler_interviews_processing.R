#Code to analyze angler interview and catch data from Cluculz, Eaglet, Norman and Nukko lakes, 2007.
# WINTER ice-fishing (may not be the same catch rates as open-water burbot fishing)

#Liz Hirsch
#Jan. 30, 2023

library(tidyverse)
library(ggplot2)

angler_interviews_cluculz <- read_csv("simulations/data/angler_interviews_cluculz.csv")

angler_interviews_eaglet <- read_csv("simulations/data/angler_interviews_eaglet.csv")

angler_interviews_norman <- read_csv("simulations/data/angler_interviews_norman.csv")

angler_interviews_nukko <- read_csv("simulations/data/angler_interviews_nukko.csv")

angler_interviews_saxton <- read_csv("simulations/data/angler_interviews_saxton.csv")

#Calculate catch rates for each lake

#1. Cluculz Lake

angler_interviews_cluculz <- angler_interviews_cluculz %>%
  rename(bb_killed = `...9`,
         bb_released = `...16`,
         effort = `...5`) %>%
  mutate(total_bb = (as.numeric(bb_killed)) + 
           (as.numeric(bb_released)),
         cpue = total_bb/ (as.numeric(effort))) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue))

mean(angler_interviews_cluculz$cpue) # mean CPUE = 0.0044
sd(angler_interviews_cluculz$cpue) # SD = 0.086


#2. Eaglet Lake

angler_interviews_eaglet <- angler_interviews_eaglet %>%
  rename(bb_killed = `...9`,
         bb_released = `...16`,
         effort = `...5`) %>%
  mutate(total_bb = (as.numeric(bb_killed)) + 
           (as.numeric(bb_released)),
         cpue = total_bb/ (as.numeric(effort))) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue)) %>%
  slice(-(1:3))

mean(angler_interviews_eaglet$cpue) # mean CPUE = 0.00008 (approx 0)
sd(angler_interviews_eaglet$cpue) # SD = 0.002

#3. Norman Lake

angler_interviews_norman <- angler_interviews_norman %>%
  rename(bb_killed = `...9`,
         bb_released = `...16`,
         effort = `...5`) %>%
  mutate(total_bb = (as.numeric(bb_killed)) + 
           (as.numeric(bb_released)),
         cpue = total_bb/ (as.numeric(effort))) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue)) %>%
  slice(-(1:3))


mean(angler_interviews_norman$cpue) # mean CPUE = 0.0002
sd(angler_interviews_norman$cpue) # SD = 0.005

#4. Nukko Lake
#NO BURBOT CAUGHT
#cpue = 0

#5. Saxton Lake
#NO EFFORT DATA
#one small burbot was apparently caught and released but there is no 
# info on gear or the amount of time spent fishing.



