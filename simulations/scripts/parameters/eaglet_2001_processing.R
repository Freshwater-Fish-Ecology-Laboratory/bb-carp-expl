#Code to import and process data files from 2000-2001 Eaglet Lake burbot assessment
#There are 6 appendices
#Trapping data for trap nets, box traps, and minnow traps, aging data, temp and oxygen data, and fish data
#Plus a sheet showing recaptures (within trap net dataset)
library(tidyverse)
library(ggpmisc)

eaglet_2000_trap_nets <- read_csv("simulations/data/eaglet_2000_trap_nets.csv")

#Generate estimate of catch rate using trap nets
# First, get total hours and burbot across all sampling occasions - remove "summary" rows in Excel sheet and keep only the data rows

eaglet_2000_trap_nets <- eaglet_2000_trap_nets[!(is.na(eaglet_2000_trap_nets$`Set Date`) | eaglet_2000_trap_nets$`Set Date`==""), ] 

sum(eaglet_2000_trap_nets$`No. Burbot`) # 294 burbot caught in total
sum(eaglet_2000_trap_nets$`Total Effort Hours`) # 3966.5 total effort hours
#CPUE calculation:
(eaglet_2000_trapnets_cpue <- 
  sum(eaglet_2000_trap_nets$`No. Burbot`) / sum(eaglet_2000_trap_nets$`Total Effort Hours`))

#do a CPUE column to see CPUE for each trapset
eaglet_2000_trap_nets <- eaglet_2000_trap_nets %>%
  mutate(cpue = `No. Burbot`/`Total Effort Hours`) %>%
  mutate(set_date = as.POSIXct(eaglet_2000_trap_nets$`Set Date`, format = "%m/%d/%y")) %>%
  mutate(depth = `Depth at Box (m)`) %>%
  mutate(temp = `Water Temp C at Set`)

# Now it'll be possible to model CPUE in response to other variables
# 1. depth
lm_cpue_depth <- lm(cpue ~ depth, data = eaglet_2000_trap_nets)
summary(lm_cpue_depth)

ggplot(eaglet_2000_trap_nets, aes(depth, cpue)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")))

# 2. What about the relationship between set date and CPUE
lm_date_cpue <- lm(cpue ~ set_date, data = eaglet_2000_trap_nets)
summary(lm_date_cpue)

ggplot(eaglet_2000_trap_nets, aes(set_date, cpue)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")))

# temperature - low temp -> higher catch rates??
lm_temp_cpue <- lm(cpue ~ temp, data = eaglet_2000_trap_nets)
summary(lm_temp_cpue)

ggplot(eaglet_2000_trap_nets, aes(temp, cpue)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")))

lm_bb <- lm(`No. Burbot` ~ depth, data = eaglet_2000_trap_nets)
summary(lm_bb)
ggplot(eaglet_2000_trap_nets, aes(depth, `No. Burbot`)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")))

