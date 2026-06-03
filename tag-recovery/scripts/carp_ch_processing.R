library(readr)
library(ggplot2)
library(tidyverse)

# Code to process and format capture histories for Carp Lake tagged burbot
# Jan 2024

carp_ch_matrix <- read_csv("carp_lk/data/raw/carp_ch_matrix.csv")

#visual

recaps <- carp_ch_matrix %>%
  filter(reco_method == "phone" | reco_method == "creel" | reco_method == "trap")
tag_type <- recaps$`tag type`
recovery_method <- recaps$`recovery method`
ggplot(recaps, aes(x = recovery_method, fill = tag_type)) +
  geom_bar(position = "stack")

#summary of recoveries:
# 236 burbot tagged
# 118 with Reward tags, 118 with Standard tags
# 7 burbot recovered via creel
# 10 burbot recovered via phone line
# 12 burbot recaptured in traps
# 207 burbot not recovered or recaptured

sum(reco_method == "creel")
sum(reco_method == "phone")
sum(reco_method == "not recovered")
sum(tag_type == "R")
sum(tag_type == "S")

# Phone recoveries: 8 Reward tags, 2 Standard tags
# Creel recoveries: 2 Reward tags, 5 Standard tags
# Unrecovered burbot: 101 Reward tags, 106 Standard tags
# Recaptures: 7 Reward tags, 5 Standard tags

sum(tag_type == "R" & reco_method == "phone")
sum(tag_type == "S" & reco_method == "phone")
sum(tag_type == "R" & reco_method == "creel")
sum(tag_type == "S" & reco_method == "creel")
sum(tag_type == "R" & reco_method == "not recovered")
sum (tag_type == "S" & reco_method == "not recovered")

# MATRIX based on Kleiven et al (2016)

# State codes:
# 0 = not detected
# 1 = recaptured with cod trap (released alive)
# 2 = recovered via phone line (harvested)
# 3 = recovered via creel survey (harvested)

n.states <- 5
n.occasions <- 5
tot_tags <- nrow(carp_ch_matrix)
PSI.STATE <- array(NA, dim=c(n.states, n.states, tot_tags, n.occasions-1))

for (i in 1:tot_tags){
  for (t in 1:(n.occasions-1)){
    PSI.STATE[,,i,t] <- matrix(c(
      s*F, s*(1-F), 1-s, 0, #removed [t] from these state probabilities
      0,   s,       1-s, 0,
      0,   0,       0,   1,
      0,   0,       0,   1), nrow = n.states, byrow = TRUE)
  } #t
} #i

