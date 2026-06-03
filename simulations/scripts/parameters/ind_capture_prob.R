# Module 2 - code to derive individual capture probability based on CPUE and burbot abundance in lake
# Oct. 18 2023

source("simulations/codes/R/generate_bb_pop.R")

####################### POISSON APPROACH ####################### 
cpue <- 0.025 # catch per unit effort using cod traps
n_traps <- 20 # number of traps available to be deployed
n_eff <- 1600 # effort (hours)

exp_bb <- cpue*(n_traps+n_eff)

# For each occasion:
tagged <- rpois(1, exp_bb)
tagged

# Over the entire study:
n.occasions <- 7
total_tags <- tagged*n.occasions
total_tags

# Get an estimate of total burbot tagged: 
# (1) calculate an expected number of burbot caught for a given CPUE, number of traps and amount of effort.
# (2) simulate numbers of burbot tagged over multiple repeated sampling occasions, using a Poisson distribution.
# (3) Sum the simulated tagged burbot across all occasions to get total tags deployed.

#  EXAMPLE:
# We had 3 main tagging occasions at Carp Lk: (1) Spring 2022 (2) Fall 2022 (3) Spring 2023.
# Each occasion can be further divided into number of weeks, since they had different durations.
# Occasion (1) = 2 weeks, 3155h; Occasion (2) = 1 week, 1619h; Occasion (3) = 4 weeks, 6328h.
# ~1600 hrs per week, with 20 cod traps * 7 repetitions

# Running this fn with cpue = 0.025, n_traps = 20, n_eff = 1600, and 7 sampling reps gives total tagged values close to
# what we actually observed at Carp Lake using cod traps (total 251 burbot captured; 238 burbot tagged).
# Note: Number of fish tagged was less than total # caught because several were recaptures, and a few were too small to tag.

############################################## 
