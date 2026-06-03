# Burbot tag-recovery simulations - Module 2
# 20 Sept. 2023
# Code to simulate burbot capture and tagging process, generating tagged sample size.

# parameters: 
## gear: cod traps, set-lines, trap nets or hoop traps
## range of CPUE for each gear type
## effort (hours)
## N

# output: number of burbot tagged and released

# Catch = CPUE * Effort
#Cod traps
sample_bb <- function(cpue, effort) {
  catch <- (cpue*effort)
  return(catch)
}

sample_bb(0.05, 3000)

#For these example parameter values, C = 119 burbot tagged.


########################################################################


## Calculate catchability coefficient using equation C = qED
# so q = C/ED

cpue_to_q <- function(catch, effort, density) {
  q <- (catch / (effort*density))
  return(q)
}

cpue_to_q(119, 4774, 0.26)

# So with these cod trap parameter values, q = 0.096

#MODULE 2a - individual capture probability
## divide catch by estimated population size for the lake -> individual capture probability

prob_capture <- function(catch, num_bb) {
  prob_capture <- (catch/num_bb)
  return(prob_capture)
}

prob_capture(119, 1276)

# So here the probability of capturing an individual burbot in the population is 0.093

############ NEXT...
## Use individual capture probability to simulate capture history for each individual in
# the population

## Simulate removals of burbot by anglers 

## If angler removes a tagged burbot, simulate reporting of the tag with reporting rate 