# Code to simulate reporting rate by anglers to inform recovery probability
# Recovery probability then becomes a parameter in the tag-recovery function.
# 2023-10-06

#Simple simple model with no difference in tag types, no creel; just a set reporting rate:

# tagged fish * probability of being caught using a set line = expected catches
# expected catches * harvest rate = expected harvests
# expected harvests * reporting rate = observed angler recoveries

#FIRST, probability of burbot being RECOVERED by an angler:
# PARAMETERS: tagged, ind_cap_prob, exploitation rate, harvest rate, survival rate, length of season

# estimate number of burbot actually recovered (regardless of whether they are reported)

tagged <- 115
expl <- 0.08

harvest.sim <- function(tagged, expl){
  harvested <- (tagged*expl)
  return(harvested)
}

harvested <- harvest.sim(tagged, expl)

# Calculate observed recoveries (actual recoveries x reporting rate, which we will play around with in
# various scenarios) :
rep_rate <- 0.8
reco.sim <- function(harvested, rep_rate){
  recovered <- (harvested*rep_rate)
  return(recovered)
}

recovered <- reco.sim(harvested, rep_rate)

# becomes more complicated if we introduce creel surveys and different tag reward types.

#Finally we want to get to recovery rate r (used in the joint recap-reco model)
#this is just the recovered tagged fish divided by total tagged fish

rec_rate.sim <- function(recovered, tagged){
  r <- (recovered/tagged)
  return(r)
}

r <- rec_rate.sim(recovered, tagged)


#creel approach - calculating reporting rate
#    y = Rc/Nc / (Rr/Nr - Rs/Nc) 
# where y = reporting rate of control tags
####### Rc = number of recoveries of control tags reported by anglers
####### Rr = number of recoveries of control tags solicited by researchers
####### Rs = number of recoveries of reward tags
####### Nc = number of control tags applied
####### Nr = number of rewards tags applied