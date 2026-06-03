# function to simulate m-array of tag-recovery data FOR EACH TAG TYPE
# with a term w for winter harvest (proportion of h)

create_marray <- function(rel_pre, rel_pos, rel_yrs, s, h, l, w) {
  
  # w proportion of harvest h that occurs in winter
  
  yrs <- nrow(rel_pre)
  
  rel <- matrix(NA, nrow = 2 * nrow(rel_pre), ncol = 2)
  
  for (t in 1:2) {
    rel[, t] <- as.vector(rbind(rel_pre[, t], rel_pos[, t]))
  }

  rel <- na.exclude(rel)
  
  pre_idx <- seq(1, nrow(rel), 2)

  pos_idx <- seq(2, nrow(rel), 2)

  marr <- array(NA, dim = c(nrow(rel), yrs + 1, 2))
  
  pi <- array(NA, dim = c(nrow(rel), yrs + 1, 2))

  # Recovery probability for pre-harvest season fish recovered in the year of tagging
  for (y in 1:yrs) {
    for (t in 1:2) {
      pi[pre_idx[y], y, t] <- h[y] * l[y, t]    # Yr1 harvest rate * Yr1 reporting rate
    } 
  }
  
  # Recovery probability for pre-harvest season fish recovered in years after year of tagging
  for (y in 1:(yrs - 1)) {
    for (j in (y + 1):yrs) {
      for (t in 1:2){
      pi[pre_idx[y], j, t] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)]) * (1 - (w[y:(j - 1)] * h[y:(j - 1)]))) * sum(h[j] * l[j, t], w[j] * h[j] * l[j, t]) # add term for harvest in winter...
    }     # past years' survival rate * past year's probability of avoiding harvest *
          # current year harvest rate * current year reporting rate
    }
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 2:yrs) {
    for (j in 1:(y - 1)) {
      for (t in 1:2) {
        pi[pre_idx[y], j, t] <- 0
      }
    }
  }
  
  # Recovery probability for post-harvest season fish (basically that of 
  # pre-harvest fish after year of tagging divided by the probability of
  # not being harvested the first year)
  for (j in pos_idx) {
    for (t in 1:2) {
      pi[j, (rel_yrs[j] + 1):yrs, t] <- pi[j - 1, (rel_yrs[j] + 1):yrs, t] / (1 - h[(rel_yrs[j] + 1):yrs])
    }
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 1:(yrs - 1)) {
    for (j in 1:y){
      for (t in 1:2) {
      pi[pos_idx[y], j, t] <- 0
    }
    }
  }
  
  # Probability of non-recovery 
  for (j in 1:nrow(pi)) {
    for (t in 1:2) {
    pi[j, yrs + 1, t] <- 1 - sum(pi[j, 1:(yrs), t])  # = 1 - the probability of recovery for all years
    }
  }
  
  # Simulate m-array of recoveries (last column will be the number of fish in
  # a release that were not recovered)
  for (j in 1:nrow(rel)) {
    for (t in 1:2) {
      marr[j, , t] <- rmultinom(1, rel[j, t], pi[j, , t])
    }
  }
  return(marr)
}
