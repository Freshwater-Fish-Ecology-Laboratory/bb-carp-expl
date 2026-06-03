create_marray <- function(rel_pre, rel_pos, rel_yrs, s, h, l) {
  
  yrs <- length(rel_pre)
  
  rel <- as.vector(rbind(rel_pre, rel_pos))
  rel <- as.vector(na.exclude(rel))
  
  pre_idx <- seq(1, length(rel), 2)
  
  pos_idx <- seq(2, length(rel), 2)
  
  marr <- matrix(NA, nrow = length(rel), ncol = yrs + 1)
  
  pi <- matrix(NA, nrow = length(rel), ncol = yrs + 1)
  
  # Recovery probability for pre-harvest season fish recovered in the year of tagging
  for (y in 1:yrs) {
   pi[pre_idx[y], y] <- h[y] * l[y]
  }
  
  # Recovery probability for pre-harvest season fish recovered in years after year of tagging
  for (y in 1:(yrs - 1)) {
    for (j in (y + 1):yrs) {
      pi[pre_idx[y], j] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)])) * h[j] * l[j]
    }
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 2:yrs) {
    for (j in 1:(y - 1)){
      pi[pre_idx[y], j] <- 0
    }
  }
  
  # Recovery probability for pos-harvest season fish (basically that of 
  # pre-harvest fish after year of harvest divided by the probability of
  # not being harvested the first year)
  for (j in pos_idx) {
    pi[j, (rel_yrs[j] + 1):yrs] <- pi[j - 1, (rel_yrs[j] + 1):yrs] / (1 - h[(rel_yrs[j] + 1):yrs])
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 1:(yrs - 1)) {
    for (j in 1:y){
      pi[pos_idx[y], j] <- 0
    }
  }
  
  # Probability of non-recovery 
  for (j in 1:nrow(pi)) {
    pi[j, yrs + 1] <- 1 - sum(pi[j, 1:(yrs)])  
  }

  # Simulate m-array of recoveries (last column will be the number of fish in
  # a release that were not recovered)
  for (j in 1:length(rel)) {
    marr[j, ] <- rmultinom(1, rel[j], pi[j, ])
  }

  return(marr)
  }
