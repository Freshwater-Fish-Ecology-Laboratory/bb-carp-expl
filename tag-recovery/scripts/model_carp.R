# JAGS model: multinomial likelihood and 2 tag types
model {
  
  # Priors ----
  for (y in 1:Y) {
    s[y] <- mean_s
    h[y] <- mean_h
    l[y, 1] <- mean_l 
    l[y, 2] <- 1
  }
  
  mean_s ~ dbeta(1, 1)
  mean_h ~ dbeta(1, 1)
  mean_l ~ dbeta(1, 1)
  
  # Multinomial probabilities ----
  
  # Recovery probability for pre-harvest season fish recovered in the year of tagging
  for (y in 1:Y) {
    for (t in 1:2) {
        pi[pre_idx[y], y, t] <- h[y] * l[y, t]
      }
  }
  
  # Recovery probability for pre-harvest season fish recovered in years after year of tagging
  for (y in 1:(Y - 1)) {
    for (j in (y + 1):Y) {
      for (t in 1:2) {
        pi[pre_idx[y], j, t] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)])) * h[j] * l[j, t]
      }
    }
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 2:Y) {
    for (j in 1:(y - 1)){
      for (t in 1:2) {
        pi[pre_idx[y], j, t] <- 0
      }
    }
  }
  
  # Recovery probability for pos-harvest season fish (basically that of 
  # pre-harvest fish after year of harvest divided by the probability of
  # not being harvested the first year)
  for (j in pos_idx) {
    for (t in 1:2) {
      pi[j, (rel_yrs[j] + 1):Y, t] <- pi[j - 1, (rel_yrs[j] + 1):Y, t] / (1 - h[(rel_yrs[j] + 1):Y])
    }
  }
  
  # Recovery probability for post-harvest season fish before year of tagging (all zero)
  for (y in 1:(Y - 1)) {
    for (j in 1:y){
      for (t in 1:2) {
        pi[pos_idx[y], j, t] <- 0
      }
    }
  }
  
  # Probability of non-recovery 
  for (j in 1:R) {
    for (t in 1:2) {
      pi[j, Y + 1, t] <- 1 - sum(pi[j, 1:Y, t])  
    }
  }
 
  # Likelihood
  
  for (r in 1:R) {
    for(t in 1:2) {
      m_array_carp[r, , t] ~ dmulti(pi[r, , t], rel[r, t])
    }
  }
}

