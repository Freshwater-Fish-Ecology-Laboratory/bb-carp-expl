model {
 
  # Priors ----
  for (y in 1:Y){
    s[y] <- mean_s
    h[y] <- mean_h
    l[y] <- mean_l
  }
  mean_s ~ dbeta(1, 1) # mean s, h, and l modeled as beta distribution
  mean_h ~ dbeta(1, 1)
  mean_l ~ dbeta(1, 1) 
  
# Multinomial probabilities ----

  # Recovery probability for pre-harvest season fish recovered in the year of tagging
  for (y in 1:Y) {
    pi[pre_idx[y], y] <- h[y] * l[y]
  }
  
  # Recovery probability for pre-harvest season fish recovered in years after year of tagging
  for (y in 1:(Y - 1)) {
    for (j in (y + 1):Y) {
      pi[pre_idx[y], j] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)])) * h[j] * l[j]
    }
  }
  
  # Recovery probability for pre-harvest season before year of tagging (all zero)
  for (y in 2:Y) {
    for (j in 1:(y - 1)){
      pi[pre_idx[y], j] <- 0
    }
  }
  
  # Recovery probability for pos-harvest season fish (basically that of 
  # pre-harvest fish after year of harvest divided by the probability of
  # not being harvested the first year)
  for (j in pos_idx) {
    pi[j, (rel_yrs[j] + 1):Y] <- pi[j - 1, (rel_yrs[j] + 1):Y] / (1 - h[(rel_yrs[j] + 1):Y])
  }
  
  # Recovery probability for post-harvest season fish before year of tagging (all zero)
  for (y in 1:(Y - 1)) {
    for (j in 1:y){
      pi[pos_idx[y], j] <- 0
    }
  }
  
  # Probability of non-recovery 
  for (j in 1:R) {
    pi[j, Y + 1] <- 1 - sum(pi[j, 1:Y])  
  }
  
# Likelihood

  for (r in 1:R) {
    marr[r, ] ~ dmulti(pi[r, ], rel[r])
  }

}

