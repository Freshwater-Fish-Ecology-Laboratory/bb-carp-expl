# function to simulate m-array of tag-recovery data FOR EACH TAG TYPE
# with a term w for winter harvest (proportion of h)

yrs <- 10
ntags <- 100
h <- rep(0.2, yrs)
s <- rep(0.8, yrs)
l <- matrix(0.7, nrow = yrs, ncol = 2)  # Assuming it's a matrix with dimensions (yrs, 2)
w <- rep(0.05, yrs)

rel_yrs <- rep(1:yrs, each = 2)
rel_yrs <- rel_yrs[-length(rel_yrs)]
rel_pre <- cbind(rep(ntags, yrs), rep(ntags, yrs))
rel_pos <- cbind(c(rep(ntags, yrs - 1), NA), c(rep(ntags, yrs - 1), NA))

#######################################################
# Simulate m-array code with checks for negative probabilities: ----------------------------

create_marray <- function(rel_pre, rel_pos, rel_yrs, s, h, l, w) {
  
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
      pi[pre_idx[y], y, t] <- h[y] * l[y, t]
    } 
  }
  
  # Recovery probability for pre-harvest season fish recovered in years after year of tagging
  for (y in 1:(yrs - 1)) {
    for (j in (y + 1):yrs) {
      for (t in 1:2) {
        pi[pre_idx[y], j, t] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)]) * (1 - (w[y:(j - 1)] * h[y:(j - 1)]))) * 
          sum(h[j], w * h[j]) * l[j, t]
      }
    }
  }
  
  # Set all values before tagging year to 0
  for (y in 2:yrs) {
    for (j in 1:(y - 1)) {
      for (t in 1:2) {
        pi[pre_idx[y], j, t] <- 0
      }
    }
  }
  
  # Handle post-harvest season probabilities
  for (j in pos_idx) {
    for (t in 1:2) {
      pi[j, (rel_yrs[j] + 1):yrs, t] <- pi[j - 1, (rel_yrs[j] + 1):yrs, t] / (1 - h[(rel_yrs[j] + 1):yrs])
    }
  }
  
  # Set all values before tagging year for post-harvest season to 0
  for (y in 1:(yrs - 1)) {
    for (j in 1:y){
      for (t in 1:2) {
        pi[pos_idx[y], j, t] <- 0
      }
    }
  }
  
  #Probability of non-recovery
  for (j in 1:nrow(pi)) {
    for (t in 1:2) {
      recovery_sum <- sum(pi[j, 1:(yrs), t])
      if (recovery_sum > 1) {
        warning(paste("Warning: Recovery sum greater than 1 for row", j, "and tag", t))
        recovery_sum <- 1  # Avoid negative probabilities
      }
      pi[j, yrs + 1, t] <- 1 - recovery_sum
    }
  }
  
  # Simulate m-array of recoveries
  for (j in 1:nrow(rel)) {
    for (t in 1:2) {
      if (any(pi[j, , t] < 0)) {
        stop(paste("Negative probabilities detected in row", j, "and tag", t))
      }
      marr[j, , t] <- rmultinom(1, rel[j, t], pi[j, , t])
    }
  }
  
  return(marr)
}

marr <- create_marray(rel_pre, rel_pos, rel_yrs, s, h, l, w)

print(marr)



#original code
# create_marray <- function(rel_pre, rel_pos, rel_yrs, s, h, l, w) {
#   
#   # w proportion of harvest h that occurs in winter
#   
#   yrs <- nrow(rel_pre)
#   
#   rel <- matrix(NA, nrow = 2 * nrow(rel_pre), ncol = 2)
#   
#   for (t in 1:2) {
#     rel[, t] <- as.vector(rbind(rel_pre[, t], rel_pos[, t]))
#   }
#   
#   rel <- na.exclude(rel)
#   
#   pre_idx <- seq(1, nrow(rel), 2)
#   
#   pos_idx <- seq(2, nrow(rel), 2)
#   
#   marr <- array(NA, dim = c(nrow(rel), yrs + 1, 2))
#   
#   pi <- array(NA, dim = c(nrow(rel), yrs + 1, 2))
#   
#   # Recovery probability for pre-harvest season fish recovered in the year of tagging
#   for (y in 1:yrs) {
#     for (t in 1:2) {
#       pi[pre_idx[y], y, t] <- h[y] * l[y, t]    # Yr1 harvest rate * Yr1 reporting rate
#     } 
#   }
#   
#   # Recovery probability for pre-harvest season fish recovered in years after year of tagging
#   for (y in 1:(yrs - 1)) {
#     for (j in (y + 1):yrs) {
#       for (t in 1:2){
#         pi[pre_idx[y], j, t] <- prod(s[y:(j - 1)] * (1 - h[y:(j - 1)]) * (1 - (w[y:(j - 1)] * h[y:(j - 1)]))) * 
#           sum(h[j], w * h[j]) * l[j, t] # add term for harvest in winter...
#       }     # past years' survival rate * past year's probability of avoiding harvest *
#       # current year harvest rate * current year reporting rate
#     }
#   }
#   
#   # Recovery probability for pre-harvest season before year of tagging (all zero)
#   for (y in 2:yrs) {
#     for (j in 1:(y - 1)) {
#       for (t in 1:2) {
#         pi[pre_idx[y], j, t] <- 0
#       }
#     }
#   }
#   
#   # Recovery probability for post-harvest season fish (basically that of 
#   # pre-harvest fish after year of tagging divided by the probability of
#   # not being harvested the first year)
#   for (j in pos_idx) {
#     for (t in 1:2) {
#       pi[j, (rel_yrs[j] + 1):yrs, t] <- pi[j - 1, (rel_yrs[j] + 1):yrs, t] / (1 - h[(rel_yrs[j] + 1):yrs])
#     }
#   }
#   
#   # Recovery probability for pre-harvest season before year of tagging (all zero)
#   for (y in 1:(yrs - 1)) {
#     for (j in 1:y){
#       for (t in 1:2) {
#         pi[pos_idx[y], j, t] <- 0
#       }
#     }
#   }
#   
#   # Probability of non-recovery 
#   for (j in 1:nrow(pi)) {
#     for (t in 1:2) {
#       pi[j, yrs + 1, t] <- 1 - sum(pi[j, 1:(yrs), t])  # = 1 - the probability of recovery for all years
#     }
#   }
#   
#   # Simulate m-array of recoveries (last column will be the number of fish in
#   # a release that were not recovered)
#   for (j in 1:nrow(rel)) {
#     for (t in 1:2) {
#       marr[j, , t] <- rmultinom(1, rel[j, t], pi[j, , t])
#     }
#   }
#   return(marr)
# }
# 
# # Call the function and assign the result to a variable
# marr <- create_marray(rel_pre, rel_pos, rel_yrs, s, h, l, w)
# 
# print(marr)

