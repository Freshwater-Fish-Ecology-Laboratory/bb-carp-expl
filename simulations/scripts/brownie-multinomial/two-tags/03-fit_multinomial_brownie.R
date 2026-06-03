# Tag-recovery simulation code with MULTINOMIAL LIKELIHOOD 
# Modified to include 2 tag types:
# (non-reward tags with some reporting rate lambda, and 
# high-reward tags with reporting rate = 1.0)

# Adapted from Kery and Schaub (2012) Ch.8.3.1

###############################################
library(jagsUI)
library(MCMCvis)
###############################################

# Scenario parameters (read from jobs.txt) --------------------------------
drac <- TRUE

if (drac == TRUE) {
  source("03-fn_simulate_marray_winter_harvest.R")
  model_path <- "02-bugs_multinomial_brownie.R"
  args <- commandArgs(TRUE)
  nsim <- as.numeric(args[1]) # number of simulations
  yrs <- as.numeric(args[2]) # number of total years (including pre-season and post-season)
  ntags <- as.numeric(args[3]) # number of tags deployed per occasion
  h <- rep(as.numeric(args[4]), yrs) # harvest probability
  s <- rep(as.numeric(args[5]), yrs) # survival probability
  l <- matrix(c(rep(as.numeric(args[6]), yrs), rep(1, yrs)), nrow = yrs, ncol = 2) # reporting probability
  w <- rep(as.numeric(args[7]), yrs) # proportion of summer harvest that occurs in winter
  scn <- args[8] # scenario number
} else {
  source("simulations/codes/R/brownie/multinomial/two-tags/03-fn_simulate_marray_winter_harvest.R")
  model_path <- "simulations/codes/R/brownie/multinomial/two-tags/02-bugs_multinomial_brownie.R"
  nsim <- 3 # number of simulations
  yrs <- 10 # number of total years (including pre-season and post-season)
  ntags <- 100 # number of tags deployed per occasion
  h <- rep(0.2, yrs) # harvest probability
  s <- rep(0.9, yrs) # survival probability
  w <- rep(0.1, yrs) # proportion of summer harvest that occurs in winter
  l <- matrix(c(rep(0.9, yrs), rep(1, yrs)), nrow = yrs, ncol = 2) # reporting probability
  scn <- "scn1" # scenario number
  
}

# Fixed values simulation parameters --------------------------------------

rel_yrs <- rep(1:yrs, each = 2)
rel_yrs <- rel_yrs[-length(rel_yrs)]

rel_pre <- cbind(rep(ntags, yrs), rep(ntags, yrs))
rel_pos <- cbind(c(rep(ntags, yrs - 1), NA), c(rep(ntags, yrs - 1), NA))

sim_input <- list()
fit <- list()
mdn_s <- list()
mdn_h <- list()
mdn_l <- list()
Rhat <- list()
true_s <- list()
true_h <- list()
true_l <- list()
winter_h <- list()

start <- Sys.time()

for (i in 1:nsim) {
  
  marr <- create_marray(rel_pre, rel_pos, rel_yrs, s, h, l, w)
  
  sim_input[[i]] <- list(yrs = yrs, s = s, h = h, l = l, w = w, 
                         rel_pre = rel_pre, rel_pos = rel_pos)
  
  rel <- matrix(NA, nrow = 2 * nrow(rel_pre), ncol = 2)
  
  for (t in 1:2) {
    rel[, t] <- as.vector(rbind(rel_pre[, t], rel_pos[, t]))
  }
  
  rel <- na.exclude(rel)
  
  # Bundle data
  jags_data <- list(marr = marr, rel = rel, Y = yrs, R = nrow(rel), 
                    pre_idx = seq(1, nrow(rel), 2), 
                    pos_idx = seq(2, nrow(rel), 2),
                    rel_yrs = rel_yrs)
  
  # Initial values
  inits <- function(){list(mean_s = runif(1, 0.5, 1), mean_h = runif(1, 0, 0.3),
                          mean_l = runif(1, 0.5, 1))}
  
  # Parameters monitored
  parameters <- c("mean_s", "mean_h", "mean_l")
  
  # MCMC settings
  ni <- 100000
  nb <- ni / 2
  nt <- (ni - nb) / 500
  nc <- 4
  
  # Call JAGS from R --------------------------------------------------------
  
  fit[[i]] <- jagsUI(jags_data, inits = inits, parallel=TRUE, 
                parameters.to.save=parameters,  model_path, 
                n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, DIC=FALSE)
  true_s[i] <- s[1]
  true_h[i] <- h[1]
  true_l[i] <- l[1]
  winter_h[i] <- w[1]
  mdn_s[i] <- fit[[i]]$q50$mean_s
  mdn_h[i] <- fit[[i]]$q50$mean_h
  mdn_l[i] <- fit[[i]]$q50$mean_l
  Rhat[i] <- all(unlist(fit[[1]]$Rhat) < 1.1)

}

run_time <- Sys.time() - start
  
out <- list(s = mdn_s, h = mdn_h, l = mdn_l, fit = fit, 
            ts = true_s, th = true_h, tl = true_l, tw = winter_h,
            tsa = s[1], tha = h[1], tla = l[1], twa = w[1],
            Rhat = Rhat,
            sim = sim_input, rt = run_time)


# Save RDS file for output ------------------------------------------------

if (drac == TRUE) {
  saveRDS(out, paste("output_winter_", scn, ".rds", sep = ""))  
} else {
  saveRDS(out, paste("simulations/outputs/winter_harvest/output_winter_", scn, ".rds", sep = ""))  
}

MCMCsummary(fit[[i]], params = c("mean_s", "mean_h", "mean_l"))
MCMCtrace(fit[[i]], params = c("mean_s", "mean_h", "mean_l"),
          pdf = FALSE)
