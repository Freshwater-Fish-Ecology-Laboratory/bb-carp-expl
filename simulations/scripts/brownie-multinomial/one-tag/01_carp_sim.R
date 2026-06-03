# Simulating a Carp Lake scenario with the **1-tag model**

library(jagsUI)
library(MCMCvis)
library(tidyverse)
library(Metrics)

source("simulations/codes/R/brownie/multinomial/one-tag/01-fn_simulate_marray.R")
model_path <- "simulations/codes/R/brownie/multinomial/one-tag/01-bugs_multinomial_brownie.R"
nsim <- 100 # number of simulations
yrs <- 2 # number of total years (including pre-season and post-season)
h <- rep(0.1, yrs) # harvest probability
s <- rep(0.8, yrs)  # survival probability
l <- rep(0.7, yrs)  # reporting probability
scn <- "test" # scenario number

# SIMULATION WITH CARP LAKE TAG-RELEASES:

ntags_pre <- c(91, 122)
ntags_post <- c(23, NA)

rel_yrs <- rep(1:yrs, each = 2)
rel_yrs <- rel_yrs[-length(rel_yrs)]

rel_pre <- ntags_pre      # fish released each pre-season
rel_pos <- ntags_post      # fish released each post-season

sim_input <- list()
fit <- list()
mdn_s <- list()
mdn_h <- list()
mdn_l <- list()
Rhat <- list()

start <- Sys.time()

for (i in 1:nsim) {
  
  marr <- create_marray(rel_pre, rel_pos, rel_yrs, s, h, l)
  
  sim_input[[i]] <- list(yrs = yrs, s = s, h = h, l = l, rel_pre = rel_pre,
                         rel_pos = rel_pos, rel_yrs = rel_yrs)
  
  rel <-  as.vector(na.exclude(as.vector((rbind(rel_pre, rel_pos)))))
  
  # Bundle data
  jags_data <- list(marr = marr, rel = rel, Y = yrs, R = length(rel), 
                    pre_idx = seq(1, length(rel), 2), 
                    pos_idx = seq(2, length(rel), 2),
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
  
  mdn_s[i] <- fit[[i]]$q50$mean_s
  mdn_h[i] <- fit[[i]]$q50$mean_h
  mdn_l[i] <- fit[[i]]$q50$mean_l
  Rhat[i] <- all(unlist(fit[[1]]$Rhat) < 1.1)
  
}

run_time <- Sys.time() - start

out <- list(s = mdn_s, h = mdn_h, l = mdn_l, fit = fit, 
            Rhat = Rhat,
            sim = sim_input, rt = run_time)


# Save RDS file for output ------------------------------------------------
saveRDS(out, paste("simulations/outputs/01-tags/output_1tag_", scn, ".rds", sep = ""))  

MCMCsummary(fit[[i]], params = c("mean_s", "mean_h", "mean_l"))
MCMCtrace(fit[[i]], params = c("mean_s", "mean_h", "mean_l"),
          pdf = FALSE)

################################################
# Processing output
output_1tag_carp <- readRDS("simulations/outputs/01-tags/output_1tag_test.rds")
h <- output_2tag_carp$h
s <- output_2tag_carp$s
l <- output_2tag_carp$l
df_tmp <- tibble(h = h, s = s, l = l, scn = "carp")
df_tru <- df_tmp %>%
  mutate(h_true = 0.10, s_true = 0.80, l_true = 0.70, yrs = 2)


#Re-format
df_long <- df_tru %>%
  pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
  pivot_longer(cols = c(h_true, s_true, l_true), names_to = "true_parameter", values_to = "true_value") %>%
  filter(paste0(parameter, "_true") == true_parameter) %>%
  select(-true_parameter)

# Calculate RMSE and bias
df_long <- df_long %>%
  group_by(scn, parameter) %>%
  mutate(rmse = sqrt(mean((as.numeric(estimate) - as.numeric(true_value)^2), na.rm = TRUE)),
         bias = bias(as.numeric(true_value), as.numeric(estimate)))

# Plot RMSE and bias for each parameter. 
ggplot(df_long, aes(x = parameter, y = rmse)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = paste("RMSE of Parameter Estimates for Carp Lake Scenario"),
       x = "Parameter",
       y = "RMSE")

ggplot(df_long, aes(x = parameter, y = bias)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = paste("Bias in Parameter Estimates for Carp Lake Scenario"),
       x = "Parameter",
       y = "Bias")

