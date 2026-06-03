# Code to run a simulation using the 2-tag multinomial brownie model, with scenario 
# similar to Carp Lake field study (i.e. same number of fish released per year, realistic
# h, s, and l based on external estimates / prior literature). 

# tag type 1 = no reward
# tag type 2 = high reward

library(jagsUI)
library(MCMCvis)
library(tidyverse)
library(Metrics)

# do not run this through DRAC:
source("simulations/codes/R/brownie/multinomial/two-tags/03-fn_simulate_marray_winter_harvest.R")
model_path <- "simulations/codes/R/brownie/multinomial/two-tags/02-bugs_multinomial_brownie.R"
nsim <- 100 # number of simulations
yrs <- 2 # number of total years (including pre-season and post-season)
h <- rep(0.05, yrs) # harvest probability
s <- rep(0.9, yrs) # survival probability
l <- matrix(c(rep(0.6, yrs), rep(1, yrs)), nrow = yrs, ncol = 2) # reporting probability

w <- rep(0.25, yrs) # proportion of summer harvest that occurs in winter

scn <- "test9w" # scenario number

# SIMULATION WITH CARP LAKE TAG-RELEASES:
ntags_pre_1 <- c(47,59)
ntags_post_1 <- c(11, NA)
ntags_pre_2 <- c(44, 63)
ntags_post_2 <- c(12, NA)

# Create matrices with the number of releases for each occasion:
rel_yrs <- rep(1:yrs, each = 2)
rel_yrs <- rel_yrs[-length(rel_yrs)]

rel_pre <- cbind(ntags_pre_1, ntags_pre_2)
rel_pos <- cbind(ntags_post_1, ntags_post_2)

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
            twa = w[1],
            Rhat = Rhat,
            sim = sim_input, rt = run_time)


# Save RDS file for output ------------------------------------------------
saveRDS(out, paste("simulations/outputs/output_2tag_", scn, ".rds", sep = ""))

MCMCsummary(fit[[i]], params = c("mean_s", "mean_h", "mean_l"))
MCMCtrace(fit[[i]], params = c("mean_s", "mean_h", "mean_l"),
          pdf = FALSE)

##############################################
# CarpLk sim output processing:
# output_2tag_carp <- readRDS("simulations/outputs/output_2tag_test9.rds")
file_paths_carp <- list.files(path = here::here("simulations/outputs/carp_sim_w/"), pattern = "*.rds", full.names = TRUE)
# Create an empty list to store data frames
df_list2 <- list()

for (i in 1:length(file_paths_carp)) {
  files <- readRDS(file_paths_carp[i])  # Read RDS file
  h <- files$h   # Access the variables h, s, and l
  s <- files$s
  l <- files$l
  true_h <- unlist(files$th)
  true_s <- unlist(files$ts)
  true_l <- unlist(files$tl)
  fit <- files$fit
  
  # Create a data frame with s, r, p
  df_tmp2 <- tibble(h = h, s = s, l = l, true_h = true_h, true_l = true_l, true_s = true_s, scn = as.integer(str_extract(file_paths_carp[i], "(?<=test)[0-9]*")))
  
  # Store the data frame in the list
  df_list2[[i]] <- df_tmp2
}

# Combine all data frames into one
df2 <- bind_rows(df_list2)
df2 <- df2 %>% group_by(scn) %>% 
  mutate(sim = 1:n())       # add a column for simulation number

# # tidy data 
df_long <- df2 %>%
  pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
  mutate(true_value = case_when(parameter == "h" ~ true_h, parameter == "s" ~ true_s, parameter == "l" ~ true_l)) %>%
  group_by(scn, parameter, true_value, true_h, true_s, true_l) %>%
  summarize(rmse = rmse(as.numeric(estimate), as.numeric(true_value)),
            rel_rmse = rmse /as.numeric(true_value),
            bias = bias(as.numeric(estimate), as.numeric(true_value)),
            abs_bias = abs(bias(as.numeric(estimate), as.numeric(true_value))),
            percent_bias = percent_bias(as.numeric(estimate), as.numeric(true_value))*100,
            mean_estimate = mean(as.numeric(estimate))) |>
  ungroup() |>
  # mutate(ntags = case_when(scn < 28 ~ 50, scn >= 28 & scn <= 54 ~ 100, scn >= 55 & scn <= 81 ~ 200, scn >= 82 & scn <= 108 ~ 50,
  #                          scn >= 109 & scn <= 135 ~ 100, scn >= 136 & scn <= 162 ~ 200, scn >= 163 & scn <= 189 ~ 50,
  #                          scn >= 190 & scn <= 216 ~ 100, scn >= 217 & scn <= 243 ~ 200),
  #        yrs = case_when(scn < 82 ~ 2, scn >= 82 & scn <= 162 ~ 5, scn >= 163 & scn <= 243 ~ 10)) %>%
  filter(!is.na(true_value))


# df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#   ggplot(aes(x = true_s, y = true_h, fill = Bias)) +
#   geom_tile() +
#   scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated survival rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery bias by simulated parameter values",
#        subtitle = "Carp Lake scenario using 2-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   # facet_grid(~true_s)+
#   theme(panel.spacing = unit(-0.3, "lines"))
# 
# df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
#   ggplot(aes(x = true_s, y = true_h, fill = RMSE))+
#   geom_tile() +
#   scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated survival rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery RMSE by simulated parameter values",
#        subtitle = "Carp Lake scenario using 2-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   # facet_grid(yrs~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines"))

###
df_summary <- df_long %>%
  group_by(true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), bias_mean = mean(bias, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))

hplot_carpsim <- ggplot(df_summary, aes(x = true_s, y = true_h, fill = RMSE_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4)) +
  geom_text(aes(label = round(RMSE_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated survival rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery RMSE by simulated parameter values",
       subtitle = "Carp Lake scenario with w = 0.25") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  # facet_grid(yrs ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='RMSE') 

bias_carpsim <- ggplot(df_summary, aes(x = true_s, y = true_h, fill = bias_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(-0.1, 0.1)) +
  geom_text(aes(label = round(bias_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated survival rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery bias by simulated parameter values",
       subtitle = "Carp Lake scenario with w = 0.25") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  # facet_grid(yrs ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='Bias') 
###




# df_tmp <- tibble(h = h, s = s, l = l, scn = "9")
# df_tru <- df_tmp %>%
#   mutate(h_true = 0.05, s_true = 0.90, l_true = 0.60, yrs = 2,
#          sim = 1:n()) 
# 
# #Re-format
# df_long <- df_tru %>%
#   pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
#   pivot_longer(cols = c(h_true, s_true, l_true), names_to = "true_parameter", values_to = "true_value") %>%
#   filter(paste0(parameter, "_true") == true_parameter) %>%
#   select(-true_parameter)
#  
# # Calculate RMSE and bias
# df_long <- df_long %>%
#   group_by(scn, parameter, true_value) %>%
#   summarize(estimate = estimate,
#             rmse = rmse(as.numeric(estimate), as.numeric(true_value)),
#             bias = bias(as.numeric(estimate), as.numeric(true_value)),
#             mean = mean(as.numeric(estimate)),
#             sd = sd(as.numeric(estimate))) %>%
#   ungroup()
# 


# ##### table code

library(tibble)

sim_results <- tribble(
  ~s, ~h, ~l, 
  ~harvest_mean, ~harvest_sd, ~harvest_bias, ~harvest_rmse,
  ~survival_mean, ~survival_sd, ~survival_bias, ~survival_rmse,
  ~reporting_mean, ~reporting_sd, ~reporting_bias, ~reporting_rmse,
  
  0.5, 0.1, 0.6, 0.103, 0.027, 0.003, 0.027, 0.522, 0.157,  0.022, 0.157, 0.571, 0.150, -0.030, 0.153,
  0.5, 0.2, 0.6, 0.196, 0.033, -0.004, 0.033, 0.524, 0.149,  0.024, 0.151, 0.594, 0.153, -0.006, 0.152,
  0.5, 0.05, 0.6, 0.053, 0.016,  0.003, 0.016, 0.509, 0.177,  0.009, 0.177, 0.557, 0.165, -0.043, 0.170,
  0.8, 0.2, 0.6, 0.200, 0.029, -0.0002794, 0.029, 0.744, 0.115, -0.056, 0.128, 0.626, 0.129,  0.026, 0.131,
  0.8, 0.1, 0.6, 0.110, 0.024, 0.010, 0.026, 0.687, 0.142, -0.113, 0.181, 0.587, 0.158, -0.013, 0.158,
  0.8, 0.05, 0.6, 0.059, 0.016,  0.009, 0.018, 0.639, 0.164, -0.161, 0.229, 0.581, 0.152, -0.019, 0.152,
  0.9, 0.1, 0.6, 0.114, 0.023,  0.014, 0.027, 0.710, 0.162, -0.190, 0.249, 0.615, 0.148,  0.015, 0.148,
  0.9, 0.2, 0.6, 0.202, 0.028, 0.002, 0.028, 0.795, 0.095, -0.105, 0.142, 0.633, 0.139, 0.033, 0.143,
  0.9, 0.05, 0.6, 0.062, 0.017,  0.012, 0.020, 0.636, 0.163, -0.264, 0.310, 0.519, 0.168, -0.081, 0.186
)


# Prepare the data with true blank spacers
sim_results_spaced <- sim_results %>%
  mutate(
    s_label = paste0("s = ", s),
    h = paste0("h = ", h),
    spacer1 = "",  # Use "" not NA
    spacer2 = ""
  ) %>%
  arrange(s, match(h, c("h = 0.05", "h = 0.1", "h = 0.2"))) %>%
  select(
    s_label, h,
    harvest_mean, harvest_sd, harvest_bias, harvest_rmse,
    spacer1,
    survival_mean, survival_sd, survival_bias, survival_rmse,
    spacer2,
    reporting_mean, reporting_sd, reporting_bias, reporting_rmse
  )

# Build the gt table
sim_results_spaced %>%
  gt(groupname_col = "s_label") %>%
  cols_label(
    h = "",
    spacer1 = "",  # Spacer columns
    spacer2 = "",
    harvest_mean = "Mean",
    harvest_sd = "SD",
    harvest_bias = "Bias",
    harvest_rmse = "RMSE",
    survival_mean = "Mean",
    survival_sd = "SD",
    survival_bias = "Bias",
    survival_rmse = "RMSE",
    reporting_mean = "Mean",
    reporting_sd = "SD",
    reporting_bias = "Bias",
    reporting_rmse = "RMSE"
  ) %>%
  tab_spanner(label = "Harvest Estimate", columns = harvest_mean:harvest_rmse) %>%
  tab_spanner(label = "Survival Estimate", columns = survival_mean:survival_rmse) %>%
  tab_spanner(label = "Reporting Estimate", columns = reporting_mean:reporting_rmse) %>%
  fmt_number(columns = where(is.numeric), decimals = 3) %>%
  tab_header(
    title = "Carp Lake simulation results"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_width(
    spacer1 ~ px(15),
    spacer2 ~ px(15)
  )

#####


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


ggplot(df_long, aes(x = parameter, y = as.numeric(estimate), fill = parameter)) +
  scale_fill_brewer(palette = "GnBu", direction = -1)+
  geom_boxplot() +
  geom_hline(yintercept = 0.1, color = "darkgrey", linetype = "dashed") +
  annotate("text", x=0.5, y=0.12, label="h = 0.1", size = 2.7)+
  geom_hline(yintercept = 0.6, color = "darkgrey", linetype = "dashed") +
  annotate("text", x=0.5, y=0.62, label="l = 0.6", size = 2.7)+
  geom_hline(yintercept = 0.5, color = "darkgrey", linetype = "dashed")+
  annotate("text", x=0.5, y=0.52, label="s = 0.5", size = 2.7)+
  theme_minimal() +
  theme(legend.position="none")+
  scale_x_discrete(labels=c("h" = "harvest rate", "l" = "tag-reporting rate", "s" = "survival rate"))+
  labs(title = paste("Parameter Estimates for Carp Lake Scenario"),
       x = "",
       y = "Estimated values from 100 simulations")



## Table of expected recoveries per year - for figures
library(gt)
library(gtExtras)

exprec <- read.csv("simulations/data/expectedrecs.csv")

gt(exprec) %>%
  tab_header(title = "Expected number of tag recoveries for each year of study") %>%
  cols_label(
    Tagging.year = "Tagging year",
    Tagging.season = "Tagging season",
    Number.tagged = "Number tagged",
    Year.1 = "Year 1",
    Year.2 = "Year 2",
    Year.3 = "Year 3",
    Year.4 = "Year 4",
    Year.5 = "Year 5"
  ) %>%
  sub_missing(missing_text = "") %>%
  tab_options(table.font.size = 13) %>%
  gt_add_divider(columns = "Number.tagged", style="dashed", color = "lightgrey")


