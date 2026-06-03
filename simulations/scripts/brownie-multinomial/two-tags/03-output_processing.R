# Code to read .rds output files for 2-tag multinomial tag-recovery model including
# a term for winter harvest and process results
# last edit Feb 19 2025

library(MCMCvis)
library(here)
library(tidyverse)
library(gridExtra)
library(grid)
library(Metrics)
library(gt)

file_paths3 <- list.files(path = here::here("simulations/outputs/winter_harvest/"), pattern = "*.rds", full.names = TRUE)
df_list3 <- list()

for (i in 1:length(file_paths3)) {
  files <- readRDS(file_paths3[i])  # Read RDS file
  h <- unlist(files$h)   # Access the variables h, s, and l
  s <- unlist(files$s)
  l <- unlist(files$l)
  w <- unlist(files$w)
  true_h <- unlist(files$th)
  true_s <- unlist(files$ts)
  true_l <- unlist(files$tl)
  winter_h <- unlist(files$tw)
  fit <- files$fit
  
  # Create a data frame with h, s, l
  df_tmp3 <- tibble(h = h, s = s, l = l, w = w, true_h = true_h, true_l = true_l, true_s = true_s, winter_h = winter_h, 
                    scn = as.integer(str_extract(file_paths3[i], "(?<=scn)[0-9]*")))
  
  # Store the data frame in the list
  df_list3[[i]] <- df_tmp3
}

df3 <- bind_rows(df_list3)
df3 <- df3 %>% group_by(scn) %>% 
  mutate(sim = 1:n())       # add a column for simulation number

# check Rhat values -------------------------------------------------------
# Loop through all files:
for (i in 1:length(files)){
  mcmc <- MCMCsummary(files$fit[[1]], params = c("mean.h", "mean.s", "mean.l"))
  rhat <- mcmc$Rhat
  check <- all(rhat < 1.1)
}
filter(mcmc, Rhat > 1.1)

# # tidy data 
df_long <- df3 %>%
  pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
  mutate(true_value = case_when(parameter == "h" ~ true_h, parameter == "s" ~ true_s, parameter == "l" ~ true_l)) %>%
  group_by(scn, parameter, true_value, true_h, true_s, true_l, winter_h) %>%
  summarize(rmse = rmse(as.numeric(estimate), as.numeric(true_value)),
            bias = bias(as.numeric(estimate), as.numeric(true_value)),
            mean_estimate = mean(as.numeric(estimate))) |>
  ungroup() |>
  mutate(ntags = case_when(scn < 28 ~ 50, scn >= 28 & scn <= 54 ~ 100, scn >= 55 & scn <= 81 ~ 200, scn >= 82 & scn <= 108 ~ 50, 
                           scn >= 109 & scn <= 135 ~ 100, scn >= 136 & scn <= 162 ~ 200, scn >= 163 & scn <= 189 ~ 50, 
                           scn >= 190 & scn <= 216 ~ 100, scn >= 217 & scn <= 243 ~ 200, scn >= 244 & scn <= 270 ~ 50,
                           scn >= 271 & scn <= 297 ~ 100, scn >= 298 & scn <= 324 ~ 200, scn >= 325 & scn <= 351 ~ 50,
                           scn >= 352 & scn <= 378 ~ 100, scn >= 379 & scn <= 405 ~ 200, scn >= 406 & scn <= 432 ~ 50,
                           scn >= 433 & scn <= 459 ~ 100, scn >= 460 & scn <= 486 ~ 200, scn >= 487 & scn <= 513 ~ 50,
                           scn >= 514 & scn <= 540 ~ 100, scn >= 541 & scn <= 567 ~ 200, scn >= 568 & scn <= 594 ~ 50,
                           scn >= 595 & scn <= 621 ~ 100, scn >= 622 & scn <= 648 ~ 200, scn >= 649 & scn <= 675 ~ 50,
                           scn >= 676 & scn <= 702 ~ 100, scn >= 703 & scn <= 729 ~ 200),
         yrs = case_when(scn < 82 ~ 2, scn >= 82 & scn <= 162 ~ 5, scn >= 163 & scn <= 243 ~ 10,
                         scn >= 244 & scn <= 324 ~ 2, scn >= 325 & scn <= 405 ~ 5, scn >= 406 & scn <= 486 ~ 10,
                         scn >= 487 & scn <= 567 ~ 2, scn >= 568 & scn <= 648 ~ 5, scn >= 649 & scn <= 729 ~ 10),
         winter_h = winter_h) %>%
  filter(!is.na(true_value))

# Look at mean error by parameter -----------------------------------------

h_data <- df_long %>%
  filter(parameter == "h")
s_data <- df_long %>%
  filter(parameter == "s")
l_data <- df_long %>%
  filter(parameter == "l")

mean(h_data$bias)
mean(h_data$rmse)

mean(s_data$bias)
mean(s_data$rmse)

mean(l_data$bias)
mean(l_data$rmse)

# Heatplots showing BIAS by parameter values and yrs ----------------------
# (hplot_y_bias <- df_long %>% 
#    mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#    ggplot(aes(x = true_l, y = true_h, fill = Bias))+
#    geom_tile() +
#    scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#    xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#    theme_minimal()+
#    labs(title = "Tag-recovery bias by simulated parameter values and study duration",
#         subtitle = "2-tag model with winter harvest")+
#    theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#    facet_grid(yrs~true_s, labeller = label_both)+
#    theme(panel.spacing = unit(-0.3, "lines")))

# ggsave("simulations/outputs/winter_harvest/hplot_y_bias.png")
# 
# # with ntags:
# (hplot_n_bias <- df_long %>% 
#     mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#     ggplot(aes(x = true_l, y = true_h, fill = Bias))+
#     geom_tile() +
#     scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#     xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#     theme_minimal()+
#     labs(title = "Tag-recovery bias by simulated parameter values and tags deployed",
#          subtitle = "2-tag model with winter harvest")+
#     theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#     facet_grid(ntags~true_s, labeller = label_both)+
#     theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/winter_harvest/hplot_n_bias.png")


# Heatplots showing RMSE by parameters and yrs ----------------------------
(hplot_y_rmse <- df_long %>% 
   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
   ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
   geom_tile() +
   scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
   theme_minimal()+
   labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
        subtitle = "2-tag model with winter harvest")+
   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
   facet_grid(yrs~true_s, labeller = label_both)+
   theme(panel.spacing = unit(-0.3, "lines")))

ggsave("simulations/outputs/winter_harvest/hplot_y_rmse.png")

# by ntags
(hplot_n_rmse <- df_long %>% 
    mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
    ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
    geom_tile() +
    scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
    xlab("simulated reporting rate") + ylab("simulated harvest rate") +
    theme_minimal()+
    labs(title = "Tag-recovery RMSE by simulated parameter values and tags deployed",
         subtitle = "2-tag model with winter harvest")+
    theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
    facet_grid(ntags~true_s, labeller = label_both)+
    theme(panel.spacing = unit(-0.3, "lines")))

ggsave("simulations/outputs/winter_harvest/hplot_n_rmse.png")

# (hplot_w_bias <- df_long %>% 
#   # filter(!is.na(bias)) %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#   ggplot(aes(x = true_l, y = true_h, fill = Bias))+
#   geom_tile() +
#   scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery bias by simulated parameter values and rate of harvest occurring
#        in winter",
#        subtitle = "2-tag model with winter harvest")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   facet_grid(winter_h~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/winter_harvest/hplot_w_bias.png")

# summarize RMSE by facet + cell grouping
df_summary <- df_long %>%
  group_by(winter_h, true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), bias_mean = mean(bias, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))


hplot_w_rmse <- ggplot(df_summary, aes(x = true_l, y = true_h, fill = RMSE_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4)) +
  geom_text(aes(label = round(RMSE_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated reporting rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery RMSE by simulated parameter values and proportion of off-season harvest",
       subtitle = "2-tag model") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  facet_grid(winter_h ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='RMSE') 

hplot_w_bias <- ggplot(df_summary, aes(x = true_l, y = true_h, fill = bias_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(-0.05, 0.05)) +
  geom_text(aes(label = round(bias_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated reporting rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery bias by simulated parameter values and proportion of off-season harvest",
       subtitle = "2-tag model") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  facet_grid(winter_h ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='Bias')

# (hplot_w_rmse <- df_long %>% 
#     mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
#     ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
#     geom_tile() +
#     scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#     xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#     theme_minimal()+
#     labs(title = "Tag-recovery RMSE by simulated parameter values and rate of harvest occurring
#        in winter",
#          subtitle = "2-tag model with winter harvest")+
#     theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#     facet_grid(winter_h~true_s, labeller = label_both)+
#     theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/winter_harvest/hplot_w_rmse.png")

# Mean bias and RMSE by value of yrs:
yrs2 <- df_long %>%
  filter(yrs == 2)
mean(yrs2$bias)
mean(yrs2$rmse)

yrs5 <- df_long %>%
  filter(yrs == 5)
mean(yrs5$bias)
mean(yrs5$rmse)

yrs10 <- df_long %>%
  filter(yrs == 10)
mean(yrs10$bias)
mean(yrs10$rmse)

# Mean bias and RMSE by value of ntags
tags50 <- df_long %>%
  filter(ntags == 50)
mean(tags50$bias)
mean(tags50$rmse)

tags100 <- df_long %>%
  filter(ntags == 100)
mean(tags100$bias)
mean(tags100$rmse)

tags200 <- df_long %>%
  filter(ntags == 200)
mean(tags200$bias)
mean(tags200$rmse)

# Mean bias and RMSE by value of h:
h_05 <- df_long %>%
  filter(true_h == 0.05)
mean(h_05$bias)
mean(h_05$rmse)

h_10 <- df_long %>%
  filter(true_h == 0.10)
mean(h_10$bias)
mean(h_10$rmse)

h_20 <- df_long %>%
  filter(true_h == 0.20)
mean(h_20$bias)
mean(h_20$rmse)

# By value of s:
s_5 <- df_long %>%
  filter(true_s == 0.5)
mean(s_5$bias)
mean(s_5$rmse)

s_8 <- df_long %>%
  filter(true_s == 0.8)
mean(s_8$bias)
mean(s_8$rmse)

s_9 <- df_long %>%
  filter(true_s == 0.9)
mean(s_9$bias)
mean(s_9$rmse)

# By value of l:
l_3 <- df_long %>%
  filter(true_l == 0.3)
mean(l_3$bias)
mean(l_3$rmse)

l_7 <- df_long %>%
  filter(true_l == 0.7)
mean(l_7$bias)
mean(l_7$rmse)

l_9 <- df_long %>%
  filter(true_l == 0.9)
mean(l_9$bias)
mean(l_9$rmse)

### Mean bias and RMSE by value of winter harvest
tags05 <- df_long %>%
  filter(winter_h == 0.05, parameter == "l")
mean(tags05$bias)
mean(tags05$rmse)

tags1 <- df_long %>%
  filter(winter_h == 0.10, parameter == "l")
mean(tags1$bias)
mean(tags1$rmse)

tags25 <- df_long %>%
  filter(winter_h == 0.25, parameter == "l")
mean(tags25$bias)
mean(tags25$rmse)

# check Rhat values -------------------------------------------------------
# Loop through all files:
for (i in 1:length(files)){
  mcmc <- MCMCsummary(files$fit[[1]], params = c("mean.h", "mean.s", "mean.l"))
  rhat <- mcmc$Rhat
  check <- all(rhat < 1.1)
}
filter(mcmc, Rhat > 1.1)

####
# Plot parameter RMSE by value of s
plot_h_s<- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

plot_s_s <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

plot_l_s <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(plot_h_s, plot_s_s, plot_l_s, top = textGrob("2-tag model with winter harvest: \n RMSE of parameter estimates by survival rate"))
###
# Plot by value of h
plot_h_h <- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

plot_s_h <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

plot_l_h <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(plot_h_h, plot_s_h, plot_l_h, top = textGrob("2-tag model with winter harvest: \n RMSE of parameter estimates by harvest rate"))

####
# Plot by value of l
plot_h_l <- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

plot_s_l <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

plot_l_l <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value)))+
  geom_point(size=1) +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("RMSE")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(plot_h_l, plot_s_l, plot_l_l, top = textGrob("2-tag model with winter harvest: \n RMSE of parameter estimates by tag-reporting rate"))


### BIAS parameter-by-parameter
bias_h_s<- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=0.7) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

bias_s_s <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=0.7) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

bias_l_s <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=0.7) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(bias_h_s, bias_s_s, bias_l_s, top = textGrob("2-tag model with winter harvest: \n Bias in parameter estimates by survival rate"))

# plot by value of h
bias_h_h <- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

bias_s_h <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

bias_l_h <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(bias_h_h, bias_s_h, bias_l_h, top = textGrob("2-tag model with winter harvest: \n Bias in parameter estimates by harvest rate"))

####
# plot by value of l
bias_h_l <- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

bias_s_l <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

bias_l_l <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_l, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(bias_h_l, bias_s_l, bias_l_l, top = textGrob("2-tag model with winter harvest: \n Bias in parameter estimates by reporting rate"))

# Facet by value of w
bias_h_w <- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Harvest rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+ xlab("")+
  scale_colour_manual(values = c("0.1" = "#6A51A3", "0.2" = "#4A1486", "0.05" = "#9E9AC8"))+
  facet_wrap(~winter_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True harvest rate")

bias_s_w <- df_long %>%
  filter(parameter=="s") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Survival rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias") + xlab("")+
  scale_colour_manual(values = c("0.8" = "#6A51A3", "0.9" = "#4A1486", "0.5" = "#9E9AC8"))+
  facet_wrap(~winter_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True survival rate")

bias_l_w <- df_long %>%
  filter(parameter=="l") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~winter_h, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(bias_h_w, bias_s_w, bias_l_w, top = textGrob("2-tag model with winter harvest: \n Bias in parameter estimates by rate of winter harvest"))


############
# Table of error measures FOR EACH MODEL and parameter estimated ----------
##
vals <- c(0.05798921, 0.002402102, 0.004, 0.006, 0.011, 0.08915917, 0.01170922, 0.012, 0.013, 0.016, 
          -0.009808374, -0.01752499, -0.007, 0.002, 0.026, 0.09974848, 0.06590868, 0.063, 0.062, 0.064,
          -0.1912275, -0.01196104, -0.013, -0.013, -0.012, 0.245341, 0.0793083, 0.078, 0.079, 0.076)
dim(vals) <- c(5,6)

vals
# add names to columns and rows:
colnames(vals) <- c("bias (h)", "RMSE (h)", "bias (s)", "RMSE (s)", 
                    "bias (l)", "RMSE (l)")
rownames(vals) <- c("1-tag", "2-tag", "2-tag with w = 0.05h", 
                    "2-tag with w = 0.10h",
                    "2-tag with w = 0.25h")
# now make it look nice:
df_error <- as.data.frame(vals)

gt_error <- gt(df_error, rownames_to_stub = T) |>
  tab_stubhead(label = "Model") |>
  tab_header(
    title = "Mean error in each estimated parameter") |>
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left"),
      style = "solid",
      color = "lightgrey"),
    locations = cells_body()) |>
  fmt_number(decimals = 3)

gt_error


# Plot difference between observed and actual -----------------------------
# scenarios <- unique(df_long$scn)
# 
# for (scn in scenarios) {
#   plot_data3 <- df_long %>% filter(scn == !!scn)
#   q <- ggplot(plot_data3, aes(x = parameter, y = (as.numeric(true_value) - as.numeric(estimate)), fill = parameter))+
#     geom_hline(yintercept=0) +
#     scale_fill_brewer(palette = "GnBu") +
#     geom_boxplot() + 
#     theme_minimal() +
#     scale_y_continuous(breaks=seq(-0.50,0.50,0.05)) +
#     labs(title = str_wrap(paste("Difference between estimated and true parameter value for Scenario", scn)),
#          x = "Parameter",
#          y = "True value - Estimate")
#   ggsave(filename = paste0("diff_Scn_", scn, ".png"), plot = q, path = here::here("plots/winter-harvest/"))
# }


# # assign true values ------------------------------------------------------
# 
# df_tru3 <- df3 %>%
#   mutate(h_true = as.numeric(case_when(scn < 10 ~ 0.05, scn < 19 ~ 0.10, scn < 28 ~ 0.20, scn < 37 ~ 0.05, scn < 46 ~ 0.10,
#                                        scn < 55 ~ 0.20, scn < 64 ~ 0.05, scn < 73 ~ 0.10, scn < 82 ~ 0.20, scn < 91 ~ 0.05,
#                                        scn < 100 ~ 0.10, scn < 109 ~ 0.20, scn < 118 ~ 0.05, scn < 127 ~ 0.10, scn < 136 ~ 0.20,
#                                        scn < 145 ~ 0.05, scn < 154 ~ 0.10, scn < 163 ~ 0.20, scn < 172 ~ 0.05, scn < 181 ~ 0.10,
#                                        scn < 190 ~ 0.20, scn < 199 ~ 0.05, scn < 208 ~ 0.10, scn < 217 ~ 0.20, scn < 226 ~ 0.05,
#                                        scn < 235 ~ 0.10, scn < 244 ~ 0.20,
#                                        scn < 253 ~ 0.05, scn < 262 ~ 0.10, scn < 271 ~ 0.20, scn < 280 ~ 0.05, scn < 289 ~ 0.10,
#                                        scn < 298 ~ 0.20, scn < 307 ~ 0.05, scn < 316 ~ 0.10, scn < 325 ~ 0.20, scn < 334 ~ 0.05,
#                                        scn < 343 ~ 0.10, scn < 352 ~ 0.20, scn < 361 ~ 0.05, scn < 370 ~ 0.10, scn < 379 ~ 0.20,
#                                        scn < 388 ~ 0.05, scn < 397 ~ 0.10, scn < 406 ~ 0.20, scn < 415 ~ 0.05, scn < 424 ~ 0.10,
#                                        scn < 433 ~ 0.20, scn < 442 ~ 0.05, scn < 451 ~ 0.10, scn < 460 ~ 0.20, scn < 469 ~ 0.05,
#                                        scn < 478 ~ 0.10, scn < 487 ~ 0.20,
#                                        scn < 496 ~ 0.05, scn < 505 ~ 0.10, scn < 514 ~ 0.20, scn < 523 ~ 0.05, scn < 532 ~ 0.10,
#                                        scn < 541 ~ 0.20, scn < 550 ~ 0.05, scn < 559 ~ 0.10, scn < 568 ~ 0.20, scn < 577 ~ 0.05,
#                                        scn < 586 ~ 0.10, scn < 595 ~ 0.20, scn < 604 ~ 0.05, scn < 613 ~ 0.10, scn < 622 ~ 0.20,
#                                        scn < 631 ~ 0.05, scn < 640 ~ 0.10, scn < 649 ~ 0.20, scn < 658 ~ 0.05, scn < 667 ~ 0.10,
#                                        scn < 676 ~ 0.20, scn < 685 ~ 0.05, scn < 694 ~ 0.10, scn < 703 ~ 0.20, scn < 712 ~ 0.05,
#                                        scn < 721 ~ 0.10, scn < 730 ~ 0.20)),
#          s_true = as.numeric(case_when(scn < 4 ~ 0.5, scn < 7 ~ 0.8, scn < 10 ~ 0.9, scn < 13 ~ 0.5, scn < 16 ~ 0.8, scn < 19 ~ 0.9,
#                                        scn < 22 ~ 0.5, scn < 25 ~ 0.8, scn < 28 ~ 0.9, scn < 31 ~ 0.5, scn < 34 ~ 0.8, scn < 37 ~ 0.9,
#                                        scn < 40 ~ 0.5, scn < 43 ~ 0.8, scn < 46 ~ 0.9, scn < 49 ~ 0.5, scn < 52 ~ 0.8, scn < 55 ~ 0.9,
#                                        scn < 58 ~ 0.5, scn < 61 ~ 0.8, scn < 64 ~ 0.9, scn < 67 ~ 0.5, scn < 70 ~ 0.8, scn < 73 ~ 0.9,
#                                        scn < 76 ~ 0.5, scn < 79 ~ 0.8, scn < 82 ~ 0.9, scn < 85 ~ 0.5, scn < 88 ~ 0.8, scn < 91 ~ 0.9,
#                                        scn < 94 ~ 0.5, scn < 97 ~ 0.8, scn < 100 ~ 0.9, scn < 103 ~ 0.5, scn < 106 ~ 0.8, scn < 109 ~ 0.9,
#                                        scn < 112 ~ 0.5, scn < 115 ~ 0.8, scn < 118 ~ 0.9, scn < 121 ~ 0.5, scn < 124 ~ 0.8, scn < 127 ~ 0.9,
#                                        scn < 130 ~ 0.5, scn < 133 ~ 0.8, scn < 136 ~ 0.9, scn < 139 ~ 0.5, scn < 142 ~ 0.8, scn < 145 ~ 0.9,
#                                        scn < 148 ~ 0.5, scn < 151 ~ 0.8, scn < 154 ~ 0.9, scn < 157 ~ 0.5, scn < 160 ~ 0.8, scn < 163 ~ 0.9,
#                                        scn < 166 ~ 0.5, scn < 169 ~ 0.8, scn < 172 ~ 0.9, scn < 175 ~ 0.5, scn < 178 ~ 0.8, scn < 181 ~ 0.9,
#                                        scn < 184 ~ 0.5, scn < 187 ~ 0.8, scn < 190 ~ 0.9, scn < 193 ~ 0.5, scn < 196 ~ 0.8, scn < 199 ~ 0.9,
#                                        scn < 202 ~ 0.5, scn < 205 ~ 0.8, scn < 208 ~ 0.9, scn < 211 ~ 0.5, scn < 214 ~ 0.8, scn < 217 ~ 0.9,
#                                        scn < 220 ~ 0.5, scn < 223 ~ 0.8, scn < 226 ~ 0.9, scn < 229 ~ 0.5, scn < 232 ~ 0.8, scn < 235 ~ 0.9,
#                                        scn < 238 ~ 0.5, scn < 241 ~ 0.8, scn < 244 ~ 0.9,
#                                        scn < 247 ~ 0.5, scn < 250 ~ 0.8, scn < 253 ~ 0.9, scn < 256 ~ 0.5, scn < 259 ~ 0.8, scn < 262 ~ 0.9,
#                                        scn < 265 ~ 0.5, scn < 268 ~ 0.8, scn < 271 ~ 0.9, scn < 274 ~ 0.5, scn < 277 ~ 0.8, scn < 280 ~ 0.9,
#                                        scn < 283 ~ 0.5, scn < 286 ~ 0.8, scn < 289 ~ 0.9, scn < 292 ~ 0.5, scn < 295 ~ 0.8, scn < 298 ~ 0.9,
#                                        scn < 301 ~ 0.5, scn < 304 ~ 0.8, scn < 307 ~ 0.9, scn < 310 ~ 0.5, scn < 313 ~ 0.8, scn < 316 ~ 0.9,
#                                        scn < 319 ~ 0.5, scn < 322 ~ 0.8, scn < 325 ~ 0.9, scn < 328 ~ 0.5, scn < 331 ~ 0.8, scn < 334 ~ 0.9,
#                                        scn < 337 ~ 0.5, scn < 340 ~ 0.8, scn < 343 ~ 0.9, scn < 346 ~ 0.5, scn < 349 ~ 0.8, scn < 352 ~ 0.9,
#                                        scn < 355 ~ 0.5, scn < 358 ~ 0.8, scn < 361 ~ 0.9, scn < 364 ~ 0.5, scn < 367 ~ 0.8, scn < 370 ~ 0.9,
#                                        scn < 373 ~ 0.5, scn < 376 ~ 0.8, scn < 379 ~ 0.9, scn < 382 ~ 0.5, scn < 385 ~ 0.8, scn < 388 ~ 0.9,
#                                        scn < 391 ~ 0.5, scn < 394 ~ 0.8, scn < 397 ~ 0.9, scn < 400 ~ 0.5, scn < 403 ~ 0.8, scn < 406 ~ 0.9,
#                                        scn < 409 ~ 0.5, scn < 412 ~ 0.8, scn < 415 ~ 0.9, scn < 418 ~ 0.5, scn < 421 ~ 0.8, scn < 424 ~ 0.9,
#                                        scn < 427 ~ 0.5, scn < 430 ~ 0.8, scn < 433 ~ 0.9, scn < 436 ~ 0.5, scn < 439 ~ 0.8, scn < 442 ~ 0.9,
#                                        scn < 445 ~ 0.5, scn < 448 ~ 0.8, scn < 451 ~ 0.9, scn < 454 ~ 0.5, scn < 457 ~ 0.8, scn < 460 ~ 0.9,
#                                        scn < 463 ~ 0.5, scn < 466 ~ 0.8, scn < 469 ~ 0.9, scn < 472 ~ 0.5, scn < 475 ~ 0.8, scn < 478 ~ 0.9,
#                                        scn < 481 ~ 0.5, scn < 484 ~ 0.8, scn < 487 ~ 0.9,
#                                        scn < 490 ~ 0.5, scn < 493 ~ 0.8, scn < 496 ~ 0.9, scn < 499 ~ 0.5, scn < 502 ~ 0.8, scn < 505 ~ 0.9,
#                                        scn < 508 ~ 0.5, scn < 511 ~ 0.8, scn < 514 ~ 0.9, scn < 517 ~ 0.5, scn < 520 ~ 0.8, scn < 523 ~ 0.9,
#                                        scn < 526 ~ 0.5, scn < 529 ~ 0.8, scn < 532 ~ 0.9, scn < 535 ~ 0.5, scn < 538 ~ 0.8, scn < 541 ~ 0.9,
#                                        scn < 544 ~ 0.5, scn < 547 ~ 0.8, scn < 550 ~ 0.9, scn < 553 ~ 0.5, scn < 556 ~ 0.8, scn < 559 ~ 0.9,
#                                        scn < 562 ~ 0.5, scn < 565 ~ 0.8, scn < 568 ~ 0.9, scn < 571 ~ 0.5, scn < 574 ~ 0.8, scn < 577 ~ 0.9,
#                                        scn < 580 ~ 0.5, scn < 583 ~ 0.8, scn < 586 ~ 0.9, scn < 589 ~ 0.5, scn < 592 ~ 0.8, scn < 595 ~ 0.9,
#                                        scn < 598 ~ 0.5, scn < 601 ~ 0.8, scn < 604 ~ 0.9, scn < 607 ~ 0.5, scn < 610 ~ 0.8, scn < 613 ~ 0.9,
#                                        scn < 616 ~ 0.5, scn < 619 ~ 0.8, scn < 622 ~ 0.9, scn < 625 ~ 0.5, scn < 628 ~ 0.8, scn < 631 ~ 0.9,
#                                        scn < 634 ~ 0.5, scn < 637 ~ 0.8, scn < 640 ~ 0.9, scn < 643 ~ 0.5, scn < 646 ~ 0.8, scn < 649 ~ 0.9,
#                                        scn < 652 ~ 0.5, scn < 655 ~ 0.8, scn < 658 ~ 0.9, scn < 661 ~ 0.5, scn < 664 ~ 0.8, scn < 667 ~ 0.9,
#                                        scn < 670 ~ 0.5, scn < 673 ~ 0.8, scn < 676 ~ 0.9, scn < 679 ~ 0.5, scn < 682 ~ 0.8, scn < 685 ~ 0.9,
#                                        scn < 688 ~ 0.5, scn < 691 ~ 0.8, scn < 694 ~ 0.9, scn < 697 ~ 0.5, scn < 700 ~ 0.8, scn < 703 ~ 0.9,
#                                        scn < 706 ~ 0.5, scn < 709 ~ 0.8, scn < 712 ~ 0.9, scn < 715 ~ 0.5, scn < 718 ~ 0.8, scn < 721 ~ 0.9,
#                                        scn < 724 ~ 0.5, scn < 727 ~ 0.8, scn < 730 ~ 0.9)),
#          l_true = as.numeric(case_when(scn == 1 | scn == 4 | scn == 7 | scn == 10 | scn == 13 | scn == 16 | scn == 19 
#                                        | scn == 22 | scn == 25 | scn == 28 | scn == 31 | scn == 34 | scn == 37
#                                        | scn == 40 | scn == 43 | scn == 46 | scn == 49 | scn == 52 | scn == 55 | scn == 58 
#                                        | scn == 61 | scn == 64 | scn == 67 | scn == 70 | scn == 73 | scn == 76 | scn == 79 
#                                        | scn == 82 | scn == 85 | scn == 88 | scn == 91 | scn == 94 | scn == 97 | scn == 100 
#                                        | scn == 103 | scn == 106 | scn == 109 | scn == 112 | scn == 115 | scn == 118 | scn == 121
#                                        | scn == 124 | scn == 127 | scn == 130 | scn == 133 | scn == 136 | scn == 139 | scn == 142
#                                        | scn == 145 | scn == 148 | scn == 151 | scn == 154 | scn == 157 | scn == 160 | scn == 163
#                                        | scn == 166 | scn == 169 | scn == 172 | scn == 175 | scn == 178 | scn == 181 | scn == 184
#                                        | scn == 187 | scn == 190 | scn == 193 | scn == 196 | scn == 199 | scn == 202 | scn == 205
#                                        | scn == 208 | scn == 211 | scn == 214 | scn == 217 | scn == 220 | scn == 223 | scn == 226
#                                        | scn == 229 | scn == 232 | scn == 235 | scn == 238 | scn == 241 ~ 0.3,
#                                        scn == 2 | scn == 5 | scn == 8 | scn == 11 | scn == 14 | scn == 17 | scn == 20 | scn == 23 
#                                        | scn == 26 | scn == 29 | scn == 32 | scn == 35 | scn == 38 | scn == 41 | scn == 44 
#                                        | scn == 47 | scn == 50 | scn == 53 | scn == 56 | scn == 59 | scn == 62 | scn == 65 
#                                        | scn == 68 | scn == 71 | scn == 74 | scn == 77 | scn == 80 | scn == 83 | scn == 86 
#                                        | scn == 89 | scn == 92 | scn == 95 | scn == 98 | scn == 101 | scn == 104 | scn == 107
#                                        | scn == 110 | scn == 113 | scn == 116 | scn == 119 | scn == 122 | scn == 125 | scn == 128
#                                        | scn == 131 | scn == 134 | scn == 137 | scn == 140 | scn == 143 | scn == 146 | scn == 149
#                                        | scn == 152 | scn == 155 | scn == 158 | scn == 161 | scn == 164 | scn == 167 | scn == 170
#                                        | scn == 173 | scn == 176 | scn == 179 | scn == 182 | scn == 185 | scn == 188 | scn == 191
#                                        | scn == 194 | scn == 197 | scn == 200 | scn == 203 | scn == 206 | scn == 209 | scn == 212
#                                        | scn == 215 | scn == 218 | scn == 221 | scn == 224 | scn == 227 | scn == 230 | scn == 233
#                                        | scn == 236 | scn == 239 | scn == 242 ~ 0.7,
#                                        scn == 3 | scn == 6 | scn == 9 | scn == 12 | scn ==15 | scn == 18 | scn == 21 | scn == 24
#                                        | scn == 27 | scn == 30 | scn == 33 | scn == 36 | scn == 39 | scn == 42 | scn == 45 | scn == 48 
#                                        | scn == 51 | scn == 54 | scn == 57 | scn == 60 | scn == 63 | scn == 66 | scn == 69 | scn == 72 
#                                        | scn == 75 | scn == 78 | scn == 81 | scn == 84 | scn == 87 | scn == 90 | scn == 93 | scn == 96 
#                                        | scn == 99 | scn == 102 | scn == 105 | scn == 108 | scn == 111 | scn == 114 | scn == 117
#                                        | scn == 120 | scn == 123 | scn == 126 | scn == 129 | scn == 132 | scn == 135 | scn == 138
#                                        | scn == 141 | scn == 144 | scn == 147 | scn == 150 | scn == 153 | scn == 156 | scn == 159
#                                        | scn == 162 | scn == 165 | scn == 168 | scn == 171 | scn == 174 | scn == 177 | scn == 180
#                                        | scn == 183 | scn == 186 | scn == 189 | scn == 192 | scn == 195 | scn == 198 | scn == 201
#                                        | scn == 204 | scn == 207 | scn == 210 | scn == 213 | scn == 216 | scn == 219 | scn == 222
#                                        | scn == 225 | scn == 228 | scn == 231 | scn == 234 | scn == 237 | scn == 240 | scn == 243 ~ 0.9,
#                                        
#                                        scn == 244 | scn == 247 | scn == 250 | scn == 253 | scn == 256 | scn == 259 | scn == 262 
#                                        | scn == 265 | scn == 268 | scn == 271 | scn == 274 | scn == 277 | scn == 280
#                                        | scn == 283 | scn == 286 | scn == 289 | scn == 292 | scn == 295 | scn == 298 | scn == 301 
#                                        | scn == 304 | scn == 307 | scn == 310 | scn == 313 | scn == 316 | scn == 319 | scn == 322 
#                                        | scn == 325 | scn == 328 | scn == 331 | scn == 334 | scn == 337 | scn == 340 | scn == 343 
#                                        | scn == 346 | scn == 349 | scn == 352 | scn == 355 | scn == 358 | scn == 361 | scn == 364
#                                        | scn == 367 | scn == 370 | scn == 373 | scn == 376 | scn == 379 | scn == 382 | scn == 385
#                                        | scn == 388 | scn == 391 | scn == 394 | scn == 397 | scn == 400 | scn == 403 | scn == 406
#                                        | scn == 409 | scn == 412 | scn == 415 | scn == 418 | scn == 421 | scn == 424 | scn == 427
#                                        | scn == 430 | scn == 433 | scn == 436 | scn == 439 | scn == 442 | scn == 445 | scn == 448
#                                        | scn == 451 | scn == 454 | scn == 457 | scn == 460 | scn == 463 | scn == 466 | scn == 469
#                                        | scn == 472 | scn == 475 | scn == 478 | scn == 481 | scn == 484 ~ 0.3,
#                                        scn == 245 | scn == 248 | scn == 251 | scn == 254 | scn == 257 | scn == 260 | scn == 263 | scn == 266 
#                                        | scn == 269 | scn == 272 | scn == 275 | scn == 278 | scn == 281 | scn == 284 | scn == 287 
#                                        | scn == 290 | scn == 293 | scn == 296 | scn == 299 | scn == 302 | scn == 305 | scn == 308 
#                                        | scn == 311 | scn == 314 | scn == 317 | scn == 320 | scn == 323 | scn == 326 | scn == 329 
#                                        | scn == 332 | scn == 335 | scn == 338 | scn == 341 | scn == 344 | scn == 347 | scn == 350
#                                        | scn == 353 | scn == 356 | scn == 359 | scn == 362 | scn == 365 | scn == 368 | scn == 371
#                                        | scn == 374 | scn == 377 | scn == 380 | scn == 383 | scn == 386 | scn == 389 | scn == 392
#                                        | scn == 395 | scn == 398 | scn == 401 | scn == 404 | scn == 407 | scn == 410 | scn == 413
#                                        | scn == 416 | scn == 419 | scn == 422 | scn == 425 | scn == 428 | scn == 431 | scn == 434
#                                        | scn == 437 | scn == 440 | scn == 443 | scn == 446 | scn == 449 | scn == 452 | scn == 455
#                                        | scn == 458 | scn == 461 | scn == 464 | scn == 467 | scn == 470 | scn == 473 | scn == 476
#                                        | scn == 479 | scn == 482 | scn == 485 ~ 0.7,
#                                        scn == 246 | scn == 249 | scn == 252 | scn == 255 | scn == 258 | scn == 261 | scn == 264 | scn == 267
#                                        | scn == 270 | scn == 273 | scn == 276 | scn == 279 | scn == 282 | scn == 285 | scn == 288 | scn == 291 
#                                        | scn == 294 | scn == 297 | scn == 300 | scn == 303 | scn == 306 | scn == 309 | scn == 312 | scn == 315 
#                                        | scn == 318 | scn == 321 | scn == 324 | scn == 327 | scn == 330 | scn == 333 | scn == 336 | scn == 339 
#                                        | scn == 342 | scn == 345 | scn == 348 | scn == 351 | scn == 354 | scn == 357 | scn == 360
#                                        | scn == 363 | scn == 366 | scn == 369 | scn == 372 | scn == 375 | scn == 378 | scn == 381
#                                        | scn == 384 | scn == 387 | scn == 390 | scn == 393 | scn == 396 | scn == 399 | scn == 402
#                                        | scn == 405 | scn == 408 | scn == 411 | scn == 414 | scn == 417 | scn == 420 | scn == 423
#                                        | scn == 426 | scn == 429 | scn == 432 | scn == 435 | scn == 438 | scn == 441 | scn == 444
#                                        | scn == 447 | scn == 450 | scn == 453 | scn == 456 | scn == 459 | scn == 462 | scn == 465
#                                        | scn == 468 | scn == 471 | scn == 474 | scn == 477 | scn == 480 | scn == 483 | scn == 486 ~ 0.9,
#                                        
#                                        scn == 487 | scn == 490 | scn == 493 | scn == 496 | scn == 499 | scn == 502 | scn == 505 
#                                        | scn == 508 | scn == 511 | scn == 514 | scn == 517 | scn == 520 | scn == 523
#                                        | scn == 526 | scn == 529 | scn == 532 | scn == 535 | scn == 538 | scn == 541 | scn == 544 
#                                        | scn == 547 | scn == 550 | scn == 553 | scn == 556 | scn == 559 | scn == 562 | scn == 565 
#                                        | scn == 568 | scn == 571 | scn == 574 | scn == 577 | scn == 580 | scn == 583 | scn == 586 
#                                        | scn == 589 | scn == 592 | scn == 595 | scn == 598 | scn == 601 | scn == 604 | scn == 607
#                                        | scn == 610 | scn == 613 | scn == 616 | scn == 619 | scn == 622 | scn == 625 | scn == 628
#                                        | scn == 631 | scn == 634 | scn == 637 | scn == 640 | scn == 643 | scn == 646 | scn == 649
#                                        | scn == 652 | scn == 655 | scn == 658 | scn == 661 | scn == 664 | scn == 667 | scn == 670
#                                        | scn == 673 | scn == 676 | scn == 679 | scn == 682 | scn == 685 | scn == 688 | scn == 691
#                                        | scn == 694 | scn == 697 | scn == 700 | scn == 703 | scn == 706 | scn == 709 | scn == 712
#                                        | scn == 715 | scn == 718 | scn == 721 | scn == 724 | scn == 727 ~ 0.3,
#                                        scn == 488 | scn == 491 | scn == 494 | scn == 497 | scn == 500 | scn == 503 | scn == 506 | scn == 509 
#                                        | scn == 512 | scn == 515 | scn == 518 | scn == 521 | scn == 524 | scn == 527 | scn == 530 
#                                        | scn == 533 | scn == 536 | scn == 539 | scn == 542 | scn == 545 | scn == 548 | scn == 551 
#                                        | scn == 554 | scn == 557 | scn == 560 | scn == 563 | scn == 566 | scn == 569 | scn == 572 
#                                        | scn == 575 | scn == 578 | scn == 581 | scn == 584 | scn == 587 | scn == 590 | scn == 593
#                                        | scn == 596 | scn == 599 | scn == 602 | scn == 605 | scn == 608 | scn == 611 | scn == 614
#                                        | scn == 617 | scn == 620 | scn == 623 | scn == 626 | scn == 629 | scn == 632 | scn == 635
#                                        | scn == 638 | scn == 641 | scn == 644 | scn == 647 | scn == 650 | scn == 653 | scn == 656
#                                        | scn == 659 | scn == 662 | scn == 665 | scn == 668 | scn == 671 | scn == 674 | scn == 677
#                                        | scn == 680 | scn == 683 | scn == 686 | scn == 689 | scn == 692 | scn == 695 | scn == 698
#                                        | scn == 701 | scn == 704 | scn == 707 | scn == 710 | scn == 713 | scn == 716 | scn == 719
#                                        | scn == 722 | scn == 725 | scn == 728 ~ 0.7,
#                                        scn == 489 | scn == 492 | scn == 495 | scn == 498 | scn == 501 | scn == 504 | scn == 507 | scn == 510
#                                        | scn == 513 | scn == 516 | scn == 519 | scn == 522 | scn == 525 | scn == 528 | scn == 531 | scn == 534 
#                                        | scn == 537 | scn == 540 | scn == 543 | scn == 546 | scn == 549 | scn == 552 | scn == 555 | scn == 558 
#                                        | scn == 561 | scn == 564 | scn == 567 | scn == 570 | scn == 573 | scn == 576 | scn == 579 | scn == 582 
#                                        | scn == 585 | scn == 588 | scn == 591 | scn == 594 | scn == 597 | scn == 600 | scn == 603
#                                        | scn == 606 | scn == 609 | scn == 612 | scn == 615 | scn == 618 | scn == 621 | scn == 624
#                                        | scn == 627 | scn == 630 | scn == 633 | scn == 636 | scn == 639 | scn == 642 | scn == 645
#                                        | scn == 648 | scn == 651 | scn == 654 | scn == 657 | scn == 660 | scn == 663 | scn == 666
#                                        | scn == 669 | scn == 672 | scn == 675 | scn == 678 | scn == 681 | scn == 684 | scn == 687
#                                        | scn == 690 | scn == 693 | scn == 696 | scn == 699 | scn == 702 | scn == 705 | scn == 708
#                                        | scn == 711 | scn == 714 | scn == 717 | scn == 720 | scn == 723 | scn == 726 | scn == 729 ~ 0.9)),
#          yrs = as.numeric(case_when(scn < 82 ~ 2, scn < 163 ~ 5, scn < 244 ~ 10,
#                                     scn < 325 ~ 2, scn < 406 ~ 5, scn < 487 ~ 10,
#                                     scn < 568 ~ 2, scn < 649 ~ 5, scn < 730 ~ 10)),
#          ntags = as.numeric(case_when(scn < 28 ~ 50, scn < 55 ~ 100, scn < 82 ~ 200, scn < 109 ~ 50, scn < 136 ~ 100, scn < 163 ~ 200,
#                                       scn < 190 ~ 50, scn < 217 ~ 100, scn < 244 ~ 200,
#                                       scn < 271 ~ 50, scn < 298 ~ 100, scn < 325 ~ 200, scn < 352 ~ 50, scn < 379 ~ 100, scn < 406 ~ 200,
#                                       scn < 433 ~ 50, scn < 460 ~ 100, scn < 487 ~ 200,
#                                       scn < 514 ~ 50, scn < 541 ~ 100, scn < 568 ~ 200, scn < 595 ~ 50, scn < 622 ~ 100, scn < 649 ~ 200,
#                                       scn < 676 ~ 50, scn < 703 ~ 100, scn < 730 ~ 200)),
#          w = as.numeric(case_when(scn < 244 ~ 0.05, scn < 487 ~ 0.10, scn < 730 ~ 0.25)))
# 
# # Re-format data
# df_long <- df_tru3 %>%
#   pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
#   pivot_longer(cols = c(h_true, s_true, l_true), names_to = "true_parameter", values_to = "true_value") %>%
#   filter(paste0(parameter, "_true") == true_parameter) %>%
#   select(-true_parameter)
# 
# df_long <- df_long %>%
#   group_by(scn, parameter) %>%
#   mutate(rmse = sqrt(mean((as.numeric(estimate) - as.numeric(true_value)^2), na.rm = TRUE)),
#          bias = bias(as.numeric(true_value), as.numeric(estimate)),
#          h_true = as.numeric(case_when(scn < 10 ~ 0.05, scn < 19 ~ 0.10, scn < 28 ~ 0.20, scn < 37 ~ 0.05, scn < 46 ~ 0.10,
#                                        scn < 55 ~ 0.20, scn < 64 ~ 0.05, scn < 73 ~ 0.10, scn < 82 ~ 0.20, scn < 91 ~ 0.05,
#                                        scn < 100 ~ 0.10, scn < 109 ~ 0.20, scn < 118 ~ 0.05, scn < 127 ~ 0.10, scn < 136 ~ 0.20,
#                                        scn < 145 ~ 0.05, scn < 154 ~ 0.10, scn < 163 ~ 0.20, scn < 172 ~ 0.05, scn < 181 ~ 0.10,
#                                        scn < 190 ~ 0.20, scn < 199 ~ 0.05, scn < 208 ~ 0.10, scn < 217 ~ 0.20, scn < 226 ~ 0.05,
#                                        scn < 235 ~ 0.10, scn < 244 ~ 0.20,
#                                        scn < 253 ~ 0.05, scn < 262 ~ 0.10, scn < 271 ~ 0.20, scn < 280 ~ 0.05, scn < 289 ~ 0.10,
#                                        scn < 298 ~ 0.20, scn < 307 ~ 0.05, scn < 316 ~ 0.10, scn < 325 ~ 0.20, scn < 334 ~ 0.05,
#                                        scn < 343 ~ 0.10, scn < 352 ~ 0.20, scn < 361 ~ 0.05, scn < 370 ~ 0.10, scn < 379 ~ 0.20,
#                                        scn < 388 ~ 0.05, scn < 397 ~ 0.10, scn < 406 ~ 0.20, scn < 415 ~ 0.05, scn < 424 ~ 0.10,
#                                        scn < 433 ~ 0.20, scn < 442 ~ 0.05, scn < 451 ~ 0.10, scn < 460 ~ 0.20, scn < 469 ~ 0.05,
#                                        scn < 478 ~ 0.10, scn < 487 ~ 0.20,
#                                        scn < 496 ~ 0.05, scn < 505 ~ 0.10, scn < 514 ~ 0.20, scn < 523 ~ 0.05, scn < 532 ~ 0.10,
#                                        scn < 541 ~ 0.20, scn < 550 ~ 0.05, scn < 559 ~ 0.10, scn < 568 ~ 0.20, scn < 577 ~ 0.05,
#                                        scn < 586 ~ 0.10, scn < 595 ~ 0.20, scn < 604 ~ 0.05, scn < 613 ~ 0.10, scn < 622 ~ 0.20,
#                                        scn < 631 ~ 0.05, scn < 640 ~ 0.10, scn < 649 ~ 0.20, scn < 658 ~ 0.05, scn < 667 ~ 0.10,
#                                        scn < 676 ~ 0.20, scn < 685 ~ 0.05, scn < 694 ~ 0.10, scn < 703 ~ 0.20, scn < 712 ~ 0.05,
#                                        scn < 721 ~ 0.10, scn < 730 ~ 0.20)),
#          s_true = as.numeric(case_when(scn < 4 ~ 0.5, scn < 7 ~ 0.8, scn < 10 ~ 0.9, scn < 13 ~ 0.5, scn < 16 ~ 0.8, scn < 19 ~ 0.9,
#                                        scn < 22 ~ 0.5, scn < 25 ~ 0.8, scn < 28 ~ 0.9, scn < 31 ~ 0.5, scn < 34 ~ 0.8, scn < 37 ~ 0.9,
#                                        scn < 40 ~ 0.5, scn < 43 ~ 0.8, scn < 46 ~ 0.9, scn < 49 ~ 0.5, scn < 52 ~ 0.8, scn < 55 ~ 0.9,
#                                        scn < 58 ~ 0.5, scn < 61 ~ 0.8, scn < 64 ~ 0.9, scn < 67 ~ 0.5, scn < 70 ~ 0.8, scn < 73 ~ 0.9,
#                                        scn < 76 ~ 0.5, scn < 79 ~ 0.8, scn < 82 ~ 0.9, scn < 85 ~ 0.5, scn < 88 ~ 0.8, scn < 91 ~ 0.9,
#                                        scn < 94 ~ 0.5, scn < 97 ~ 0.8, scn < 100 ~ 0.9, scn < 103 ~ 0.5, scn < 106 ~ 0.8, scn < 109 ~ 0.9,
#                                        scn < 112 ~ 0.5, scn < 115 ~ 0.8, scn < 118 ~ 0.9, scn < 121 ~ 0.5, scn < 124 ~ 0.8, scn < 127 ~ 0.9,
#                                        scn < 130 ~ 0.5, scn < 133 ~ 0.8, scn < 136 ~ 0.9, scn < 139 ~ 0.5, scn < 142 ~ 0.8, scn < 145 ~ 0.9,
#                                        scn < 148 ~ 0.5, scn < 151 ~ 0.8, scn < 154 ~ 0.9, scn < 157 ~ 0.5, scn < 160 ~ 0.8, scn < 163 ~ 0.9,
#                                        scn < 166 ~ 0.5, scn < 169 ~ 0.8, scn < 172 ~ 0.9, scn < 175 ~ 0.5, scn < 178 ~ 0.8, scn < 181 ~ 0.9,
#                                        scn < 184 ~ 0.5, scn < 187 ~ 0.8, scn < 190 ~ 0.9, scn < 193 ~ 0.5, scn < 196 ~ 0.8, scn < 199 ~ 0.9,
#                                        scn < 202 ~ 0.5, scn < 205 ~ 0.8, scn < 208 ~ 0.9, scn < 211 ~ 0.5, scn < 214 ~ 0.8, scn < 217 ~ 0.9,
#                                        scn < 220 ~ 0.5, scn < 223 ~ 0.8, scn < 226 ~ 0.9, scn < 229 ~ 0.5, scn < 232 ~ 0.8, scn < 235 ~ 0.9,
#                                        scn < 238 ~ 0.5, scn < 241 ~ 0.8, scn < 244 ~ 0.9,
#                                        scn < 247 ~ 0.5, scn < 250 ~ 0.8, scn < 253 ~ 0.9, scn < 256 ~ 0.5, scn < 259 ~ 0.8, scn < 262 ~ 0.9,
#                                        scn < 265 ~ 0.5, scn < 268 ~ 0.8, scn < 271 ~ 0.9, scn < 274 ~ 0.5, scn < 277 ~ 0.8, scn < 280 ~ 0.9,
#                                        scn < 283 ~ 0.5, scn < 286 ~ 0.8, scn < 289 ~ 0.9, scn < 292 ~ 0.5, scn < 295 ~ 0.8, scn < 298 ~ 0.9,
#                                        scn < 301 ~ 0.5, scn < 304 ~ 0.8, scn < 307 ~ 0.9, scn < 310 ~ 0.5, scn < 313 ~ 0.8, scn < 316 ~ 0.9,
#                                        scn < 319 ~ 0.5, scn < 322 ~ 0.8, scn < 325 ~ 0.9, scn < 328 ~ 0.5, scn < 331 ~ 0.8, scn < 334 ~ 0.9,
#                                        scn < 337 ~ 0.5, scn < 340 ~ 0.8, scn < 343 ~ 0.9, scn < 346 ~ 0.5, scn < 349 ~ 0.8, scn < 352 ~ 0.9,
#                                        scn < 355 ~ 0.5, scn < 358 ~ 0.8, scn < 361 ~ 0.9, scn < 364 ~ 0.5, scn < 367 ~ 0.8, scn < 370 ~ 0.9,
#                                        scn < 373 ~ 0.5, scn < 376 ~ 0.8, scn < 379 ~ 0.9, scn < 382 ~ 0.5, scn < 385 ~ 0.8, scn < 388 ~ 0.9,
#                                        scn < 391 ~ 0.5, scn < 394 ~ 0.8, scn < 397 ~ 0.9, scn < 400 ~ 0.5, scn < 403 ~ 0.8, scn < 406 ~ 0.9,
#                                        scn < 409 ~ 0.5, scn < 412 ~ 0.8, scn < 415 ~ 0.9, scn < 418 ~ 0.5, scn < 421 ~ 0.8, scn < 424 ~ 0.9,
#                                        scn < 427 ~ 0.5, scn < 430 ~ 0.8, scn < 433 ~ 0.9, scn < 436 ~ 0.5, scn < 439 ~ 0.8, scn < 442 ~ 0.9,
#                                        scn < 445 ~ 0.5, scn < 448 ~ 0.8, scn < 451 ~ 0.9, scn < 454 ~ 0.5, scn < 457 ~ 0.8, scn < 460 ~ 0.9,
#                                        scn < 463 ~ 0.5, scn < 466 ~ 0.8, scn < 469 ~ 0.9, scn < 472 ~ 0.5, scn < 475 ~ 0.8, scn < 478 ~ 0.9,
#                                        scn < 481 ~ 0.5, scn < 484 ~ 0.8, scn < 487 ~ 0.9,
#                                        scn < 490 ~ 0.5, scn < 493 ~ 0.8, scn < 496 ~ 0.9, scn < 499 ~ 0.5, scn < 502 ~ 0.8, scn < 505 ~ 0.9,
#                                        scn < 508 ~ 0.5, scn < 511 ~ 0.8, scn < 514 ~ 0.9, scn < 517 ~ 0.5, scn < 520 ~ 0.8, scn < 523 ~ 0.9,
#                                        scn < 526 ~ 0.5, scn < 529 ~ 0.8, scn < 532 ~ 0.9, scn < 535 ~ 0.5, scn < 538 ~ 0.8, scn < 541 ~ 0.9,
#                                        scn < 544 ~ 0.5, scn < 547 ~ 0.8, scn < 550 ~ 0.9, scn < 553 ~ 0.5, scn < 556 ~ 0.8, scn < 559 ~ 0.9,
#                                        scn < 562 ~ 0.5, scn < 565 ~ 0.8, scn < 568 ~ 0.9, scn < 571 ~ 0.5, scn < 574 ~ 0.8, scn < 577 ~ 0.9,
#                                        scn < 580 ~ 0.5, scn < 583 ~ 0.8, scn < 586 ~ 0.9, scn < 589 ~ 0.5, scn < 592 ~ 0.8, scn < 595 ~ 0.9,
#                                        scn < 598 ~ 0.5, scn < 601 ~ 0.8, scn < 604 ~ 0.9, scn < 607 ~ 0.5, scn < 610 ~ 0.8, scn < 613 ~ 0.9,
#                                        scn < 616 ~ 0.5, scn < 619 ~ 0.8, scn < 622 ~ 0.9, scn < 625 ~ 0.5, scn < 628 ~ 0.8, scn < 631 ~ 0.9,
#                                        scn < 634 ~ 0.5, scn < 637 ~ 0.8, scn < 640 ~ 0.9, scn < 643 ~ 0.5, scn < 646 ~ 0.8, scn < 649 ~ 0.9,
#                                        scn < 652 ~ 0.5, scn < 655 ~ 0.8, scn < 658 ~ 0.9, scn < 661 ~ 0.5, scn < 664 ~ 0.8, scn < 667 ~ 0.9,
#                                        scn < 670 ~ 0.5, scn < 673 ~ 0.8, scn < 676 ~ 0.9, scn < 679 ~ 0.5, scn < 682 ~ 0.8, scn < 685 ~ 0.9,
#                                        scn < 688 ~ 0.5, scn < 691 ~ 0.8, scn < 694 ~ 0.9, scn < 697 ~ 0.5, scn < 700 ~ 0.8, scn < 703 ~ 0.9,
#                                        scn < 706 ~ 0.5, scn < 709 ~ 0.8, scn < 712 ~ 0.9, scn < 715 ~ 0.5, scn < 718 ~ 0.8, scn < 721 ~ 0.9,
#                                        scn < 724 ~ 0.5, scn < 727 ~ 0.8, scn < 730 ~ 0.9)),
#          l_true = as.numeric(case_when(scn == 1 | scn == 4 | scn == 7 | scn == 10 | scn == 13 | scn == 16 | scn == 19 
#                                        | scn == 22 | scn == 25 | scn == 28 | scn == 31 | scn == 34 | scn == 37
#                                        | scn == 40 | scn == 43 | scn == 46 | scn == 49 | scn == 52 | scn == 55 | scn == 58 
#                                        | scn == 61 | scn == 64 | scn == 67 | scn == 70 | scn == 73 | scn == 76 | scn == 79 
#                                        | scn == 82 | scn == 85 | scn == 88 | scn == 91 | scn == 94 | scn == 97 | scn == 100 
#                                        | scn == 103 | scn == 106 | scn == 109 | scn == 112 | scn == 115 | scn == 118 | scn == 121
#                                        | scn == 124 | scn == 127 | scn == 130 | scn == 133 | scn == 136 | scn == 139 | scn == 142
#                                        | scn == 145 | scn == 148 | scn == 151 | scn == 154 | scn == 157 | scn == 160 | scn == 163
#                                        | scn == 166 | scn == 169 | scn == 172 | scn == 175 | scn == 178 | scn == 181 | scn == 184
#                                        | scn == 187 | scn == 190 | scn == 193 | scn == 196 | scn == 199 | scn == 202 | scn == 205
#                                        | scn == 208 | scn == 211 | scn == 214 | scn == 217 | scn == 220 | scn == 223 | scn == 226
#                                        | scn == 229 | scn == 232 | scn == 235 | scn == 238 | scn == 241 ~ 0.3,
#                                        scn == 2 | scn == 5 | scn == 8 | scn == 11 | scn == 14 | scn == 17 | scn == 20 | scn == 23 
#                                        | scn == 26 | scn == 29 | scn == 32 | scn == 35 | scn == 38 | scn == 41 | scn == 44 
#                                        | scn == 47 | scn == 50 | scn == 53 | scn == 56 | scn == 59 | scn == 62 | scn == 65 
#                                        | scn == 68 | scn == 71 | scn == 74 | scn == 77 | scn == 80 | scn == 83 | scn == 86 
#                                        | scn == 89 | scn == 92 | scn == 95 | scn == 98 | scn == 101 | scn == 104 | scn == 107
#                                        | scn == 110 | scn == 113 | scn == 116 | scn == 119 | scn == 122 | scn == 125 | scn == 128
#                                        | scn == 131 | scn == 134 | scn == 137 | scn == 140 | scn == 143 | scn == 146 | scn == 149
#                                        | scn == 152 | scn == 155 | scn == 158 | scn == 161 | scn == 164 | scn == 167 | scn == 170
#                                        | scn == 173 | scn == 176 | scn == 179 | scn == 182 | scn == 185 | scn == 188 | scn == 191
#                                        | scn == 194 | scn == 197 | scn == 200 | scn == 203 | scn == 206 | scn == 209 | scn == 212
#                                        | scn == 215 | scn == 218 | scn == 221 | scn == 224 | scn == 227 | scn == 230 | scn == 233
#                                        | scn == 236 | scn == 239 | scn == 242 ~ 0.7,
#                                        scn == 3 | scn == 6 | scn == 9 | scn == 12 | scn ==15 | scn == 18 | scn == 21 | scn == 24
#                                        | scn == 27 | scn == 30 | scn == 33 | scn == 36 | scn == 39 | scn == 42 | scn == 45 | scn == 48 
#                                        | scn == 51 | scn == 54 | scn == 57 | scn == 60 | scn == 63 | scn == 66 | scn == 69 | scn == 72 
#                                        | scn == 75 | scn == 78 | scn == 81 | scn == 84 | scn == 87 | scn == 90 | scn == 93 | scn == 96 
#                                        | scn == 99 | scn == 102 | scn == 105 | scn == 108 | scn == 111 | scn == 114 | scn == 117
#                                        | scn == 120 | scn == 123 | scn == 126 | scn == 129 | scn == 132 | scn == 135 | scn == 138
#                                        | scn == 141 | scn == 144 | scn == 147 | scn == 150 | scn == 153 | scn == 156 | scn == 159
#                                        | scn == 162 | scn == 165 | scn == 168 | scn == 171 | scn == 174 | scn == 177 | scn == 180
#                                        | scn == 183 | scn == 186 | scn == 189 | scn == 192 | scn == 195 | scn == 198 | scn == 201
#                                        | scn == 204 | scn == 207 | scn == 210 | scn == 213 | scn == 216 | scn == 219 | scn == 222
#                                        | scn == 225 | scn == 228 | scn == 231 | scn == 234 | scn == 237 | scn == 240 | scn == 243 ~ 0.9,
#                                        
#                                        scn == 244 | scn == 247 | scn == 250 | scn == 253 | scn == 256 | scn == 259 | scn == 262 
#                                        | scn == 265 | scn == 268 | scn == 271 | scn == 274 | scn == 277 | scn == 280
#                                        | scn == 283 | scn == 286 | scn == 289 | scn == 292 | scn == 295 | scn == 298 | scn == 301 
#                                        | scn == 304 | scn == 307 | scn == 310 | scn == 313 | scn == 316 | scn == 319 | scn == 322 
#                                        | scn == 325 | scn == 328 | scn == 331 | scn == 334 | scn == 337 | scn == 340 | scn == 343 
#                                        | scn == 346 | scn == 349 | scn == 352 | scn == 355 | scn == 358 | scn == 361 | scn == 364
#                                        | scn == 367 | scn == 370 | scn == 373 | scn == 376 | scn == 379 | scn == 382 | scn == 385
#                                        | scn == 388 | scn == 391 | scn == 394 | scn == 397 | scn == 400 | scn == 403 | scn == 406
#                                        | scn == 409 | scn == 412 | scn == 415 | scn == 418 | scn == 421 | scn == 424 | scn == 427
#                                        | scn == 430 | scn == 433 | scn == 436 | scn == 439 | scn == 442 | scn == 445 | scn == 448
#                                        | scn == 451 | scn == 454 | scn == 457 | scn == 460 | scn == 463 | scn == 466 | scn == 469
#                                        | scn == 472 | scn == 475 | scn == 478 | scn == 481 | scn == 484 ~ 0.3,
#                                        scn == 245 | scn == 248 | scn == 251 | scn == 254 | scn == 257 | scn == 260 | scn == 263 | scn == 266 
#                                        | scn == 269 | scn == 272 | scn == 275 | scn == 278 | scn == 281 | scn == 284 | scn == 287 
#                                        | scn == 290 | scn == 293 | scn == 296 | scn == 299 | scn == 302 | scn == 305 | scn == 308 
#                                        | scn == 311 | scn == 314 | scn == 317 | scn == 320 | scn == 323 | scn == 326 | scn == 329 
#                                        | scn == 332 | scn == 335 | scn == 338 | scn == 341 | scn == 344 | scn == 347 | scn == 350
#                                        | scn == 353 | scn == 356 | scn == 359 | scn == 362 | scn == 365 | scn == 368 | scn == 371
#                                        | scn == 374 | scn == 377 | scn == 380 | scn == 383 | scn == 386 | scn == 389 | scn == 392
#                                        | scn == 395 | scn == 398 | scn == 401 | scn == 404 | scn == 407 | scn == 410 | scn == 413
#                                        | scn == 416 | scn == 419 | scn == 422 | scn == 425 | scn == 428 | scn == 431 | scn == 434
#                                        | scn == 437 | scn == 440 | scn == 443 | scn == 446 | scn == 449 | scn == 452 | scn == 455
#                                        | scn == 458 | scn == 461 | scn == 464 | scn == 467 | scn == 470 | scn == 473 | scn == 476
#                                        | scn == 479 | scn == 482 | scn == 485 ~ 0.7,
#                                        scn == 246 | scn == 249 | scn == 252 | scn == 255 | scn == 258 | scn == 261 | scn == 264 | scn == 267
#                                        | scn == 270 | scn == 273 | scn == 276 | scn == 279 | scn == 282 | scn == 285 | scn == 288 | scn == 291 
#                                        | scn == 294 | scn == 297 | scn == 300 | scn == 303 | scn == 306 | scn == 309 | scn == 312 | scn == 315 
#                                        | scn == 318 | scn == 321 | scn == 324 | scn == 327 | scn == 330 | scn == 333 | scn == 336 | scn == 339 
#                                        | scn == 342 | scn == 345 | scn == 348 | scn == 351 | scn == 354 | scn == 357 | scn == 360
#                                        | scn == 363 | scn == 366 | scn == 369 | scn == 372 | scn == 375 | scn == 378 | scn == 381
#                                        | scn == 384 | scn == 387 | scn == 390 | scn == 393 | scn == 396 | scn == 399 | scn == 402
#                                        | scn == 405 | scn == 408 | scn == 411 | scn == 414 | scn == 417 | scn == 420 | scn == 423
#                                        | scn == 426 | scn == 429 | scn == 432 | scn == 435 | scn == 438 | scn == 441 | scn == 444
#                                        | scn == 447 | scn == 450 | scn == 453 | scn == 456 | scn == 459 | scn == 462 | scn == 465
#                                        | scn == 468 | scn == 471 | scn == 474 | scn == 477 | scn == 480 | scn == 483 | scn == 486 ~ 0.9,
#                                        
#                                        scn == 487 | scn == 490 | scn == 493 | scn == 496 | scn == 499 | scn == 502 | scn == 505 
#                                        | scn == 508 | scn == 511 | scn == 514 | scn == 517 | scn == 520 | scn == 523
#                                        | scn == 526 | scn == 529 | scn == 532 | scn == 535 | scn == 538 | scn == 541 | scn == 544 
#                                        | scn == 547 | scn == 550 | scn == 553 | scn == 556 | scn == 559 | scn == 562 | scn == 565 
#                                        | scn == 568 | scn == 571 | scn == 574 | scn == 577 | scn == 580 | scn == 583 | scn == 586 
#                                        | scn == 589 | scn == 592 | scn == 595 | scn == 598 | scn == 601 | scn == 604 | scn == 607
#                                        | scn == 610 | scn == 613 | scn == 616 | scn == 619 | scn == 622 | scn == 625 | scn == 628
#                                        | scn == 631 | scn == 634 | scn == 637 | scn == 640 | scn == 643 | scn == 646 | scn == 649
#                                        | scn == 652 | scn == 655 | scn == 658 | scn == 661 | scn == 664 | scn == 667 | scn == 670
#                                        | scn == 673 | scn == 676 | scn == 679 | scn == 682 | scn == 685 | scn == 688 | scn == 691
#                                        | scn == 694 | scn == 697 | scn == 700 | scn == 703 | scn == 706 | scn == 709 | scn == 712
#                                        | scn == 715 | scn == 718 | scn == 721 | scn == 724 | scn == 727 ~ 0.3,
#                                        scn == 488 | scn == 491 | scn == 494 | scn == 497 | scn == 500 | scn == 503 | scn == 506 | scn == 509 
#                                        | scn == 512 | scn == 515 | scn == 518 | scn == 521 | scn == 524 | scn == 527 | scn == 530 
#                                        | scn == 533 | scn == 536 | scn == 539 | scn == 542 | scn == 545 | scn == 548 | scn == 551 
#                                        | scn == 554 | scn == 557 | scn == 560 | scn == 563 | scn == 566 | scn == 569 | scn == 572 
#                                        | scn == 575 | scn == 578 | scn == 581 | scn == 584 | scn == 587 | scn == 590 | scn == 593
#                                        | scn == 596 | scn == 599 | scn == 602 | scn == 605 | scn == 608 | scn == 611 | scn == 614
#                                        | scn == 617 | scn == 620 | scn == 623 | scn == 626 | scn == 629 | scn == 632 | scn == 635
#                                        | scn == 638 | scn == 641 | scn == 644 | scn == 647 | scn == 650 | scn == 653 | scn == 656
#                                        | scn == 659 | scn == 662 | scn == 665 | scn == 668 | scn == 671 | scn == 674 | scn == 677
#                                        | scn == 680 | scn == 683 | scn == 686 | scn == 689 | scn == 692 | scn == 695 | scn == 698
#                                        | scn == 701 | scn == 704 | scn == 707 | scn == 710 | scn == 713 | scn == 716 | scn == 719
#                                        | scn == 722 | scn == 725 | scn == 728 ~ 0.7,
#                                        scn == 489 | scn == 492 | scn == 495 | scn == 498 | scn == 501 | scn == 504 | scn == 507 | scn == 510
#                                        | scn == 513 | scn == 516 | scn == 519 | scn == 522 | scn == 525 | scn == 528 | scn == 531 | scn == 534 
#                                        | scn == 537 | scn == 540 | scn == 543 | scn == 546 | scn == 549 | scn == 552 | scn == 555 | scn == 558 
#                                        | scn == 561 | scn == 564 | scn == 567 | scn == 570 | scn == 573 | scn == 576 | scn == 579 | scn == 582 
#                                        | scn == 585 | scn == 588 | scn == 591 | scn == 594 | scn == 597 | scn == 600 | scn == 603
#                                        | scn == 606 | scn == 609 | scn == 612 | scn == 615 | scn == 618 | scn == 621 | scn == 624
#                                        | scn == 627 | scn == 630 | scn == 633 | scn == 636 | scn == 639 | scn == 642 | scn == 645
#                                        | scn == 648 | scn == 651 | scn == 654 | scn == 657 | scn == 660 | scn == 663 | scn == 666
#                                        | scn == 669 | scn == 672 | scn == 675 | scn == 678 | scn == 681 | scn == 684 | scn == 687
#                                        | scn == 690 | scn == 693 | scn == 696 | scn == 699 | scn == 702 | scn == 705 | scn == 708
#                                        | scn == 711 | scn == 714 | scn == 717 | scn == 720 | scn == 723 | scn == 726 | scn == 729 ~ 0.9)))

