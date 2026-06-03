# Evaluating simulations output - 1-tag multinomial likelihood model
# code to look at estimates of h, s, and l relative to true values
# Apr.17 2024

library(MCMCvis)
library(here)
library(tidyverse)
library(grid)
library(Metrics)
library(gridExtra)
library(RColorBrewer)
library(gt)
library(ggpubr)

# 1 - create path to .rds output files 
# 2 - read RDS files and assign name 
# 3 - for each file, assign variable names to h, s, and l
# 4 - create tmp dataframe with h, s, l, scn#
# 5 - bind rows of h, s, l dataframe and tmp dataframe.

file_paths <- list.files(path = here::here("simulations/outputs/01-tags/"), pattern = "*.rds", full.names = TRUE)

# Create an empty list to store data frames
df_list <- list()

for (i in 1:length(file_paths)) {
  files <- readRDS(file_paths[i])  # Read RDS file
  h <- unlist(files$h)   # Access the variables h, s, and l
  s <- unlist(files$s)
  l <- unlist(files$l)
  true_h <- unlist(files$th)
  true_s <- unlist(files$ts)
  true_l <- unlist(files$tl)
  fit <- files$fit

  # Create a data frame with h, s, l
  df_tmp <- tibble(h = h, s = s, l = l, true_h = true_h, true_l = true_l, true_s = true_s, scn = as.integer(str_extract(file_paths[i], "(?<=scn)[0-9]*")))
  
  # Store the data frame in the list
  df_list[[i]] <- df_tmp
}

# Combine all data frames into one
df <- bind_rows(df_list)
df <- df %>% group_by(scn) %>% 
  mutate(sim = 1:n())       # add a column for simulation number

# # tidy data 
df_long <- df %>%
  pivot_longer(cols = c(h, s, l), names_to = "parameter", values_to = "estimate") %>%
  mutate(true_value = case_when(parameter == "h" ~ true_h, parameter == "s" ~ true_s, parameter == "l" ~ true_l)) %>%
  group_by(scn, parameter, true_value, true_h, true_s, true_l) %>%
  summarize(rmse = rmse(as.numeric(estimate), as.numeric(true_value)),
            rel_rmse = (rmse(as.numeric(estimate), as.numeric(true_value)))/as.numeric(true_value),
            bias = bias(as.numeric(estimate), as.numeric(true_value)),
            mean_estimate = mean(as.numeric(estimate))) |>
  ungroup() |>
  mutate(ntags = case_when(scn < 28 ~ 50, scn >= 28 & scn <= 54 ~ 100, scn >= 55 & scn <= 81 ~ 200, scn >= 82 & scn <= 108 ~ 50, 
                           scn >= 109 & scn <= 135 ~ 100, scn >= 136 & scn <= 162 ~ 200, scn >= 163 & scn <= 189 ~ 50, 
                           scn >= 190 & scn <= 216 ~ 100, scn >= 217 & scn <= 243 ~ 200),
         yrs = case_when(scn < 82 ~ 2, scn >= 82 & scn <= 162 ~ 5, scn >= 163 & scn <= 243 ~ 10)) %>%
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
(hplot_y_bias <- df_long %>% 
  mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
  ggplot(aes(x = true_l, y = true_h, fill = Bias))+
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
  xlab("simulated reporting rate") + ylab("simulated harvest rate") +
  theme_minimal()+
  labs(title = "Tag-recovery bias by simulated parameter values and study duration",
       subtitle = "1-tag model")+
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
  facet_grid(yrs~true_s, labeller = label_both)+
  theme(panel.spacing = unit(-0.3, "lines")))

ggsave("simulations/outputs/01-tags/hplot_y_bias.png")

# with ntags:
(hplot_n_bias <- df_long %>% 
  mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
  ggplot(aes(x = true_l, y = true_h, fill = Bias))+
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
  xlab("simulated reporting rate") + ylab("simulated harvest rate") +
  theme_minimal()+
  labs(title = "Tag-recovery bias by simulated parameter values and tags deployed",
       subtitle = "1-tag model")+
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
  facet_grid(ntags~true_s, labeller = label_both)+
  theme(panel.spacing = unit(-0.3, "lines")))

ggsave("simulations/outputs/01-tags/hplot_n_bias.png")


# Heatplots showing RMSE by parameters and yrs ----------------------------
# summarize RMSE by facet + cell grouping
df_summary <- df_long %>%
  group_by(yrs, true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))

df_ntags <- df_long %>%
  group_by(ntags, true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))

# (hplot_y_rmse <- df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
#   ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
#     scale_fill_viridis_c(limits = c(0, 0.7)) +
#     geom_text(aes(label = round(RMSE_mean, digits = 3)), 
#               colour = "white", size = 3) +
#   geom_tile() +
#   # scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
#        subtitle = "1-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   facet_grid(yrs~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines"))+
#     labs(fill='RMSE') 

hplot_y_rmse <- ggplot(df_summary, aes(x = true_l, y = true_h, fill = RMSE_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4)) +
  geom_text(aes(label = round(RMSE_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated reporting rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
       subtitle = "1-tag model") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  facet_grid(yrs ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='RMSE') 

hplot_n_rmse <- ggplot(df_ntags, aes(x = true_l, y = true_h, fill = RMSE_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4)) +
  geom_text(aes(label = round(RMSE_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated reporting rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
       subtitle = "1-tag model") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  facet_grid(ntags~true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='RMSE') 
  

# ggsave("simulations/outputs/01-tags/hplot_y_rmse.png")

#####

# Mean bias and RMSE by value of yrs:
yrs2 <- df_long %>%
  filter(yrs == 2) %>%
  filter(parameter == "s") %>%
  filter(true_value == 0.8)

mean(yrs2$bias)
mean(yrs2$percent_bias)
mean(yrs2$rmse)

yrs5 <- df_long %>%
  filter(yrs == 5)
mean(yrs5$bias)
mean(yrs5$percent_bias)
mean(yrs5$rmse)

yrs10 <- df_long %>%
  filter(yrs == 10) %>%
  filter(parameter == "s") %>%
  filter(true_value == 0.8)

mean(yrs10$bias)
mean(yrs10$percent_bias)
mean(yrs10$rmse)

# Mean bias and RMSE by value of ntags
tags50 <- df_long %>%
  filter(ntags == 50) %>%
  filter(parameter == "s") %>%
  filter(true_value == 0.8)

mean(tags50$bias)
mean(tags50$percent_bias)
mean(tags50$rmse)

tags100 <- df_long %>%
  filter(ntags == 100)
mean(tags100$bias)
mean(tags100$percent_bias)
mean(tags100$rmse)

tags200 <- df_long %>%
  filter(ntags == 200)
mean(tags200$bias)
mean(tags200$percent_bias)
mean(tags200$rmse)

###
s_5 <- df_long %>% filter(true_s == 0.5) %>%
  filter(parameter == "s")

mean(s_5$bias)

mean(s_5$rmse)

s_8 <- df_long %>% filter(true_s == 0.8) %>%
  filter(parameter == "s")

mean(s_8$bias)

mean(s_8$rmse)

s_9 <- df_long %>% filter(true_s == 0.9) %>%
  filter(parameter == "s")

mean(s_9$bias)

mean(s_9$rmse)

###
h_05 <- df_long %>%filter(true_h == 0.05) %>%
  filter(parameter == "l")

mean(h_05$bias)
mean(h_05$percent_bias)
mean(h_05$abs_bias)
mean(h_05$rmse)

h_1 <- df_long %>% filter(true_h == 0.10) %>%
  filter(parameter == "l")

mean(h_1$bias)
mean(h_1$percent_bias)
mean(h_1$abs_bias)
mean(h_1$rmse)

h_2 <- df_long %>% filter(true_h == 0.20) %>%
  filter(parameter == "l")

mean(h_2$bias)
mean(h_2$percent_bias)
mean(h_2$abs_bias)
mean(h_2$rmse)

### this is where there is surprising result (bias and RMSE are both higher under higher reporting rates)
l_3 <- df_long %>%
  filter(true_l == 0.3) %>%
  filter(parameter == "l")

mean(l_3$bias)
mean(l_3$rmse)

l_7 <- df_long %>%
  filter(true_l == 0.7) %>%
  filter(parameter == "l")

mean(l_7$bias)
mean(l_7$rmse)

l_9 <- df_long %>%
  filter(true_l == 0.9) %>%
  filter(parameter == "l")

mean(l_9$bias)
mean(l_9$rmse)

# Table of RMSE and bias by scenario -----------------------------------------------

# scn_tbl <- df_long %>%
#   group_by(scn, parameter, true_value) %>% 
#   summarize(Estimate = mean_estimate,
#             Bias = mean(as.numeric(true_value) - as.numeric(estimate), na.rm = TRUE),
#             RMSE = sqrt(mean((as.numeric(estimate) - as.numeric(true_value)^2), na.rm = TRUE)))
# 
# summary_table1 <- gt(scn_tbl) %>%
#   fmt_number(
#     columns = vars(Estimate, Bias, RMSE),
#     decimals = 4, 
#     drop_trailing_zeros = TRUE) %>%
#   tab_header(title = "Bias and RMSE of Simulated Burbot Tag-Recovery Scenarios")
# 
# print(summary_table1)

# check Rhat values -------------------------------------------------------

# Loop through all files:
for (i in 1:length(files)){
  mcmc <- MCMCsummary(files$fit[[1]], params = c("mean.h", "mean.s", "mean.l"))
  rhat <- mcmc$Rhat
  check <- all(rhat < 1.1)
}
filter(mcmc, Rhat > 1.1)

# RMSE plots faceted by both years and ntags ------------------------------
# 
# # harvest
# df_long %>%
#   filter(parameter=="h") %>%
#   ggplot(aes(x = scn, y = rmse, color=true_value))+
#   geom_point(size=1) +
#   geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#   ggtitle("RMSE in estimated harvest rate by study duration and sample size")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   ylab("RMSE")+
#   xlab("scenario number")+
#   scale_color_gradient(low = "lightblue", high = "navy")+
#   facet_grid(ntags~yrs, scales="free", labeller = label_both) +
#   theme_minimal()
# 
# # survival
# df_long %>%
#   filter(parameter=="s") %>%
#   ggplot(aes(x = scn, y = rmse, color=true_value))+
#   geom_point(size=1) +
#   geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#   ggtitle("RMSE in estimated survival rate by study duration and sample size")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   ylab("RMSE")+
#   xlab("scenario number")+
#   scale_color_gradient(low = "palegreen", high = "#01665E")+
#   facet_grid(ntags~yrs, scales="free", labeller = label_both) +
#   theme_minimal()
# 
# # reporting
# df_long %>%
#   filter(parameter=="l") %>%
#   ggplot(aes(x = scn, y = rmse, color=true_value))+
#   geom_point(size=1) +
#   geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#   ggtitle("RMSE in estimated tag-reporting rate by study duration and sample size")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   ylab("RMSE")+
#   xlab("scenario number")+
#   scale_color_gradient(low = "plum1", high = "darkslateblue")+
#   facet_grid(ntags~yrs, scales="free", labeller = label_both) +
#   theme_minimal()


# Error in each parameter by value of s
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
  ggplot(aes(x = scn, y = rmse, color=as.factor(true_value))) +
  geom_point(size=1) +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("RMSE")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")


grid.arrange(plot_h_s, plot_s_s, plot_l_s, top = textGrob("1-tag model: RMSE of parameter estimates by survival rate"))

###
# Error in parameters by value of h
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

grid.arrange(plot_h_h, plot_s_h, plot_l_h, top = textGrob("1-tag model: RMSE of parameter estimates by harvest rate"))

####
# Facet by l
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

grid.arrange(plot_h_l, plot_s_l, plot_l_l, top = textGrob("1-tag model: RMSE of parameter estimates by reporting rate"))

#############################################################

# Bias plots --------------------------------------------------------------
# (har <- df_long %>%
#    filter(parameter=="h") %>%
#    ggplot(aes(x = scn, y = bias, color=true_value))+
#    geom_point(size=1) +
#    geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#    ggtitle("Bias in estimated harvest rate by study duration and sample size")+
#    theme(plot.title = element_text(hjust = 0.5))+
#    ylab("Bias")+
#    xlab("scenario number")+
#    scale_color_gradient(low = "lightblue", high = "navy")+
#    facet_grid(ntags~yrs, scales="free", labeller = label_both) +
#   theme_minimal())
# 
# (surv <- df_long %>%
#     filter(parameter=="s") %>%
#     ggplot(aes(x = scn, y = bias, color=true_value))+
#     geom_point(size=1) +
#     geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#     ggtitle("Bias in estimated survival rate by study duration and sample size")+
#     theme(plot.title = element_text(hjust = 0.5))+
#     ylab("Bias")+
#     xlab("scenario number")+
#     scale_color_gradient(low = "palegreen", high = "#01665E")+
#     facet_grid(ntags~true_l, scales="free", labeller = label_both) +
#     theme_minimal())
# 
# (rep <- df_long %>%
#     filter(parameter=="l") %>%
#     ggplot(aes(x = scn, y = bias, color=true_value))+
#     geom_point(size=1) +
#     geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#     ggtitle("Bias in estimated tag-reporting rate by study duration and sample size")+
#     theme(plot.title = element_text(hjust = 0.5))+
#     ylab("Bias")+
#     xlab("scenario number")+
#     scale_color_gradient(low = "plum1", high = "darkslateblue")+
#     facet_grid(ntags~yrs, scales="free", labeller = label_both)+
#     theme_minimal())

# Facet by s
bias_h_s<- df_long %>%
  filter(parameter=="h") %>%
  ggplot(aes(x = scn, y = bias, color=as.factor(true_value)))+
  geom_point(size=1) +
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
  geom_point(size=1) +
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
  geom_point(size=1) +
  geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
  ggtitle("Tag-reporting rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Bias")+
  xlab("scenario number")+
  scale_colour_manual(values = c("0.7" = "#6A51A3", "0.9" = "#4A1486", "0.3" = "#9E9AC8"))+
  facet_wrap(~true_s, labeller = label_both) +
  theme_minimal() +
  labs(color = "True reporting rate")

grid.arrange(bias_h_s, bias_s_s, bias_l_s, top = textGrob("1-tag model: Bias in parameter estimates by survival rate"))

# Facet by h
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

grid.arrange(bias_h_h, bias_s_h, bias_l_h, top = textGrob("1-tag model: Bias in parameter estimates by harvest rate"))

####
# Facet by l
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

grid.arrange(bias_h_l, bias_s_l, bias_l_l, top = textGrob("1-tag model: Bias in parameter estimates by reporting rate"))


# Filter by scn and create actual vs observed plot for each scn -----------

scenarios <- unique(df_long$scn)

for (scn in scenarios) {
  plot_data2 <- df_long %>% filter(scn == !!scn)
  q <- ggplot(plot_data2, aes(x = parameter, y = (as.numeric(true_value) - as.numeric(estimate)), fill = parameter))+
    geom_hline(yintercept=0) +
    scale_fill_brewer(palette = "GnBu") +
    geom_boxplot() + 
    theme_minimal() +
    scale_y_continuous(breaks=seq(-0.50,0.50,0.05)) +
    labs(title = paste("Difference between estimated and true parameter value for Scenario", scn),
         x = "Parameter",
         y = "True value - Estimate")
  ggsave(filename = paste0("diff_Scn_", scn, ".png"), plot = q, path = here::here("simulations/outputs/01-tags/plots/diff/"))
}

# how to look at prior vs posterior distribution
MCMCtrace(output_2tag_scn13$fit[[1]], prior = pr, post_zm = FALSE, pdf = FALSE)









# old codes
# assign true values ------------------------------------------------------
# df_tru <- df %>%
#   mutate(h_true = case_when(scn < 10 ~ 0.05, scn < 19 ~ 0.10, scn < 28 ~ 0.20, scn < 37 ~ 0.05, scn < 46 ~ 0.10,
#                             scn < 55 ~ 0.20, scn < 64 ~ 0.05, scn < 73 ~ 0.10, scn < 82 ~ 0.20,
#                             scn < 91 ~ 0.05, scn < 100 ~ 0.10, scn < 109 ~ 0.20,
#                             scn < 118 ~ 0.05, scn < 127 ~ 0.10, scn < 136 ~ 0.20,
#                             scn < 145 ~ 0.05, scn < 154 ~ 0.10, scn < 163 ~ 0.20,
#                             scn < 172 ~ 0.05, scn < 181 ~ 0.10, scn < 190 ~ 0.20,
#                             scn < 199 ~ 0.05, scn < 208 ~ 0.10, scn < 217 ~ 0.20,
#                             scn < 226 ~ 0.05, scn < 235 ~ 0.10, scn < 244 ~ 0.20),
#          s_true = case_when(scn < 4 ~ 0.5, scn < 7 ~ 0.8, scn < 10 ~ 0.9, scn < 13 ~ 0.5, scn < 16 ~ 0.8, scn < 19 ~ 0.9,
#                             scn < 22 ~ 0.5, scn < 25 ~ 0.8, scn < 28 ~ 0.9, scn < 31 ~ 0.5, scn < 34 ~ 0.8, scn < 37 ~ 0.9,
#                             scn < 40 ~ 0.5, scn < 43 ~ 0.8, scn < 46 ~ 0.9, scn < 49 ~ 0.5, scn < 52 ~ 0.8, scn < 55 ~ 0.9,
#                             scn < 58 ~ 0.5, scn < 61 ~ 0.8, scn < 64 ~ 0.9, scn < 67 ~ 0.5, scn < 70 ~ 0.8, scn < 73 ~ 0.9,
#                             scn < 76 ~ 0.5, scn < 79 ~ 0.8, scn < 82 ~ 0.9,
#                             
#                             scn < 85 ~ 0.5, scn < 88 ~ 0.8, scn < 91 ~ 0.9, scn < 94 ~ 0.5, scn < 97 ~ 0.8, scn < 100 ~ 0.9,
#                             scn < 103 ~ 0.5, scn < 106 ~ 0.8, scn < 109 ~ 0.9, scn < 112 ~ 0.5, scn < 115 ~ 0.8, scn < 118 ~ 0.9,
#                             scn < 121 ~ 0.5, scn < 124 ~ 0.8, scn < 127 ~ 0.9, scn < 130 ~ 0.5, scn < 133 ~ 0.8, scn < 136 ~ 0.9,
#                             scn < 139 ~ 0.5, scn < 142 ~ 0.8, scn < 145 ~ 0.9, scn < 148 ~ 0.5, scn < 151 ~ 0.8, scn < 154 ~ 0.9,
#                             scn < 157 ~ 0.5, scn < 160 ~ 0.8, scn < 163 ~ 0.9,
#                             
#                             scn < 166 ~ 0.5, scn < 169 ~ 0.8, scn < 172 ~ 0.9, scn < 175 ~ 0.5, scn < 178 ~ 0.8, scn < 181 ~ 0.9,
#                             scn < 184 ~ 0.5, scn < 187 ~ 0.8, scn < 190 ~ 0.9, scn < 193 ~ 0.5, scn < 196 ~ 0.8, scn < 199 ~ 0.9,
#                             scn < 202 ~ 0.5, scn < 205 ~ 0.8, scn < 208 ~ 0.9, scn < 211 ~ 0.5, scn < 214 ~ 0.8, scn < 217 ~ 0.9,
#                             scn < 220 ~ 0.5, scn < 223 ~ 0.8, scn < 226 ~ 0.9, scn < 229 ~ 0.5, scn < 232 ~ 0.8, scn < 235 ~ 0.9,
#                             scn < 238 ~ 0.5, scn < 241 ~ 0.8, scn < 244 ~ 0.9),
#          l_true = case_when(#scn %in% c(1, 4, 3) ~ 0.3
#                            scn == 1 | scn == 4 | scn == 7 | scn == 10 | scn == 13 | scn == 16 | scn == 19 
#                             | scn == 22 | scn == 25 | scn == 28 | scn == 31 | scn == 34 | scn == 37
#                             | scn == 40 | scn == 43 | scn == 46 | scn == 49 | scn == 52 | scn == 55 | scn == 58 
#                             | scn == 61 | scn == 64 | scn == 67 | scn == 70 | scn == 73 | scn == 76 | scn == 79 ~ 0.3,
#                             scn == 2 | scn == 5 | scn == 8 | scn == 11 | scn == 14 | scn == 17 | scn == 20 | scn == 23 
#                             | scn == 26 | scn == 29 | scn == 32 | scn == 35 | scn == 38 | scn == 41 | scn == 44 
#                             | scn == 47 | scn == 50 | scn == 53 | scn == 56 | scn == 59 | scn == 62 | scn == 65 
#                             | scn == 68 | scn == 71 | scn == 74 | scn == 77 | scn == 80 ~ 0.7,
#                             scn == 3 | scn == 6 | scn == 9 | scn == 12 | scn ==15 | scn == 18 | scn == 21 | scn == 24
#                             | scn == 27 | scn == 30 | scn == 33 | scn == 36 | scn == 39 | scn == 42 | scn == 45 | scn == 48 
#                             | scn == 51 | scn == 54 | scn == 57 | scn == 60 | scn == 63 | scn == 66 | scn == 69 | scn == 72 
#                             | scn == 75 | scn == 78 | scn == 81 ~ 0.9,
#                             
#                             scn == 82 | scn == 85 | scn == 88 | scn == 91 | scn == 94 | scn == 97 | scn == 100 
#                             | scn == 103 | scn == 106 | scn == 109 | scn == 112 | scn == 115 | scn == 118
#                             | scn == 121 | scn == 124 | scn == 127 | scn == 130 | scn == 133 | scn == 136 | scn == 139 
#                             | scn == 142 | scn == 145 | scn == 148 | scn == 151 | scn == 154 | scn == 157 | scn == 160 ~ 0.3,
#                             scn == 83 | scn == 86 | scn == 89 | scn == 92 | scn == 95 | scn == 98 | scn == 101 | scn == 104 
#                             | scn == 107 | scn == 110 | scn == 113 | scn == 116 | scn == 119 | scn == 122 | scn == 125 
#                             | scn == 128 | scn == 131 | scn == 134 | scn == 137 | scn == 140 | scn == 143 | scn == 146 
#                             | scn == 149 | scn == 152 | scn == 155 | scn == 158 | scn == 161 ~ 0.7,
#                             scn == 84 | scn == 87 | scn == 90 | scn == 93 | scn == 96 | scn == 99 | scn == 102 | scn == 105
#                             | scn == 108 | scn == 111 | scn == 114 | scn == 117 | scn == 120 | scn == 123 | scn == 126 | scn == 129 
#                             | scn == 132 | scn == 135 | scn == 138 | scn == 141 | scn == 144 | scn == 147 | scn == 150 | scn == 153 
#                             | scn == 156 | scn == 159 | scn == 162 ~ 0.9,
#                             
#                             scn == 163 | scn == 166 | scn == 169 | scn == 172 | scn == 175 | scn == 178 | scn == 181 
#                             | scn == 184 | scn == 187 | scn == 190 | scn == 193 | scn == 196 | scn == 199
#                             | scn == 202 | scn == 205 | scn == 208 | scn == 211 | scn == 214 | scn == 217 | scn == 220 
#                             | scn == 223 | scn == 226 | scn == 229 | scn == 232 | scn == 235 | scn == 238 | scn == 241 ~ 0.3,
#                             scn == 164 | scn == 167 | scn == 170 | scn == 173 | scn == 176 | scn == 179 | scn == 182 | scn == 185 
#                             | scn == 188 | scn == 191 | scn == 194 | scn == 197 | scn == 200 | scn == 203 | scn == 206 
#                             | scn == 209 | scn == 212 | scn == 215 | scn == 218 | scn == 221 | scn == 224 | scn == 227 
#                             | scn == 230 | scn == 233 | scn == 236 | scn == 239 | scn == 242 ~ 0.7,
#                             scn == 165 | scn == 168 | scn == 171 | scn == 174 | scn == 177 | scn == 180 | scn == 183 | scn == 186
#                             | scn == 189 | scn == 192 | scn == 195 | scn == 198 | scn == 201 | scn == 204 | scn == 207 | scn == 210 
#                             | scn == 213 | scn == 216 | scn == 219 | scn == 222 | scn == 225 | scn == 228 | scn == 231 | scn == 234 
#                             | scn == 237 | scn == 240 | scn == 243 ~ 0.9),
#          ntags = case_when(scn < 28 ~ 50, scn < 55 ~ 100, scn < 82 ~ 200,
#                            scn < 109 ~ 50, scn < 136 ~ 100, scn < 163 ~ 200,
#                            scn < 190 ~ 50, scn < 217 ~ 100, scn < 244 ~ 200),
#          yrs = case_when(scn < 82 ~ 2, scn < 163 ~ 5, scn < 244 ~ 10))

# # Stat tests for RMSE across scenarios ------------------------------------
# library(FSA)
# 
# #bias
# df_long$yrs = factor(df_long$yrs) 
# aov_yrs <- aov(df_long$bias~yrs, data = df_long)
# hist(aov_yrs$residuals)
# qqPlot(aov_yrs$residuals) # ANOVA - does not meet the assumption of normality of residuals
# # Kruskal-Wallis test:
# kruskal.test(df_long$bias~yrs, data = df_long) # difference between at least one value of yrs
# # Dunn post-hoc test:
# dunnTest(df_long$bias~yrs, data = df_long, method = "holm")
# 
# # RMSE
# aov_yrs <- aov(rmse~yrs, data = df_long)
# hist(aov_yrs$residuals)
# qqPlot(aov_yrs$residuals)
# # Kruskal-Wallis test:
# kruskal.test(rmse~yrs, data = df_long) # difference between at least one value of yrs
# # Dunn post-hoc test:
# dunnTest(df_long$rmse~yrs, data = df_long, method = "holm") # significant difference in RMSE between each level of yrs
# 
# # Bias by ntags ### CHECK WHETHER I SHOULD USE ABSOLUTE VALUE ###
# df_long$ntags = factor(df_long$ntags)
# aov_ntags <- aov(abs(df_long$bias)~ntags, data = df_long)
# hist(aov_ntags$residuals)
# qqPlot(aov_ntags$residuals) # does not meet assumption of normality.
# kruskal.test(abs(df_long$bias)~ntags, data = df_long) # difference between at least one value of yrs
# dunnTest(abs(df_long$bias)~ntags, data = df_long, method = "holm") # significant difference in RMSE by each level of ntags.
# 
# # RMSE by ntags:
# df_long$ntags = factor(df_long$ntags)
# aov_ntags <- aov(df_long$rmse~ntags, data = df_long)
# hist(aov_ntags$residuals)
# qqPlot(aov_ntags$residuals) # does not meet assumption of normality.
# kruskal.test(df_long$rmse~ntags, data = df_long) # difference between at least one value of yrs
# dunnTest(df_long$rmse~ntags, data = df_long, method = "holm") # significant difference in RMSE by each level of ntags.

# bias in each parameter, by harvest values
# Create the data frame
data_h <- data.frame(
  h = c("h = 0.05", "h = 0.05", "h = 0.1", "h = 0.1", "h = 0.2", "h = 0.2"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.07353223, 0.09451528, 0.06321712, 0.09069893, 0.03721829, 0.08226329),
  "survival estimate" = c(-0.02163654, 0.1188913, -0.005524766, 0.09765445, -0.002263814, 0.08269965),
  "reporting estimate" = c(-0.3110745, 0.3317115, -0.1907104, 0.2406629, -0.07189761, 0.1636487),
  check.names = FALSE  # Prevent R from auto-renaming the columns
)

# Build the gt table
data_h %>%
  gt(groupname_col = "h") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of harvest (1-tag model)"
  ) %>%
  cols_label(
    Metric = "",
    `harvest estimate` = "harvest estimate",
    `survival estimate` = "survival estimate",
    `reporting estimate` = "reporting estimate") %>%
  fmt_number(
    columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`),
    decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# s values
# Create the data frame
data_s <- data.frame(
  s = c("s = 0.5", "s = 0.5", "s = 0.8", "s = 0.8", "s = 0.9", "s = 0.9"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.08393705, 0.1192064, 0.05123623, 0.0796576, 0.03879435, 0.06861348),
  "survival estimate" = c(0.04181147, 0.1097466, -0.01751326, 0.09437251, -0.05372333, 0.09512628),
  "reporting estimate" = c(-0.2565266, 0.2933641, -0.1825659, 0.2371201, -0.13459, 0.2055389),
  check.names = FALSE
)

data_s %>%
  gt(groupname_col = "s") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of survival (1-tag model)"
  ) %>%
  cols_label(
    Metric = "",
    `harvest estimate` = "harvest estimate",
    `survival estimate` = "survival estimate",
    `reporting estimate` = "reporting estimate"
  ) %>%
  fmt_number(
    columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# l value
# Create the data frame
data <- data.frame(
  l = c("l = 0.3", "l = 0.3", "l = 0.7", "l = 0.7", "l = 0.9", "l = 0.9"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.02819457, 0.0812716, 0.06556784, 0.08942303, 0.08020522, 0.09678287),
  "survival estimate" = c(-0.04148561, 0.1205939, 6.59E-05, 0.09042855, 0.01199457, 0.08822303),
  "reporting estimate" = c(-0.01195981, 0.1230008, -0.2195879, 0.2542702, -0.3421348, 0.3587521),
  check.names = FALSE
)

data %>%
  gt(groupname_col = "l") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of reporting rate (1-tag model)"
  ) %>%
  cols_label(
    Metric = "",
    `harvest estimate` = "harvest estimate",
    `survival estimate` = "survival estimate",
    `reporting estimate` = "reporting estimate"
  ) %>%
  fmt_number(
    columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`),
    decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(`harvest estimate`, `survival estimate`, `reporting estimate`))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )


