# Evaluating simulations output - 2-tag multinomial likelihood model
# code to look at estimates of h, s, and l relative to true values
# Apr.17 2024

library(MCMCvis)
library(here)
library(stringr)
library(tidyverse)
library(gridExtra)
library(grid)
library(Metrics)
library(RColorBrewer)
library(gt)

# 1 - create path to .rds output files 
# 2 - read RDS files and assign name 
# 3 - for each file, assign variable names to h, s, and l
# 4 - create tmp dataframe with h, s, l, scn#
# 5 - bind rows of h, s, l dataframe and tmp dataframe.

file_paths2 <- list.files(path = here::here("simulations/outputs/02-tags/"), pattern = "*.rds", full.names = TRUE)

# Create an empty list to store data frames
df_list2 <- list()

for (i in 1:length(file_paths2)) {
  files <- readRDS(file_paths2[i])  # Read RDS file
  h <- files$h   # Access the variables h, s, and l
  s <- files$s
  l <- files$l
  true_h <- unlist(files$th)
  true_s <- unlist(files$ts)
  true_l <- unlist(files$tl)
  fit <- files$fit
  
  # Create a data frame with s, r, p
  df_tmp2 <- tibble(h = h, s = s, l = l, true_h = true_h, true_l = true_l, true_s = true_s, scn = as.integer(str_extract(file_paths2[i], "(?<=scn)[0-9]*")))
  
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
mean(h_data$percent_bias)
mean(h_data$rmse)

mean(s_data$bias)
mean(s_data$percent_bias)
mean(s_data$rmse)

mean(l_data$bias)
mean(l_data$percent_bias)
mean(l_data$rmse)

# Heatplots showing BIAS by parameter values and yrs ----------------------
###

# (hplot_y_bias <- df_long %>% 
#    mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#    ggplot(aes(x = true_l, y = true_h, fill = Bias)) +
#    geom_tile() +
#    # geom_text(aes(label = round(Bias, digits = 5))) +
#    # scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#    scale_fill_gradient(low = "blue", mid = "cyan", high = "purple",
#                        midpoint = mean(rng),
#                        breaks = seq(-100, 100, 4),
#                        limits = c(floor(rng[1]), ceiling(rng[2])))+
#    xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#    theme_minimal()+
#    labs(title = "Tag-recovery bias by simulated parameter values and study duration",
#         subtitle = "2-tag model")+
#    theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#    facet_grid(yrs~true_s, labeller = label_both)+
#    theme(panel.spacing = unit(-0.3, "lines")))

# (hplot_y_perbias <- df_long %>% 
#     mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#     ggplot(aes(x = true_l, y = true_h, fill = Bias)) +
#     geom_tile() +
#     # geom_text(aes(label = round(Bias, digits = 5))) +
#     scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#     xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#     theme_minimal()+
#     labs(title = "Tag-recovery bias by simulated parameter values and study duration",
#          subtitle = "2-tag model")+
#     theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#     facet_grid(yrs~true_s, labeller = label_both)+
#     theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/02-tags/hplot_y_bias.png")

# # with ntags:
# (hplot_n_bias <- df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#   ggplot(aes(x = true_l, y = true_h, fill = Bias))+
#   geom_tile() +
#   scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery bias by simulated parameter values and tags deployed",
#        subtitle = "2-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   facet_grid(ntags~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/02-tags/hplot_n_bias.png")
# 
# (hplot_n_perbias <- df_long %>% 
#     mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), Bias = bias) %>%
#     ggplot(aes(x = true_l, y = true_h, fill = Bias))+
#     geom_tile() +
#     scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#     xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#     theme_minimal()+
#     labs(title = "Tag-recovery bias by simulated parameter values and tags deployed",
#          subtitle = "2-tag model")+
#     theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#     facet_grid(ntags~true_s, labeller = label_both)+
#     theme(panel.spacing = unit(-0.3, "lines")))

# Heatplots showing RMSE by parameters and yrs ----------------------------
# x <- matrix(0:0.4, 0.05)
# y <- matrix()
# rng = range(c((x), (y)))

# (hplot_y_rmse <- df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
#   ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
#   geom_tile() +
#    scale_fill_viridis_c(limits = c(0, 0.4)) +
#    geom_text(aes(label = round((RMSE), digits = 3)), colour = "white", size = 3) +
#   # scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
#        subtitle = "2-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   facet_grid(yrs~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines")))

# summarize RMSE by facet + cell grouping
df_summary <- df_long %>%
  group_by(yrs, true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))

df_ntags <- df_long %>%
  group_by(ntags, true_s, true_l, true_h) %>%
  summarise(RMSE_mean = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(true_s, true_l, true_h), factor))

hplot_y_rmse <- ggplot(df_summary, aes(x = true_l, y = true_h, fill = RMSE_mean)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.4)) +
  geom_text(aes(label = round(RMSE_mean, digits = 3)), 
            colour = "white", size = 3) +
  xlab("simulated reporting rate") + 
  ylab("simulated harvest rate") +
  theme_minimal() +
  labs(title = "Tag-recovery RMSE by simulated parameter values and study duration",
       subtitle = "2-tag model") +
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
  labs(title = "Tag-recovery RMSE by simulated parameter values and tags deployed",
       subtitle = "2-tag model") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9)) +
  facet_grid(ntags ~ true_s, labeller = label_both) +
  theme(panel.spacing = unit(-0.3, "lines")) +
  labs(fill='RMSE') 

ggsave("simulations/outputs/02-tags/hplot_y_rmse.png")

# # by ntags
# (hplot_n_rmse <- df_long %>% 
#   mutate(true_s = factor(true_s), true_l = factor(true_l), true_h = factor(true_h), RMSE = rmse) %>%
#   ggplot(aes(x = true_l, y = true_h, fill = RMSE))+
#   geom_tile() +
#     scale_fill_viridis_c(limits = c(0, 0.4)) +
#   # scale_fill_gradientn(colors = hcl.colors(6, "RdYlBu"))+
#   xlab("simulated reporting rate") + ylab("simulated harvest rate") +
#   theme_minimal()+
#   labs(title = "Tag-recovery RMSE by simulated parameter values and tags deployed",
#        subtitle = "2-tag model")+
#   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9))+ 
#   facet_grid(ntags~true_s, labeller = label_both)+
#   theme(panel.spacing = unit(-0.3, "lines")))
# 
# ggsave("simulations/outputs/02-tags/hplot_n_rmse.png")

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

mean(yrs2$percent_bias)
mean(yrs5$percent_bias)
mean(yrs10$percent_bias)

# Mean bias and RMSE by value of ntags
tags50 <- df_long %>%
  filter(ntags == 50)
mean(tags50$bias)
mean(tags50$rel_rmse)
mean(tags50$rmse)

tags100 <- df_long %>%
  filter(ntags == 100)
mean(tags100$bias)
mean(tags100$rel_rmse)
mean(tags100$rmse)

tags200 <- df_long %>%
  filter(ntags == 200)
mean(tags200$bias)
mean(tags200$rel_rmse)
mean(tags200$rmse)

mean(tags50$percent_bias)
mean(tags100$percent_bias)
mean(tags200$percent_bias)

###
h_5 <- df_long %>%
  filter(true_h == 0.05) %>%
  filter(parameter == "h")

mean(h_5$bias)
mean(h_5$rmse)

h_1 <- df_long %>%
  filter(true_h == 0.10) %>%
  filter(parameter == "l")

mean(h_1$bias)
mean(h_1$rmse)

h_2 <- df_long %>%
  filter(true_h == 0.20) %>%
  filter(parameter == "l")

mean(h_2$bias)
mean(h_2$rmse)

###
s_5 <- df_long %>%
  filter(true_s == 0.5) %>%
  filter(parameter == "l")

mean(s_5$bias)
mean(s_5$rmse)

s_8 <- df_long %>%
  filter(true_s == 0.8) %>%
  filter(parameter == "l")

mean(s_8$bias)
mean(s_8$rmse)

s_9 <- df_long %>%
  filter(true_s == 0.9) %>%
  filter(parameter == "l")

mean(s_9$bias)
mean(s_9$rmse)

###  Look at estimation of parameters OTHER THAN reporting rate itself
l_3 <- df_long %>%
  filter(true_l == 0.3) %>%
  filter(parameter == "l")
 # filter(parameter == "s" | parameter == "h")

mean(l_3$bias)
mean(l_3$rmse)

l_7 <- df_long %>%
  filter(true_l == 0.7) %>%
  filter(parameter == "l")
  #filter(parameter == "s" | parameter == "h")

mean(l_7$bias)
mean(l_7$rmse)

l_9 <- df_long %>%
  filter(true_l == 0.9) %>%
  filter(parameter == "l")
  #filter(parameter == "s" | parameter == "h")

mean(l_9$bias)
mean(l_9$rmse)


# Error (RMSE) in each parameter by value of s
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


grid.arrange(plot_h_s, plot_s_s, plot_l_s, top = textGrob("2-tag model: RMSE of parameter estimates by survival rate"))

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

grid.arrange(plot_h_h, plot_s_h, plot_l_h, top = textGrob("2-tag model: RMSE of parameter estimates by harvest rate"))

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

grid.arrange(plot_h_l, plot_s_l, plot_l_l, top = textGrob("2-tag model: RMSE of parameter estimates by reporting rate"))


# Bias by each parameter --------------------------------------------------

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

grid.arrange(bias_h_s, bias_s_s, bias_l_s, top = textGrob("2-tag model: Bias in parameter estimates by survival rate"))

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

grid.arrange(bias_h_h, bias_s_h, bias_l_h, top = textGrob("2-tag model: Bias in parameter estimates by harvest rate"))

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

grid.arrange(bias_h_l, bias_s_l, bias_l_l, top = textGrob("2-tag model: Bias in parameter estimates by reporting rate"))


# check Rhat values -------------------------------------------------------
# Loop through all files:
for (i in 1:length(files)){
  mcmc <- MCMCsummary(files$fit[[1]], params = c("mean.h", "mean.s", "mean.l"))
  rhat <- mcmc$Rhat
  check <- all(rhat < 1.1)
}
filter(mcmc, Rhat > 1.1)


# Create a traceplot for each simulation ----------------------------------

#loop does not seem to be working 
pr <- matrix(rbeta(3 * 2000, 1, 1), ncol = 3)

for (i in 1:length(files)){
  mcmcplot <- MCMCtrace(files$fit[[i]], prior = pr, pdf = FALSE)
  rhat <- mcmc$Rhat
  check <- all(rhat < 1.1)
  #ggsave(filename = paste0("trace_", i, ".png"), plot = mcmcplot, path = "simulations/outputs/02-tags/MCMCplots/")
}

# Create actual vs observed plot for each scn -----------
# 
# scenarios <- unique(df_long$scn)
# # 
# for (scn in scenarios) {
#   plot_data <- df_long %>% filter(scn == !!scn)
#   p <- ggplot(plot_data, aes(x = as.factor(true_value), y = as.numeric(mean_estimate), fill = parameter))+
#     facet_wrap(~parameter) +
#     geom_boxplot() +
#     #geom_text(label= round(plot_data$bias, 3), nudge_x=0.1, nudge_y=0.01, check_overlap=T) +
#     theme_minimal() +
#     labs(title = paste("Estimated vs actual parameter values for Scenario", scn),
#          x = "True parameter value",
#          y = "Estimate")
#   ggsave(filename = paste0("est_tru_Scn_", scn, ".png"), plot = p, path = here::here("plots/2tag/est_tru/"))
# }
# 
# 
# for (scn in scenarios) {
#   plot_data2 <- df_long %>% filter(scn == !!scn)
#   q <- ggplot(plot_data2, aes(x = parameter, y = (as.numeric(true_value) - as.numeric(mean_estimate)), fill = parameter))+
#     geom_hline(yintercept=0) +
#     scale_fill_brewer(palette = "GnBu") +
#     geom_boxplot() + 
#     theme_minimal() +
#     scale_y_continuous(breaks=seq(-0.50,0.50,0.05)) +
#     labs(title = paste("Difference between estimated and true parameter value for Scenario", scn,": 2-tag model"),
#          x = "Parameter",
#          y = "True value - Estimate")
#   ggsave(filename = paste0("diff_Scn_", scn, ".png"), plot = q, path = here::here("plots/2tag/diff/"))
# }
# 


# # RMSE plots --------------------------------------------------------------
# 
# # harvest
# df_long %>%
#    filter(parameter=="h") %>%
#    ggplot(aes(x = scn, y = rmse, color=true_value))+
#    geom_point(size=1) +
#    geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#    ggtitle("RMSE in estimated harvest rate by study duration and sample size")+
#    theme(plot.title = element_text(hjust = 0.5))+
#    ylab("RMSE")+
#    xlab("scenario number")+
#    scale_color_gradient(low = "lightblue", high = "navy")+
#    facet_grid(ntags~yrs, scales="free", labeller = label_both)
# 
# # survival
# df_long %>%
#     filter(parameter=="s") %>%
#     ggplot(aes(x = scn, y = rmse, color=true_value))+
#     geom_point(size=1) +
#     geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#     ggtitle("RMSE in estimated survival rate by study duration and sample size")+
#     theme(plot.title = element_text(hjust = 0.5))+
#     ylab("RMSE")+
#     xlab("scenario number")+
#     scale_color_gradient(low = "palegreen", high = "#01665E")+
#     facet_grid(ntags~yrs, scales="free", labeller = label_both)
# 
# # reporting
# df_long %>%
#     filter(parameter=="l") %>%
#     ggplot(aes(x = scn, y = rmse, color=true_value))+
#     geom_point(size=1) +
#     geom_hline(yintercept=0, linetype = "dashed", colour="darkgrey") +
#     ggtitle("RMSE in estimated tag-reporting rate by study duration and sample size")+
#     theme(plot.title = element_text(hjust = 0.5))+
#     ylab("RMSE")+
#     xlab("scenario number")+
#     scale_color_gradient(low = "plum1", high = "darkslateblue")+
#     facet_grid(ntags~yrs, scales="free", labeller = label_both)
# 
######################
# 
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
#    facet_grid(ntags~yrs, scales="free", labeller = label_both))
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
#     facet_grid(ntags~yrs, scales="free", labeller = label_both))
# 
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
#     facet_grid(ntags~yrs, scales="free", labeller = label_both))


# Create the data frame
data <- data.frame(
  h = c("h = 0.05", "h = 0.05", "h = 0.1", "h = 0.1", "h = 0.2", "h = 0.2"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.002468376, 0.008333319, 0.002527022, 0.01152128, 0.002210907, 0.01527307),
  "survival estimate" = c(-0.02864356, 0.0852504, -0.01570044, 0.06409786, -0.008230963, 0.0483778),
  "reporting estimate" = c(-0.02286839, 0.1026219, -0.008238312, 0.07786513, -0.00477641, 0.05743784),
  check.names = FALSE  # Prevent R from auto-renaming the columns
)

# Build the gt table
data %>%
  gt(groupname_col = "h") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of harvest (2-tag model)"
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

# survival values

# Create the data frame
data <- data.frame(
  s = c("s = 0.5", "s = 0.5", "s = 0.8", "s = 0.8", "s = 0.9", "s = 0.9"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.000988779, 0.01263194, 0.002564361, 0.01128056, 0.003653165, 0.01121517),
  "survival estimate" = c(-0.006933439, 0.06785675, -0.02030242, 0.06247797, -0.03920599, 0.06739133),
  "reporting estimate" = c(-0.0145727, 0.09024264, -0.0116222, 0.07570255, -0.009682815, 0.07197972),
  check.names = FALSE
)

# Build the gt table with styling
data %>%
  gt(groupname_col = "s") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of survival (2-tag model)"
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

# reporting rate table 

# Create the data frame
data <- data.frame(
  l = c("l = 0.3", "l = 0.3", "l = 0.7", "l = 0.7", "l = 0.9", "l = 0.9"),
  Metric = c("mean bias", "mean RMSE", "mean bias", "mean RMSE", "mean bias", "mean RMSE"),
  "harvest estimate" = c(0.000926794, 0.01215254, 0.001773041, 0.01150604, 0.004506047, 0.01146908),
  "survival estimate" = c(-0.02089626, 0.07224225, -0.0166172, 0.06413346, -0.01506151, 0.06135034),
  "reporting estimate" = c(0.01480912, 0.06771041, -0.00281837, 0.08457116, -0.04787387, 0.08564334),
  check.names = FALSE
)

# Build the gt table with styling
data %>%
  gt(groupname_col = "l") %>%
  tab_header(
    title = "Accuracy of parameter estimates by value of reporting rate (2-tag model)"
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

