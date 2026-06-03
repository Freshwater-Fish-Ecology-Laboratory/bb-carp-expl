# code to process length/weight/age data for Carp Lake sampled burbot
library(tidyverse)
library(gridExtra)
library(devtools)

creel_bbdata <- read_csv("carp_lk/data/creel/carp_creel_bbdata.csv")


# Length and weight frequency histograms ----------------------------------------------
(len_hist <- creel_bbdata %>%
  mutate(length_mm = (`Total Length (cm)`*10)) %>%
  ggplot(aes(x = length_mm)) +
  ylab("Number of burbot") + xlab("Length (mm)") + 
  geom_histogram(color = "navy", fill = "lightblue", bins = 15) +
  theme_minimal() +
  # ggtitle("Length of harvested burbot at Carp Lake, 2023") +
  theme(plot.title = element_text(size=15))+
  scale_y_continuous(limits = c(0, 12), breaks=seq(0,13,1)))

(wt_hist <- creel_bbdata %>%
  ggplot(aes(x = `Weight (g)`)) +
  geom_histogram(color = "darkseagreen4", fill = "darkseagreen1", bins = 15) +
  theme_minimal() +
  # ggtitle("Weight of harvested burbot at Carp Lake, 2023") +
  ylab("") + xlab("Weight (g)") + 
  scale_x_continuous(breaks = seq(0, 7000, 1000)) +
  theme(plot.title = element_text(size=15))+
  scale_y_continuous(limits = c(0, 12), breaks=seq(0,12,1)))

grid.arrange(len_hist, wt_hist, nrow = 1, top = "Lengths and weights of burbot harvested by survey participants")

# Length-weight scatterplot -----------------------------------------------
lmheight_wt <- lm(creel_bbdata$`Weight (g)` ~ creel_bbdata$`Total Length (cm)`, data = creel_bbdata)
summary(lmheight_wt)

library(ggpmisc)

(lw_scatter <- creel_bbdata %>%
  ggplot(aes(x = `Total Length (cm)`, y = `Weight (g)`)) +
  # geom_smooth(method = "lm", formula= y~x, se = TRUE, color = "grey70") +
  stat_poly_line(color = "grey40") +
  stat_poly_eq() +
  geom_point(color="black") +
  theme_minimal() +
  ggtitle("Length vs weight of burbot harvested by survey participants") +
  theme(plot.title = element_text(size=13)))

# Age frequency histogram -------------------------------------------------
age_hist <- creel_bbdata %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth= 1, color = "darkseagreen4", fill = "darkseagreen1") +
  ggtitle("Ages of burbot harvested by survey participants") +
  theme_minimal() +
  xlab("Age (years)") + ylab("Number of burbot")+
  scale_y_continuous(breaks = seq(0, 10, by = 2))+
  scale_x_continuous(breaks = seq(6, 20, by = 2))

# Age-length and age-weight scatterplots ------------------------------------------------
(age_len_scatter <- creel_bbdata %>%
  ggplot(aes(x = age, y = `Total Length (cm)`)) +
  geom_point(color="black") +
  # geom_smooth(method = "lm", formula= y~x, se = TRUE, color = "grey70") +
   stat_poly_line(color = "grey40") +
   stat_poly_eq() +
  scale_x_continuous(name = "Age (years)",
                     breaks = seq(9, 19, 1),
                     limits=c(9, 19)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle("Age vs length of burbot harvested by survey participants") +
   theme_minimal())

# Length-age regression
lm_age_len <- lm(creel_bbdata$`Total Length (cm)` ~ creel_bbdata$age, data = creel_bbdata)
summary(lm_age_len)

(age_wt_scatter <- creel_bbdata %>%
  ggplot(aes(x = age, y = `Weight (g)`)) +
  geom_point(color="black") +
  # geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "grey30") +
  stat_poly_line(color = "grey40") +
  stat_poly_eq() +
  theme_minimal() +
  scale_x_continuous(name = "Age (years)",
                     breaks = seq(9, 19, 1),
                     limits=c(9, 19)) +
  ggtitle("Age vs weight of burbot harvested by survey participants"))

# Weight-age regression
lm_age_wt <- lm(creel_bbdata$`Weight (g)` ~ creel_bbdata$age, data = creel_bbdata)
summary(lm_age_wt)

# grid.arrange(len_hist, wt_hist, lw_scatter, age_hist, age_len_scatter, age_wt_scatter, nrow = 2)
# grid.arrange(len_hist, wt_hist, nrow = 1)

grid.arrange(age_len_scatter, age_wt_scatter, nrow = 1, top = "Age vs length and weight of burbot harvested by survey participants")

# Median and IQR for length, weight, and age of HARVESTED burbot from creel 
median(creel_bbdata$`Weight (g)`, na.rm = TRUE)
quantile(creel_bbdata$`Weight (g)`, na.rm = TRUE)

median(creel_bbdata$`Total Length (cm)`, na.rm = TRUE)
quantile(creel_bbdata$`Total Length (cm)`, na.rm = TRUE)
min(creel_bbdata$age, na.rm = TRUE)
median(creel_bbdata$age, na.rm = TRUE)
quantile(creel_bbdata$age, na.rm = TRUE)
