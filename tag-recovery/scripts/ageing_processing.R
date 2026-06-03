library(ggplot2)
library(gridExtra)
library(tidyverse)

# Read in ageing data -----------------------------------------------------
age_data <- read_csv("carp_lk/data/raw/age_data.csv")

# Carp Lake ---------------------------------------------------------------
# age frequency histogram
(age_hist_c <- age_data %>%
  filter(Lake == "Carp Lake") %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth= 1, color = "darkseagreen4", fill = "darkseagreen1") +
  ggtitle("Age distribution of harvested burbot at Carp Lake") +
  theme_minimal() +
  xlab("Age (years)") +
  theme(plot.title = element_text(size=18)))


# Fraser Lake -------------------------------------------------------------
(age_hist_f <- age_data %>%
  filter(Lake == "Fraser Lake") %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth= 1, color = "darkseagreen4", fill = "darkseagreen1") +
  ggtitle("Age distribution of burbot mortalities at Fraser Lake") +
  theme_bw() +
  xlab("Age (years)") +
  theme(plot.title = element_text(size=10)))

grid.arrange(age_hist_c, age_hist_f, nrow = 1)

# Boxplot showing age of burbot at both lakes -----------------------------
age_boxplot <- age_data %>%
  na.omit() %>%
  ggplot(aes(x = Lake, y = age, fill = Lake)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "BuGn") +
  theme_bw() +
  ylab("Age of burbot (yrs)") +
  xlab("Lake") +
  ggtitle("Burbot ages at Carp Lake and Fraser Lake, \n from harvests and mortalities") +
  theme(legend.position = "none")
  #+ stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")

# t-test to compare age between Carp Lake and Fraser Lake burbot ----------

t.test(age ~ Lake, data = age_data)
fra_ages <- filter(age_data, age_data$Lake =="Fraser Lake")
