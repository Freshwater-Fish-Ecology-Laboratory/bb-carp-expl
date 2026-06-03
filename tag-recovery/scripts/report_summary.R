# script to create a plot showing tag reports per month 
# (or exact reporting date using Julian day of the year)
# X-asix = month (or day), 
# Y-axis = number of reports (different colour bars per reporting year). 

library(tidyverse)

reports <- read.csv("carp_lk/data/raw/report_summary.csv")

# Ensure month is a proper factor with Jan–Dec order
month_levels <- c("January", "February", "March", "April", "May", "June", "July",
                  "August", "September", "October", "November", "December")

reports_clean <- reports %>%
  # keep unique fish_num per date
  distinct(year, month, Date, fish_num, .keep_all = TRUE) %>%
  mutate(month = factor(month, levels = month_levels)) %>%
  group_by(year, month) %>%
  summarise(total_fish = n(), .groups = "drop") %>%
  # make sure all year-month combos exist
  complete(year, month, fill = list(total_fish = 0))

ggplot(reports_clean, aes(x = month, y = total_fish, fill = as.factor(year))) +
  geom_col(position = "stack") +
  labs(x = "Month", y = "Number of tag reports", fill = "Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
