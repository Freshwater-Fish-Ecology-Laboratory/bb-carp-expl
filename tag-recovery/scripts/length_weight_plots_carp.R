# Code to create length and weight frequency histograms for Carp Lake burbot (2022)
# Liz Hirsch
# March 7, 2023
library(tidyverse)


# 2022 data ---------------------------------------------------------------


source("carp_lk/codes/R/burbot_2021_2022_summary.R")
source("carp_lk/codes/processing/bb_2022_tag_processing.R")
source("carp_lk/codes/processing/traps_2023_processing.R")
source("carp_lk/codes/processing/traps_2022_processing.R")


# Length frequency histogram
# convert length in cm to length in mm

length_22_hist <- ggplot(carp_bb %>%
         mutate(length_mm = (length*10)), aes(x = length_mm)) + 
  geom_histogram(color = "navy", fill = "white", bins = 15) + 
  ylab("Number of burbot") + xlab("Length (mm)") + 
  theme_bw() +
  theme(panel.border = element_blank()) + 
  ggtitle("Length distribution of burbot tagged at Carp Lake, 2022") + 
  scale_y_continuous(breaks = seq(0, 40, by = 5))

mean(carp_bb$length*10, na.rm = TRUE) # 676.9217 mm
median(carp_bb$length*10, na.rm = TRUE) # 650 mm
min(carp_bb$length*10, na.rm = TRUE)
# Weight frequency histogram
# range: 700g - 6300g
# remove the NA (burbot whose weight was not measured)

weight_22_hist <- carp_bb %>%
  subset(!is.na(weight)) %>%
  ggplot(aes(x = weight)) + geom_histogram(color = "darkgreen", fill = "white", bins = 15) + 
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5), panel.border = element_blank()) + 
  xlab("Weight (g)") + ylab("Number of burbot") +
  ggtitle("Weight distribution of burbot tagged at Carp Lake, 2022")

mean(carp_bb$weight, na.rm = TRUE) # 2341.667 g
median(carp_bb$weight, na.rm = TRUE) # 1825 g


# 2023 / COMPLETE data ---------------------------------------------------------------

bb_2023 <- read_csv("carp_lk/data/raw/carp_bb_capture_histories.csv")

bb_2023 <- rename(bb_2023, id = `ID`, floy_id = `Floy tag number`, date = `Date tagged`, 
                  rel_time = `Release Time`, trap = `Trap ID`, depth = `Trap depth (m)`,
                  reward = `Floy reward amount`, length = `Total length (cm)`, weight = `Weight (g)`,
                  rel_loc = `Release location`, comment = `Notes`, recap_date = `Recapture date`, 
                  rep_date = `Report date`, fate = `Fate`, recap_method = `Method`, rep_loc = `Reported location`,
                  rep_length = `Reported length`, rep_wt = `Reported weight`, photo = `Photo verification Y/N`,
                  rep_notes = `Return/report notes`)

lenth_23_hist <- 
  ggplot(bb_2023 %>%
           mutate(length_mm = (length*10)), aes(x = length_mm)) + 
  geom_histogram(color = "navy", fill = "lightblue", bins = 15) + 
  ylab("Number of burbot") + xlab("Length (mm)") + 
  theme_bw() +
  #theme(panel.border = element_blank(), panel.grid = element_blank()) + 
  ggtitle("Length of burbot tagged at Carp Lake, 2022-2023") + 
  scale_y_continuous(breaks = seq(0, 40, by = 5))

weight_23_hist <- bb_2023 %>%
  subset(!is.na(weight)) %>%
  ggplot(aes(x = weight)) + geom_histogram(color = "darkgreen", fill = "darkseagreen1", bins = 15) + 
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  scale_x_continuous(breaks = seq(0, 7000, by = 1000)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5)) + 
        #panel.border = element_blank(), panel.grid = element_blank()) + 
  xlab("Weight (g)") + ylab("Number of burbot") +
  ggtitle("Weight of burbot tagged at Carp Lake, 2022-2023")


# length vs weight scatterplot - COMPLETE 2022-2023 data --------------------------------------------

lw_taggedbb_23 <- bb_2023 %>%
  mutate(length_mm = (length*10)) %>%
  mutate(start_date = as.Date(date, format = '%m/%d/%y')) %>%
  mutate(season = case_when(start_date >= '2022-05-30' & start_date <= '2022-06-23' ~ 'Spring 2022',
                            start_date >= '2022-10-24' & start_date <= '2022-10-27' ~ 'Fall 2022',
                            start_date >= '2023-05-23' & start_date <= '2023-06-16' ~ 'Spring 2023')) %>%
  subset(is.na(season) == FALSE) %>%
  ggplot(aes(x = length_mm, y = weight, color = season)) +
  geom_smooth(method = "lm", se = TRUE, color = "darkseagreen1") +
  geom_point() +
  ggtitle("Length vs weight of tagged burbot, Carp Lake 2022-2023") +
  xlab("Length (mm)") +
  ylab("Weight (g)") +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(plot.title = element_text(size=10)) 

