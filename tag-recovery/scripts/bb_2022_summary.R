#Code to produce summary stats and plots for burbot tagged at Fraser Lake and Carp Lake, 2022
source("codes/processing/bb_2022_tag_processing.R")
#Median lengths and weights
median(fraser_bb$length, na.rm = TRUE)
median(carp_bb$length, na.rm = TRUE)
median(cluculz_bb$length, na.rm = TRUE)

median(fraser_bb$weight, na.rm = TRUE)
median(carp_bb$weight, na.rm = TRUE)
median(cluculz_bb$weight, na.rm = TRUE)

mean(fraser_bb$length, na.rm = TRUE)
mean(carp_bb$length, na.rm = TRUE)
mean(cluculz_bb$length, na.rm = TRUE)

mean(fraser_bb$weight, na.rm = TRUE)
mean(carp_bb$weight, na.rm = TRUE)
mean(cluculz_bb$weight, na.rm = TRUE)

#Some length-weight plots!

#Fraser lake
fraser_bb$length <- as.numeric(fraser_bb$length)
fraser_bb$weight <- as.numeric(fraser_bb$weight)

fraser_bb_plot <- fraser_bb %>%
  drop_na(length) %>%
  drop_na(weight) %>%
  ggplot(aes(x = length, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Length and weight of tagged Fraser Lake Burbot, 2022") +
  xlab("Length (cm)") +
  ylab("Weight (g)")

print(fraser_bb_plot)
#Histogram of burbot lengths at Fraser Lake:
fraser_bb %>%
  ggplot(aes(x = length)) +
  geom_histogram(binwidth=5,
                 colour = "black", fill = "white")+
  ggtitle("Length distribution of burbot tagged at Fraser Lake, 2022")

fraser_bb %>%
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth=150,
                 colour = "black", fill = "white")+
  ggtitle("Weight distribution of burbot tagged at Fraser Lake, 2022")

#Carp lake
carp_bb$length <- as.numeric(carp_bb$length)
carp_bb$weight <- as.numeric(carp_bb$weight)

carp_bb_plot <- carp_bb %>%
  drop_na(length) %>%
  drop_na(weight) %>%
  ggplot(aes(x = length, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Length and weight of tagged Carp Lake Burbot, 2022") +
  xlab("Length (cm)") +
  ylab("Weight (g)")

print(carp_bb_plot)

carp_bb %>%
  ggplot(aes(x = length)) +
  geom_histogram(binwidth=5,
                 colour = "black", fill = "white")+
  ggtitle("Length distribution of burbot tagged at Carp Lake, 2022")

carp_bb %>%
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth=150,
                 colour = "black", fill = "white")+
  ggtitle("Weight distribution of burbot tagged at Carp Lake, 2022")

#regression line
lm_lw_fraser <- lm(fraser_bb$weight ~ fraser_bb$length, data = fraser_bb)
summary(lm_lw_fraser)
lm_lw_carp <- lm(carp_bb$weight ~ carp_bb$length, data = carp_bb)
summary(lm_lw_carp)

#look at the depths at which our fish were trapped
#And the trap depth vs temperature (vs date...)
#Here's just the mean depth and temps:
mean(fraser_bb$depth)
mean(fraser_bb$temp, na.rm = TRUE)
mean(carp_bb$depth)
#we measured temperature at some trapping sites but it is not included in the tagging file
#included in trap set file
mean(carp_bb$temp, na.rm = TRUE)
#want to merge floy tag file with trap set file to get trapping info for each fish.

source("/Users/elh/Fraser Lake thesis database/bb-fraserlk-telemetry/codes/processing/traps_2022_processing.R")
#split data by lake
fraser_2022_sets <- filter(bb_traps_2022, bb_traps_2022$lake_id == "Fraser Lake")
carp_2022_sets <- filter(bb_traps_2022, bb_traps_2022$lake_id == "Carp Lake")
cluculz_2022_sets <- filter(bb_traps_2022, bb_traps_2022$lake_id == "Cluculz Lake")

start_dt <- as.POSIXct(bb_traps_2022$start_dt, format = "%m/%d/%y %H:%M")
end_dt <- as.POSIXct(bb_traps_2022$end_dt, format = "%m/%d/%y %H:%M")
trap_dur <- as.numeric(difftime(end_dt, start_dt))
#Fraser lake
fraser_2022_sets$start_dt
start_dt_fr <- as.POSIXct(fraser_2022_sets$start_dt, format = "%m/%d/%y %H:%M")

end_dt_fr <- as.POSIXct(fraser_2022_sets$end_dt, format = "%m/%d/%y %H:%M")
trap_dur_fr <- as.numeric(difftime(end_dt_fr, start_dt_fr))

#Carp lake
start_dt_carp <- as.POSIXct(carp_2022_sets$start_dt, format = "%m/%d/%y %H:%M")

end_dt_carp <- as.POSIXct(carp_2022_sets$end_dt, format = "%m/%d/%y %H:%M")
trap_dur_carp <- as.numeric(difftime(end_dt_carp, start_dt_carp))

#histogram of number of burbot captured per trap
ggplot(fraser_2022_sets, aes(x = bb_ct_fr)) + geom_histogram(binwidth = .5, fill = "lightblue")
ggplot(carp_2022_sets, aes(x = bb_ct_carp)) + geom_histogram(binwidth = .5, fill = "lightblue")

#plot trap duration vs number of BB trapped
bb_ct_fr <- fraser_2022_sets$bb_count
bb_ct_carp <- carp_2022_sets$bb_count

ggplot(fraser_2022_sets, aes(x = trap_dur_fr, y = bb_ct_fr)) + geom_point()
ggplot(carp_2022_sets, aes(x = trap_dur_carp, y = bb_ct_carp)) + geom_point()

#Catch-Per-Unit-Effort!
bb_traps <- filter(bb_traps_2022, bb_count >= 1)
bb_weight <- as.numeric(bb_traps$weight, na.rm = TRUE)
bb_traps_full <- bb_traps[!is.na(bb_weight),]
bb_weight <- as.numeric(bb_traps_full$weight)
trap_dur <- as.numeric(trap_dur, na.rm = TRUE)
#now we have just the trap sets which resulted in a burbot catch, and where the weight was recorded.

cpue_bb <- bb_traps_full %>%
  mutate(weight_kg = as.numeric(bb_weight / 1000)) %>%
  mutate(start_dt = as.POSIXct(start_dt, format = "%m/%d/%y %H:%M")) %>%
  mutate(end_dt = as.POSIXct(end_dt, format = "%m/%d/%y %H:%M")) %>%
  mutate(trap_dur = as.numeric(difftime(end_dt, start_dt), na.rm = TRUE)) %>%
  group_by(lake_id, trap_id) %>%
  summarize(cpue = sum(weight_kg) / mean(trap_dur)) %>%
  group_by(lake_id) %>%
  summarize(median_cpue_kg_hr = median(cpue))

cpue_bb

#high and low reward tags sample sizes
highreward_fr <- filter(bb_tagging_2022, bb_tagging_2022$floy_type == "100" & bb_tagging_2022$lake_id == "Fraser Lake")
fr_highreward_bb <- nrow(highreward_fr)
fr_highreward_bb

highreward_carp <- filter(bb_tagging_2022, bb_tagging_2022$floy_type == "100" & bb_tagging_2022$lake_id == "Carp Lake")
carp_highreward_bb <- nrow(highreward_carp)
carp_highreward_bb

standard_fr <- filter(bb_tagging_2022, bb_tagging_2022$floy_type == "0" & bb_tagging_2022$lake_id == "Fraser Lake")
fr_standard_bb <- nrow(standard_fr)
fr_standard_bb

standard_carp <- filter(bb_tagging_2022, bb_tagging_2022$floy_type == "0" & bb_tagging_2022$lake_id == "Carp Lake")
carp_standard_bb <- nrow(standard_carp)
carp_standard_bb

#temperature vs date scatterplot
ggplot(fraser_bb, aes(x = fraser_bb$tag_date, y = fraser_bb$temp)) + geom_point() + 
  ggtitle("Fraser Lake: water temperature at trap depth by Date") +
  xlab("Date") + ylab("Water temperature (C)")

#Temperature vs depth scatterplot
ggplot(fraser_bb, aes(x = fraser_bb$depth, y = fraser_bb$temp)) + geom_point() + 
  ggtitle("Fraser Lake: water temperature and trap depth") +
  xlab("Trap depth (m)") + ylab("Water temperature (C)")


