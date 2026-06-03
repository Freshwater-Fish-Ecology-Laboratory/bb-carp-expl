library(tidyverse)
library(gridExtra)
carp_creel_A <- read_csv("carp_lk/data/creel/carp_creel_A.csv")

colnames(carp_creel_A) <- c("survey_number", "date", "time", "day_type", "angler_id", 
                            "party_size", "fishing", "start_time", "end_time", "sets", "hrs_fished",
                            "bb_fishing", "bb_caught", "cpue", "bb_kept", "bb_released", "tagged_bb", 
                            "sampled", "gear", "bait", "bait_1", "bait_2", "approx_loc", "loc_code", "comment")

#######################################################
# start and end times for burbot fishing

# categorize the start times into 3 main intervals:
# morning (before 11am)
# evening (after 5pm)
# lunchtime/ mid day (between 11am and 5pm)

carp_creel_setlines <- carp_creel_A %>%
  filter(gear == "set-line") %>%
  mutate(setline_CPUE = as.numeric(bb_caught)/as.numeric(sets))

mean(carp_creel_setlines$setline_CPUE, na.rm = TRUE)

carp_creel_A <- carp_creel_A %>%
  subset(!is.na(survey_number)) %>%
  mutate(cpue = as.numeric(cpue)) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue)) %>%
  mutate(
    start_cat = case_when(
      start_time == "\"last night\"" | start_time == "\"nighttime\"" | start_time == "5:00:00 PM THURS" | 
        start_time == "22:00" | start_time == "20:30" | start_time == "21:00" | start_time == "20:00" | 
        start_time == "19:30" | start_time == "19:00" | start_time == "21:00-22:00" | 
        start_time == "17:30" | start_time == "18:00" | start_time == "19:30, 08:00" | start_time == "7/18/23" ~ "evening",
      start_time == "6:00" | start_time == "9:00" | start_time == "10:00:00 AM" | start_time == "7:00" |
        start_time == "10:00" | start_time == "7:30" ~ "morning",
      start_time == "12:00" | start_time == "16:00" | start_time == "13:00" | start_time == "15:00" |
        start_time == "11:00" | start_time == "16:30" ~ "midday"
    )
  ) %>%
  mutate(start_cat = factor(start_cat, levels = c("morning", "midday", "evening")))

mean(carp_creel_A$cpue)
# bar chart
stplot <- carp_creel_A %>%
  drop_na(start_cat) %>%
  ggplot(aes(x = start_cat, y = "", fill = start_cat)) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))*100)) + 
  theme_minimal() +
  ggtitle("Burbot fishing start times") +
  xlab("") + ylab("Responses (%)") +
  scale_fill_brewer(palette = "GnBu", direction = -1, name = "Time interval", 
                    labels = c("Morning (0:00-11:00)", "Midday (11:00-17:00)", "Evening (17:00-0:00)"))

# calc percentages
per_morn <- sum(carp_creel_A$start_cat == "morning", na.rm = TRUE) / sum(carp_creel_A$start_cat == "morning" | carp_creel_A$start_cat == "evening" | carp_creel_A$start_cat == "midday", na.rm = TRUE)
per_eve <- sum(carp_creel_A$start_cat == "evening", na.rm = TRUE) / sum(carp_creel_A$start_cat == "morning" | carp_creel_A$start_cat == "evening" | carp_creel_A$start_cat == "midday", na.rm = TRUE)
per_midd <- sum(carp_creel_A$start_cat == "midday", na.rm = TRUE) / sum(carp_creel_A$start_cat == "morning" | carp_creel_A$start_cat == "evening" | carp_creel_A$start_cat == "midday", na.rm = TRUE)

# Now look at end times
carp_creel_A <- carp_creel_A %>%
  mutate(
    end_cat = case_when(
      end_time == "\"morning\"" | end_time == "10:00" | end_time == "09:00 SUN" | end_time == "8:30" |
        end_time == "8:00" | end_time == "10:00" | end_time == "morning" | end_time == "9:30" | end_time ==
        "9:45" | end_time == "10:45" | end_time == "09:00-09:30" | end_time == "10:30" | end_time == "checked evening, then again morning at 6:45" |
        end_time == "08:00, 19:30" | end_time == "7/21/23 8:00" | end_time == "9:00" ~ "morning",
      end_time == "12:00" | end_time == "15:00" | end_time == "11:30" | end_time == "11:00" | end_time == "14:45" ~ "midday",
      end_time == "20:00" | end_time == "checked evening, then again morning at 6:45" | end_time == "21:40" |
        end_time == "08:00, 19:30" ~ "evening"
    )
  ) %>%
  mutate(end_cat = factor(end_cat, levels = c("morning", "midday", "evening")))


endplot <- carp_creel_A %>%
  drop_na(end_cat) %>%
  ggplot(aes(x = end_cat, y = "", fill = end_cat)) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))*100)) + 
  theme_minimal() +
  xlab("") + ylab("Responses (%)") +
  ggtitle("Burbot fishing end times") +
  scale_fill_brewer(palette = "GnBu", direction = -1, name = "Time interval", 
                    labels = c("Morning (0:00-11:00)", "Midday (11:00-17:00)", "Evening (17:00-0:00)"))

sum(carp_creel_A$end_cat == "morning", na.rm = TRUE) / sum(carp_creel_A$end_cat == "morning" | carp_creel_A$end_cat == "evening" | carp_creel_A$end_cat == "midday", na.rm = TRUE)
sum(carp_creel_A$end_cat == "evening", na.rm = TRUE) / sum(carp_creel_A$end_cat == "morning" | carp_creel_A$end_cat == "evening" | carp_creel_A$end_cat == "midday", na.rm = TRUE)
sum(carp_creel_A$end_cat == "midday", na.rm = TRUE) / sum(carp_creel_A$end_cat == "morning" | carp_creel_A$end_cat == "evening" | carp_creel_A$end_cat == "midday", na.rm = TRUE)


grid.arrange(stplot, endplot)
#######################################################

# Some stats on burbot catches
sum(carp_creel_A$bb_caught)
sum(carp_creel_A$bb_kept)
sum(carp_creel_A$bb_released)
sum(carp_creel_A$tagged_bb)

# Total burbot caught (and observed) during creels, proportion of caught burbot that were harvested, 
# proportion of caught burbot that were released
(creel_bb_caught <- sum(as.numeric(carp_creel_A$bb_caught)))
(harvest_rate <- sum(carp_creel_A$bb_kept) / sum(carp_creel_A$bb_caught))

(release_rate <- sum(carp_creel_A$bb_released) / sum(carp_creel_A$bb_caught))

# Proportion of creel burbot caught which had a tag:
(tagged_bb_prop <- sum(carp_creel_A$tagged_bb) / creel_bb_caught)
#######################################################

# Fishing methods
carp_creel_A <- carp_creel_A %>%
  mutate(
    fish_method = factor(case_when(
      gear == "set-line" ~ "set-line",
      gear == "jigging" ~ "jigging",
      gear == "set-line + jigging 3h" ~ "both set-line and jigging"),
      levels = c("set-line", "jigging", "both set-line and jigging")),
    cpue = as.numeric(cpue))
    
carp_creel_A %>%
  subset(!is.na(fish_method)) %>%
  ggplot(aes(x = fish_method, y = prop.table((after_stat(count))), fill = fish_method, label = scales::percent(prop.table(after_stat(count))))) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  ggtitle("Burbot fishing methods used at Carp Lake") +
  theme_minimal() +
  xlab("Fishing method") + ylab("Percentage") +
  scale_fill_brewer(palette = "GnBu", direction = -1, name = "Method") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
    

per_setline <- sum(carp_creel_A$fish_method == "set-line") / sum(carp_creel_A$fish_method == "set-line"| carp_creel_A$fish_method == "jigging" | carp_creel_A$fish_method
                                                          == "both set-line and jigging") *100
per_jig <- sum(carp_creel_A$fish_method == "jigging") / sum(carp_creel_A$fish_method == "set-line"| carp_creel_A$fish_method == "jigging" | carp_creel_A$fish_method
                                                     == "both set-line and jigging") * 100
per_both <- sum(carp_creel_A$fish_method == "both set-line and jigging") / sum(carp_creel_A$fish_method == "set-line"| carp_creel_A$fish_method == "jigging" | carp_creel_A$fish_method
                                                                               == "both set-line and jigging") * 100
#######################################################
# Types of bait used -
# rainbow trout
# chicken
# shrimp/shellfish
# worms
# other fish

carp_creel_A <- carp_creel_A %>%
  mutate(
    bait_cat = factor(case_when(
      bait == "\"fish head\"" | bait == "\"trout\"" | bait == "\"rainbow trout head\"" | bait == "\"rainbow trout heads\"" |
        bait == "\"trout head\"" | bait == "\"rainbow trout parts\"" | bait == "\"chicken, fish head\"" | bait == "\"rainbow trout head and pike minnow body\"" |
        bait == "\"trout heads\"" | bait == "\"rainbow trout heads\"" | bait == "\"rainbow trout\"" | bait == "\"rainbow trout heads and/or tails, guts\"" ~ "rainbow trout",
      bait == "\"whitefish\"" | bait == "\"northern pike & salmon eggs\"" | bait == "\"peamouth chub\"" | bait == "\"pike minnow\"" |
        bait == "\"rainbow trout head and pike minnow body\"" | bait == "\"shrimp and sardines\"" ~ "other fish",
      bait == "\"shrimp and sardines\"" | bait == "\"shrimps, prawn\"" | bait == "\"shrimp\"" ~ "shrimp/shellfish",
      bait == "\"chicken gizzards\"" | bait == "\"chicken, fish head\"" ~ "chicken*",
      bait == "\"worms\"" ~ "worms*",
      bait == "\"glow jig and tube\"" ~ "jigging lures*"), levels =  c("rainbow trout", "other fish", "shrimp/shellfish", "chicken*", "worms*", "jigging lures*")))


carp_creel_A %>%
  subset(!is.na(bait_cat)) %>%
  ggplot(aes(x = bait_cat, y = prop.table((after_stat(count))), fill = bait_cat, label = scales::percent(prop.table(after_stat(count))))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))*100))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  ggtitle("Bait used by burbot fishers surveyed at Carp Lake") +
  scale_fill_brewer(palette = "GnBu", direction = -1, name = "Bait category") + 
  theme_minimal() +
  xlab("Bait type") +
  ylab("Percentage") +
  scale_y_continuous(limits = c(0, 70), breaks=seq(0,70,10))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5))


########################################################
# Calculating total catches and total effort hours!
carp_creel_A$bb_caught <- as.factor(carp_creel_A$bb_caught)

carp_creel_A %>%
  subset(!is.na(bb_caught)) %>%
  ggplot(aes(x = bb_caught, y = prop.table((after_stat(count))), fill = bb_caught, label = scales::percent(prop.table(after_stat(count))))) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  ggtitle("Number of burbot caught by fishing parties surveyed at Carp Lake") +
  theme_minimal() +
  xlab("Number of burbot") + ylab("Percentage") +
  scale_fill_brewer(palette = "GnBu", direction = -1, name = "Number of burbot") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

########################################################
# Stat tests to look at:
# (1) CPUE by fishing method
# (2) CPUE by bait category
# (3) CPUE by start time category and end time category

carp_setline_data <- carp_creel_A %>%
  filter(fish_method == "set-line")%>%
  mutate(bb_caught = as.numeric(bb_caught),
         sets = as.numeric(sets)) %>%
  mutate(cpset = (bb_caught/sets))
  
mean(carp_setline_data$cpset, na.rm = TRUE)
sd(carp_setline_data$cpset, na.rm = TRUE)

bait_all <- carp_creel_A %>%
  subset(!is.na(bait_cat)) %>%
 # filter(fish_method == "set-line") %>%
  ggplot(aes(x = bait_cat, y = cpue, fill = bait_cat)) + 
  geom_boxplot()+
  ylab("CPUE (burbot per hour of fishing)")+
  xlab("Bait category")+
  theme_minimal()+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
        legend.position = "none") +
  ggtitle("Including both set-line and jigging data")


bait_setl <- carp_creel_A %>%
  subset(!is.na(bait_cat)) %>%
  filter(fish_method == "set-line") %>%
  ggplot(aes(x = bait_cat, y = cpue, fill = bait_cat)) + 
  geom_boxplot()+
  ylab("")+
  xlab("Bait category")+
  theme_minimal()+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
        legend.position = "none")+
  ggtitle("Set-line data only") 


grid.arrange(bait_all, bait_setl, nrow = 1)



# Analysis - ANOVA cpue by capture method:
method_lm <- lm(cpue ~ fish_method, data = carp_creel_A)
method_anova <- aov(method_lm)
summary(method_anova)

aov.model <- aov(cpue ~ fish_method, data = carp_creel_A)
summary(aov.model)

tukey_method <- TukeyHSD(aov.model)
tukey_method
# jigging has a mean CPUE 1.67 higher than mean CPUE of set-lines


# Analysis - ANOVA cpue by bait category
bait_lm <- lm(cpue ~ bait_cat, data = carp_setline_data)
bait_anova <- aov(bait_lm)
summary(bait_anova)
tukey_bait <- TukeyHSD(bait_anova)
tukey_bait
plot(tukey_bait)

# Significant difference in **CPUE** by fishing method, but not significant difference in **number of 
# burbot caught** by fishing method. Suggests difference in CPUE is due to difference in fishing 
# duration rather than number of catches

carp_creel_A %>%
  subset(!is.na(fish_method)) %>%
  ggplot(aes(x = fish_method, y = cpue)) +
  geom_boxplot() +
  theme_minimal()

# dates of surveys

surveydays <- read.csv("carp_lk/data/surveydays.csv")
surveydays %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  ggplot(aes(x = Date, y = number.of.surveys)) +
  geom_col(fill = "#238A8DFF") +
  theme_minimal() +
  ylab("Number of Surveys Completed")+
  ggtitle("Burbot fisher knowledge surveys completed by date, Carp Lake 2023") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))

