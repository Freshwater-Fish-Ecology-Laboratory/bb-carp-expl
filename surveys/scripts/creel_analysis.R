# code to analyze all Carp Lake creel data together using statistical tests 
# Aug 2024

library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(stats)
library(gplots)
library(rstatix)

# read full creel data and format columns for variables we want to look at.

creeldata <- read_csv("carp_lk/data/creel/creeldata.csv") %>%
  mutate(day_type = factor(day_type, levels = c("weekend", "weekday")),
         bb_caught = as.numeric(`burbot caught`),
         cpue = as.numeric(cpue),
         fishing_freq = recode(`About how often do you fish for burbot at Carp Lake each year?2`, A = "once a year or less",
                               B = "between 2-5 times per year", C = "more than 5 times per year"),
         income = recode(`What is your estimated annual household income?`,
                         A = "under $15,000",
                         B = "between $15,000 and $29,999", C = "between $30,000 and $49,000",
                         D = "between $50,000 and $74,999", E = "between $75,000 and $99,999",
                         F = "between $100,000 and $150,000", G = "over $150,000", H = "I prefer not to say"),
         residence = recode(`About how long does it take you to drive to Carp Lake from place of residence?`,
                            A = "less than 1 hour",
                            B = "1-3 hours", C = "more than 3 hours",
                            D = "I prefer not to say"),
         age = recode(`Which age range do you belong to?`, 
                      A = "18-34 years old",
                      B = "35-49 years old", C = "50-64 years old",
                      D = "65-79 years old"),
         experience = recode(`How long have you been fishing for burbot at Carp Lake?`,
                             A = "1 year or less", B = "2-5 years", D = "10-20 years", E = "more than 20 years")) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue))

# Let's look at whether distance from Carp Lake is related to the time of week 
# (weekend vs weekday) that fishers visit

chisq.test(creeldata$fishing_freq, creeldata$residence, correct = FALSE) #significant??

freq_res <- table(creeldata$residence, creeldata$fishing_freq) 
freq_res %>% as.data.frame.matrix() %>% kbl() %>% kable_minimal()
mosaicplot(freq_res,
           color=TRUE)
fisher.test(creeldata$fishing_freq, creeldata$residence)

gender_edu <- table(creeldata$`What gender do you identify as? (one respondent per party)`, creeldata$`Highest level of education completed`)
mosaicplot(gender_edu,
           color=TRUE)

fisher.test(creeldata$`What gender do you identify as? (one respondent per party)`, creeldata$`Highest level of education completed`)

setlines <- creeldata %>%
  filter(Gear == "set-line")

mean(setlines$cpue)
sd(setlines$cpue)
# burbot caught by type of day - use a t-test for continuous variable (bb caught) vs 
# categorical variable (weekend/weekday)
t.test(cpue ~ day_type, data = creeldata) # no significant difference

bb_bydist <- aov(cpue ~ residence, data = creeldata)
summary(bb_bydist)
# not significant

bb_freq <- aov(cpue ~ fishing_freq, data = creeldata)
summary(bb_freq) # no signif difference

bb_age <- aov(cpue ~ age, data = creeldata)
summary(bb_age) # no signif difference

bb_exp <- aov(cpue~experience, data = creeldata)
summary(bb_exp) # no signif difference

creeldata %>%
  subset(!is.na(experience)) %>%
  ggplot(aes( x = experience, y = cpue)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0,0.3)

creeldata %>%
  subset(!is.na(fishing_freq)) %>%
  ggplot(aes( x = fishing_freq, y = cpue)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0,0.3)

creeldata %>%
  subset(!is.na(residence)) %>%
  ggplot(aes( x = residence, y = cpue)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0,0.3)

creeldata %>%
  subset(!is.na(day_type)) %>%
  ggplot(aes( x = day_type, y = cpue)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0,0.3)

creeldata %>%
  subset(!is.na(day_type)) %>%
  ggplot(aes( x = day_type, y = cpue)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0,0.3)
