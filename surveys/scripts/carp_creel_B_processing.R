# Code to load and process creel data for Part B
library(tidyverse)
library(purrr)
library(RColorBrewer)

carp_creel_B <- read_csv("carp_lk/data/creel/carp_creel_B.csv")
carp_creel_B <- carp_creel_B %>%
  rename(fishing_freq = `About how often do you fish for burbot at Carp Lake each year?2`,
         aware = `Before we began this survey, were you aware there is a burbot-tagging program happening at Carp Lake?`,
         typ_set = `At what time of day do you typically set your line?`,
         typ_check = `At what time of day do you typically check your line?`)

# let's look at the number of times these anglers fish at Carp Lake per year:

carp_creel_B %>%
  ggplot(aes(x = fishing_freq, y = prop.table((after_stat(count))), fill = fishing_freq, label = scales::percent(prop.table(after_stat(count))))) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))*100)) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  theme_minimal()+
  xlab("") + ylab("Percent responses") +
  scale_x_discrete(labels = c("A" = "once a year or less", "B" = "2-5 times / year", "C" = "more than 5 times / year"))+
  ggtitle("Participants' reported burbot fishing trip frequency") +
  scale_fill_brewer(palette = "GnBu", direction = -1)+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 70), breaks=seq(0,70,10))


freq_percentages <- carp_creel_B %>%
  group_by(fishing_freq) %>%
  summarise(percentage = n() / nrow(carp_creel_B) * 100)
print(freq_percentages)

# Were anglers aware of the tagging program before beginning the survey?

carp_creel_B <- carp_creel_B %>%
  mutate(
    aware_yn = case_when(
      aware == "Yes" | aware == "Yes (saw poster)" | aware == "Yes (from other people)" ~ "Yes",
      aware == "No" ~ "No")
  )

aware_percentages <- carp_creel_B %>%
  group_by(aware_yn) %>%
  summarise(percentage = n() / nrow(carp_creel_B) * 100)
print(aware_percentages)


carp_creel_B %>%
  ggplot(aes(x = aware_yn, y = prop.table((after_stat(count))), fill = aware_yn, label = scales::percent(prop.table(after_stat(count)))))+
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))))+
  theme_minimal() +
  xlab("") + ylab("Percentage") +
  scale_x_discrete(labels = c("No" = "Not previously aware of \n tagging study", "Yes" = "Previously aware of tagging study"))+
  scale_fill_brewer(palette = "GnBu", name = "Aware of burbot tagging study before survey?")+
  ggtitle("Fishers' awareness about the Carp Lake \n burbot study prior to starting survey") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))



#######################################################
# Likelihood to report reward tags and non-reward tags!
# let's load a dataframe of just the tag-reporting likelihood questions:
# I put this into long format with a column for the percentage giving each response to each question

library(viridis)
lik_questions <- read_csv("carp_lk/data/creel/lik_questions.csv")%>%
  mutate(percent = per*100)

response1 <- factor(lik_questions$response, levels = c("very likely", "likely", "neutral", "unlikely", "very unlikely"))

# Now to create a plot:
ggplot(data = lik_questions, aes(x = question, y = percent, fill = response1)) +
  geom_bar(stat = "identity", width = 0.7) +
  # scale_fill_brewer(palette = "GnBu", direction = -1, name = "Response") +
  scale_fill_viridis_d(name = "Response") +
  coord_flip() +
  xlab("") +
  ylab("percentage") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("Survey participants' reported likelihood to report tagged burbot") +
  theme_minimal()+
  theme(panel.grid.major = element_line(colour = "grey50"))+
  theme(panel.grid.minor = element_line(colour = "grey50"))


#######################################################

# Typical set and pull times for set-lining:
# group into the same time blocks used for Part A fishing start and end times...
carp_creel_B$typ_set
carp_creel_B <- carp_creel_B %>%
  mutate(
    typ_set_cat = case_when(
      typ_set == "19:00" | typ_set == "night" | typ_set == "17:00" | 
        typ_set == "20:30" | typ_set == "evening" | typ_set == "22:00" | typ_set == "21:00-22:00" | 
        typ_set == "overnight (around 17:30)" | typ_set == "20:00" | typ_set == "18:00" | 
        typ_set == "end of evening" | typ_set == "night or after checking" | typ_set == "19:30" ~ "evening",
      
      typ_set == "10:00" | typ_set == "6:00" ~ "morning",
      
      typ_set == "16:00" | typ_set == "all day" | typ_set == "15:00" |
        typ_set == "after checking the last line (~13:30)" ~ "midday",
      
      typ_set == "11:00 or 19:00" | typ_set == "during day/overnight" ~ "either midday or at night"
    )
  )

typ_stplot <- carp_creel_B %>%
  drop_na(typ_set) %>%
  ggplot(aes(x = typ_set_cat, y = "", fill = typ_set_cat)) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count)))) + 
  theme_minimal() +
  ggtitle("Typical burbot fishing start times") +
  xlab("") + ylab("percent responses")
  # + scale_fill_brewer(palette = "GnBu", name = "Time interval", 
  #                  labels = c("Evening (17:00-midnight)", "Midday (11:00-17:00)", "Morning (midnight-11:00)",
   #                            "Either midday or at night"))
# fix these labels and make colour scheme nice

typst_percentages <- carp_creel_B %>%
  filter(!is.na(typ_set_cat))

typst_percentages_1 <- typst_percentages %>%
  group_by(typ_set_cat) %>%
  summarise(percentage = n() / (nrow(carp_creel_B)-5) * 100, number = n())
print(typst_percentages_1)

carp_creel_B$typ_check
carp_creel_B <- carp_creel_B %>%
  mutate(
    typ_check_cat = case_when(
      typ_check == "7:00" | typ_check == "10:00" | typ_check == "morning" |
        typ_check == "9:00" | typ_check == "first thing in the morning" |
        typ_check == "morning" | typ_check == "AM" | typ_check == "morning (around 10:30)" |
        typ_check == "8:00" | typ_check == "08:00-10:00" | typ_check == "early morning" ~ "morning",
       
      typ_check == "21:00" | typ_check == "19:00" ~ "evening",
      
      typ_check == "11:00 or 19:00" | typ_check ==  "morning and evening" |
        typ_check == "morning/evening" | typ_check == "night/ again next day" ~ "both morning and evening"
    )
  )

typ_checkplot <- carp_creel_B %>%
  drop_na(typ_check) %>%
  ggplot(aes(x = typ_check_cat, y = "", fill = typ_check_cat)) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count)))) + 
  theme_minimal() +
  ggtitle("Typical burbot fishing check/pull times") +
  xlab("") + ylab("percent responses")
# + scale_fill_brewer(palette = "GnBu", name = "Time interval", 
#                  labels = c("Evening (17:00-midnight)", "Midday (11:00-17:00)", "Morning (midnight-11:00)",
#                            "Either midday or at night"))

typend_percentages <- carp_creel_B %>%
  group_by(typ_check_cat) %>%
  summarise(percentage = n() / nrow(carp_creel_B) * 100)
print(typend_percentages)


alt_responses <- read.csv("carp_lk/data/creel/alt_responses.csv")
names(alt_responses) <- c("Alt. methods of reporting tags - response", "Number",
                 "Alt. incentives to report tags - response", "Number",
                 "No suggestions - response", "Number")

gt(alt_responses)

df <- tibble(
  `Alternative methods of reporting tags - response` = c("online", "email", "tag drop-box", "mobile app", "compulsory reporting"),
  `Alternative methods of reporting tags - number` = c(7, 2, 2, 1, 1),
  `Alternative incentives to report tags - response` = c("different incentives", "clearer regulations", "more advertisement of study", NA, NA),
  `Alternative incentives to report tags - number` = c(4, 1, 1, NA, NA),
  `No suggestions - response` = c("NA (did not answer)", "none", NA, NA, NA),
  `No suggestions - number` = c(8, 7, NA, NA, NA)
)

df_clean <- df %>%
  mutate(across(where(is.character), ~replace_na(.x, ""))) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, NA_real_)))  # keep numeric NAs

df_clean %>%
  gt() %>%
  fmt_missing(columns = everything(), missing_text = "") %>%  # blanks for all NA
  tab_spanner(
    label = md("**Alternative methods of reporting tags**"),
    columns = c(1, 2)
  ) %>%
  tab_spanner(
    label = md("**Alternative incentives to report tags**"),
    columns = c(3, 4)
  ) %>%
  tab_spanner(
    label = md("**No suggestions**"),
    columns = c(5, 6)
  ) %>%
  cols_label(
    `Alternative methods of reporting tags - response` = md("*category*"),
    `Alternative methods of reporting tags - number` = md("*responses*"),
    `Alternative incentives to report tags - response` = md("*category*"),
    `Alternative incentives to report tags - number` = md("*responses*"),
    `No suggestions - response` = md("*category*"),
    `No suggestions - number` = md("*responses*")
  ) 

# concerns table
obs <- read.csv("carp_lk/data/creel/observations.csv")

gt(obs)
obs <- tibble(
  `Concerns about burbot fishery - response` = c("none", "overharvest or noncompliance with regulations", "fish health", "low or decreasing catch rates"),
  `Concerns about burbot fishery - number` = c(24, 5, 2, 1),
  `Observed changes in burbot / Carp Lake - response` = c("none", "fish size", "fish health", "seasonal changes"),
  `Observed changes in burbot / Carp Lake - number` = c(23, 3, 1, 1)
)

obs_clean <- obs %>%
  mutate(across(where(is.character), ~replace_na(.x, ""))) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, NA_real_)))  # keep numeric NAs

obs_clean %>%
  gt() %>%
  fmt_missing(columns = everything(), missing_text = "") %>%  # blanks for all NA
  tab_spanner(
    label = md("**Concerns about burbot fishery**"),
    columns = c(1, 2)
  ) %>%
  tab_spanner(
    label = md("**Observed changes in burbot / Carp Lake**"),
    columns = c(3, 4)
  ) %>%
  cols_label(
    `Concerns about burbot fishery - response` = md("*category*"),
    `Concerns about burbot fishery - number` = md("*responses*"),
    `Observed changes in burbot / Carp Lake - response` = md("*category*"),
    `Observed changes in burbot / Carp Lake - number` = md("*responses*")) 
