
library(tidyverse)
library(gt)

carp_creel_D <- read_csv("carp_lk/data/creel/carp_creel_D.csv")

carp_creel_D <- carp_creel_D %>%
  rename(q_age = `Which age range do you belong to?`,
         q_dist =  `About how long does it take you to drive to Carp Lake from place of residence?`,
         q_indg = `Are you a member of any of the following Indigenous communities?`,
         q_gender = `What gender do you identify as? (one respondent per party)`,
         q_edu = `Highest level of education completed`,
         q_income = `What is your estimated annual household income?`)

# Calculate percentages of participants giving each response:

# gt tables for each demographic question: 

all_age_levels <- c("A","B","C","D","E","F")

age_percentages <- carp_creel_D %>%
  group_by(q_age) %>%
  summarise(Number = n(), .groups="drop") %>%
  complete(q_age = all_age_levels, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    Number = ifelse(Number == 0, NA, Number),
    q_age = recode(q_age,
                   "A" = "18-34 years old",
                   "B" = "35-49 years old",
                   "C" = "50-64 years old",
                   "D" = "65-79 years old",
                   "E" = "80 years or older",
                   "F" = "I prefer not to say"),
    q_age = factor(q_age, 
                   levels = c("18-34 years old", 
                              "35-49 years old", 
                              "50-64 years old", 
                              "65-79 years old", 
                              "80 years or older", 
                              "I prefer not to say")),
    Number = replace_na(Number, 0)) %>%
  rename("Age Range" = q_age) %>%
  arrange(`Age Range`)   # Sort by the factor order

gt(age_percentages) |> 
  fmt_number(decimals = 1)



# Distance from place of residence
all_distances <- c("A","B","C","D")

dist_percentages <- carp_creel_D %>%
  group_by(q_dist) %>%
  summarise(Number = n(),
            .groups="drop") %>%
  complete(q_dist = all_distances, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    Number = ifelse(Number == 0, NA, Number),  # Replace 0 with NA
    q_dist = recode(q_dist,
                    "A" = "less than 1 hour", "B" = "1-3 hours", 
                    "C" = "more than 3 hours", "D" = "I prefer not to say"),
    q_dist = factor(q_dist, 
                   levels = c("less than 1 hour", 
                              "1-3 hours", 
                              "more than 3 hours", 
                              "I prefer not to say")),
    Number = replace_na(Number, 0)) %>%
  rename("Distance from Home" = q_dist) %>%
  arrange(`Distance from Home`)

gt(dist_percentages) |>
  fmt_number(decimals = 1)

# Member of Indigenous community?
all_indg <- c("A","B","C","D","E","F")

indg_percentages <- carp_creel_D %>%
  group_by(q_indg) %>%
  summarise(Number = n(),
            .groups="drop") %>%
  complete(q_indg = all_indg, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    Number = ifelse(Number == 0, NA, Number),  # Replace 0 with NA
    q_indg = recode(q_indg,
                    "A" = "First Nation", "B" = "Inuit", "C" = "Métis", "D" = "Other",
                    "E" = "Non-Indigenous", "F" = "I prefer not to say"),
    q_indg = ifelse(is.na(q_indg), "Did not answer", q_indg),
    q_indg = factor(q_indg,
                    levels = c("First Nation", "Inuit", "Métis","Other",
                               "Non-Indigenous", "I prefer not to say", "Did not answer")),
    Number = replace_na(Number, 0)) %>%
  rename("Indigenous Community" = q_indg) %>%
  arrange(`Indigenous Community`)

gt(indg_percentages) |>
  fmt_number(decimals = 1)

# Gender
all_gender <- c("A", "B", "C","D","E","F", "G")

gend_percentages <- carp_creel_D %>%
  group_by(q_gender) %>%
  summarise(Number = n(),
            .groups="drop") %>%
  complete(q_gender = all_gender, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    # Number = ifelse(Number == 0, NA, Number),  # Replace 0 with NA
    q_gender = recode(q_gender,
                      "A" = "Female", "B" = "Male", "C" = "Non-binary", "D" = "Transgender", 
                      "E" = "Two-Spirit", "F" = "I prefer not to say", "G" = "Other"),
    q_gender = ifelse(is.na(q_gender), "Did not answer", q_gender),
    Number = replace_na(Number, 0)# Add label for NA
  ) %>%
  rename("Gender" = q_gender) %>%
  arrange(desc(Percentage))


gt(gend_percentages) |>
  fmt_number(decimals = 1)

# Level of education completed
all_edu <- c("A", "B", "C","D","E","F", "G", "H")

edu_percentages <- carp_creel_D %>%
  group_by(q_edu) %>%
  summarise(Number = n(),
            .groups="drop") %>%
  complete(q_edu = all_edu, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    # Number = ifelse(Number == 0, NA, Number),
    q_edu=recode(q_edu, "A" = "some high school", "B" = "high school diploma", "C" = "some university or college or trade school",
                      "D" = "bachelor's degree or college diploma or trades certificate", "E" = "Master's degree",
                      "F" = "some graduate school", "G" = "Ph.D/doctoral degree or equivalent", "H" = "I prefer not to say"),
         q_edu = factor(q_edu, levels = c("some high school", "high school diploma", "some university or college or trade school",
                                          "bachelor's degree or college diploma or trades certificate", "Master's degree",
                                          "some graduate school", "Ph.D/doctoral degree or equivalent", "I prefer not to say"))) %>%
  rename("Education Level Completed" = q_edu) %>%
  arrange(`Education Level Completed`)

gt(edu_percentages) |>
  fmt_number(decimals = 1)

# Income
all_inc <- c("A", "B", "C","D","E","F", "G", "H")

inc_percentages <- carp_creel_D %>%
  group_by(q_income) %>%
  summarise(Number = n(),
            .groups="drop") %>%
  complete(q_income = all_inc, fill = list(Number = 0)) %>%
  mutate(
    Percentage = Number / nrow(carp_creel_D) * 100,
    q_income=recode(q_income, "A" = "Under $15,000", "B" = "between $15,000 and $29,999", "C" = "$30,000 - $49,000",
                      "D" = "$50,000 - $74,999", "E" = "$75,000 - $99,999",
                      "F" = "$100,000 - $150,000", "G" = "over $150,000", "H" = "I prefer not to say"),
    q_income = factor(q_income, levels = c("Under $15,000", "between $15,000 and $29,999", "$30,000 - $49,000",
                                          "$50,000 - $74,999", "$75,000 - $99,999",
                                            "$100,000 - $150,000", "over $150,000", "I prefer not to say"))) %>%
  rename("Annual Household Income" = q_income) %>%
  arrange(`Annual Household Income`)

gt(inc_percentages) |>
  fmt_number(decimals = 1)

#### PLOTS
# 1.  Age
carp_creel_D %>%
  ggplot(aes(x = q_age, y = prop.table((after_stat(count))), fill = q_age, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "18-34 years", "B" = "35-49 years", "C" = "50-64 years", "D" = "65-79 years", "F"= "I prefer not to say"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Ages of creel survey participants", y = "Percentage", x = "Age group")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# 2. Place of residence

carp_creel_D %>%
  ggplot(aes(x = q_dist, y = prop.table((after_stat(count))), fill = q_dist, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "less than 1 hour", "B" = "1-3 hours", "C" = "more than 3 hours", "D" = "I prefer not to say"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Creel participants' distance from home to Carp Lake", y = "Percentage", x = "Distance (hours of driving)")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# 3. Indigenous communities

carp_creel_D %>%
  subset(!is.na(q_indg)) %>%
  ggplot(aes(x = q_indg, y = prop.table((after_stat(count))), fill = q_indg, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "First Nation", "B" = "Inuit", "C" = "Métis", "D" = "Other", "E" = "None", "F" = "I prefer not to say"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Creel participants' membership in Indigenous communities", y = "Percentage", x = "Indigenous status")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# 4. Gender 

carp_creel_D %>%
  subset(!is.na(q_gender)) %>%
  ggplot(aes(x = q_gender, y = prop.table((after_stat(count))), fill = q_gender, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "Female", "B" = "Male"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Gender of creel survey participants", y = "Percentage", x = "Gender")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# 5. Education
carp_creel_D %>%
  subset(!is.na(q_edu)) %>%
  ggplot(aes(x = q_edu, y = prop.table((after_stat(count))), fill = q_edu, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "some high school", "B" = "high school diploma", "C" = "some university or \n college or trade school",
                              "D" = "bachelor's degree or \n college diploma or \n trades certificate", "E" = "Master's degree", "F" = "some graduate school",
                              "G" = "Ph.D/doctoral degree \n or equivalent", "H" = "I prefer not \n to say"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Education level completed by creel survey participants", y = "Percentage", x = "Education level completed")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        legend.position = "none")

# 6. Household income

carp_creel_D %>%
  subset(!is.na(q_income)) %>%
  ggplot(aes(x = q_income, y = prop.table((after_stat(count))), fill = q_income, label = scales::percent(prop.table(after_stat(count))))) +
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count))))+
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, size = 3) + 
  scale_x_discrete(labels = c("A" = "under $15,000", "B" = "$15,000 - $29,999", "C" = "$30,000 - $49,000",
                              "D" = "$50,000 - $74,999", "E" = "$75,000 - $99,999", "F" = "$100,000 - $150,000",
                              "G" = "over $150,000", "H" = "I prefer not \n to say"))+
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  theme_minimal() +
  labs(title = "Annual household income of creel survey participants", y = "Percentage", x = "Annual household income range")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        legend.position = "none")

library(pwr)
pwr.anova.test(k = 5, f = 0.25, sig.level = 0.05, power = 0.8)



