# Processing Carp Lake creel survey, part C (general observations of burbot fishery)
# Nov 28, 2023
library(tidyverse)
carp_creel_C <- read_csv("carp_lk/data/creel/carp_creel_C.csv")%>%
  mutate(carp_exp = factor(`How long have you been fishing for burbot at Carp Lake?`, levels = c("one year or less", "2-5 years", "5-10 years", "10-20 years", "more than 20 years")))

carp_creel_C %>%
  subset(!is.na(carp_exp)) %>%
  ggplot(aes(x = carp_exp, y = prop.table((after_stat(count))), fill = carp_exp, label = scales::percent(prop.table(after_stat(count))))) + 
  geom_bar(aes(y=after_stat(count)/ sum(after_stat(count))*100)) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  theme_minimal()+
  xlab("") + ylab("Percent responses") +
  scale_x_discrete(labels = c("A" = "1 year or less", "B" = "2-5 years", "C" = "5-10 years", "D" = "10-20 years", "E" = "more than 20 years"))+
  ggtitle("Reported years of burbot fishing experience at Carp Lake") +
  scale_fill_brewer(palette = "GnBu", direction = -1)+
  scale_y_continuous(limits = c(0, 40), breaks=seq(0,40,10))+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

