#Code to import and process 2007 sampler's effort from Cluculz, Eaglet, Norman, Saxton and Nukko lakes
library(readr)
library(ggplot2)

sampler_effort_cluculz <- read_csv("data/raw/sampler_effort_cluculz.csv")
sampler_effort_eaglet <- read_csv("data/raw/sampler_effort_eaglet.csv")
sampler_effort_norman <- read_csv("data/raw/sampler_effort_norman.csv")
sampler_effort_nukko <- read_csv("data/raw/sampler_effort_nukko.csv")

View(sampler_effort_cluculz) #2 burbot (killed)
View(sampler_effort_eaglet) #1 burbot (killed)
View(sampler_effort_norman) #1 burbot (killed)
View(sampler_effort_nukko) #0 burbot

#sparse data. Still, here are the catch-per-unit effort calculations:

cpue_sampler_cluculz <- sum(sampler_effort_cluculz$`Burbot killed`, na.rm = TRUE) / sum(sampler_effort_cluculz$`Effort (hrs)`[1:5])
cpue_sampler_cluculz #CPUE = 0.322 burbot per hour of fishing

cpue_sampler_eaglet <- sum(sampler_effort_eaglet$`Burbot killed`, na.rm = TRUE) / sum(sampler_effort_eaglet$`Effort (hours)`[1:15])
cpue_sampler_eaglet #CPUE = 0.018 burbot per hour

cpue_sampler_norman <- sum(sampler_effort_norman$`Burbot killed`, na.rm = TRUE) / sum(sampler_effort_norman$`Effort (hours)`[1:6])
cpue_sampler_norman #CPUE = 0.045 burbot per hour

cpue_sampler_nukko <- 0 #no burbot were caught at Nukko Lake during researchers' sampling efforts

