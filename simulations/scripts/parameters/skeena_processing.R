library(tidyverse)

skeena_traps <- read_csv("simulations/data/skeena_traps.csv")

# Trap sets were 48 hours: calculate total effort hours
skeena_traps <- skeena_traps %>%
  mutate(trap_duration = Sets * 48,
         cpue = skeena_traps$`BB catch`/trap_duration,
         bb_catch = as.numeric(`BB catch`)/Sets)

skeena_codtraps <- filter(skeena_traps, Method == "CodTrap")
mean(skeena_codtraps$bb_catch) # 0.02761619
sd(skeena_codtraps$bb_catch) # sd = 0.004

skeena_hooplong <- filter(skeena_traps, Method == "HoopLong")%>%
  mutate(bb_catch = as.numeric(`BB catch`)/Sets)

mean(skeena_hooplong$bb_catch) 
sd(skeena_hooplong$bb_catch)

skeena_hoopshort <- filter(skeena_traps, Method == "HoopShort")%>%
  mutate(bb_catch = as.numeric(`BB catch`)/Sets)

mean(skeena_hoopshort$bb_catch)
sd(skeena_hoopshort$bb_catch)

aov_trp <- aov(cpue ~ Method, data = skeena_traps)
summary(aov_trp)