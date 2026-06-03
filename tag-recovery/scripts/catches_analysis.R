# Code to analyse catch-per-unit effort data for CARP LAKE across both tagging years.
# Boxplots and ANOVA to identify any differences in mean CPUE by season or zone

# Liz Hirsch
# Feb. 27 2024
source("carp_lk/codes/R/carp_bb_heatmap.R")
source("carp_lk/codes/processing/fraser_ffsbc_processing.R")

# Look at burbot catch success by zone ------------------------------------
all_carptraps <- all_carptraps %>%
  mutate(zone = as.factor(case_when(start_date == "2022-05-30" ~ 2, start_date == "2022-05-31" ~ 3,
                                    start_date == "2022-06-01" ~ 4, start_date == "2022-06-02" ~ 1, start_date == "2022-06-20" ~ 2,
                                    start_date == "2022-06-21" ~ 3, start_date == "2022-06-22" ~ 4, start_date == "2022-06-23" ~ 1,
                                    start_date == "2022-10-24" ~ 2, start_date == "2022-10-25" ~ 3, start_date == "2022-10-26" ~ 4,
                                    start_date == "2022-10-27" ~ 1, str_detect(trap_id, "Z1") ~ 1, str_detect(trap_id, "Z2") ~ 2,
                                    str_detect(trap_id, "Z3") ~ 3, str_detect(trap_id, "Z4") ~ 4)),
         season = factor(season, levels = c("Spring 2022", "Fall 2022", "Spring 2023")),
         zone = factor(zone, levels = c("1", "2", "3", "4")))


pal3 <- colorFactor(palette = "Reds", domain = all_carptraps$zone)

all_carptraps %>%
 # filter(species == "BB") %>%
  leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", 
                   radius = 3, color = ~pal3(zone)) %>%
  addLegendFactor(position = "topright", shape = "rect", orientation = "vertical", pal = pal3, 
                  values = all_carptraps$zone, title = "zone", 
                  width = 4, height = 30) 


# Boxplot CPUE (in BURBOT PER TRAP SET) by zone, all traps 2022-2023 -------------------------------

ggplot(all_carptraps, aes(x = zone, y = bb_count)) +
  geom_boxplot(fill = "darkseagreen3") +
  xlab("Zone") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot catch-per-unit-effort by trapping zone")+
  theme(plot.title = element_text(hjust = 0.5, position_nudge(x = 1))) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

ggplot(all_carptraps, aes(x = zone, y = bb_count, color = zone)) +
  geom_jitter(height = 0.1) +
  xlab("Zone") + 
  ylab("Number of burbot caught") +
  ggtitle("Number of burbot caught per trap by zone in Carp Lake")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

ggplot(all_carptraps, aes(x = zone, y = trap_dur)) +
  geom_boxplot(fill = "darkseagreen3") +
  xlab("Zone") + 
  ylab("Trap duration (hours)") +
  ggtitle("Carp Lake trap set durations by trapping zone")+
  theme(plot.title = element_text(hjust = 0.5, position_nudge(x = 1))) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())


# ANOVA (CPUE~zone) -------------------------------------------------------
library(car)

zon_aov <- aov(bb_count~zone, data = all_carptraps)
par(mfrow = c(1,2))
hist(zon_aov$residuals)
qqPlot(zon_aov$residuals, id = FALSE)
summary(zon_aov)


res2 <-cor.test(all_carptraps$bb_count, all_carptraps$depth,  method = "spearman")
res2

tempcor <- cor.test(all_carptraps$bb_count, all_carptraps$temp,  method = "spearman")
tempcor

all_carptraps %>%
  ggplot(aes(x = temp, y = bb_count))+
  geom_point()+
  geom_smooth(method = "lm")
# There does NOT appear to be a significant difference in mean CPUE between any of the 4 different 
# trapping zones in Carp Lake (p > 0.05). 

# CPUE (in burbot per trap set) by SEASON, all traps -----------------------------------------------

ggplot(all_carptraps, aes(x = season, y = bb_count)) +
  geom_boxplot(fill = "seagreen3") +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping season")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

# jitter to look at modes
ggplot(all_carptraps, aes(x = season, y = bb_count, color = season)) +
  geom_jitter() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping season")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

# ANOVA (CPUE~season) -------------------------------------------------------------------
season_aov <- aov(cpue ~ season,
                  data = all_carptraps)
par(mfrow = c(1,2))
hist(season_aov$residuals)
qqPlot(season_aov$residuals, id = FALSE)
summary(season_aov)

# ANOVA indicates we can reject the null hypothesis that mean CPUE is equal among all trapping 
# seasons. At lease one season is different from the others in terms of the mean CPUE (p = 0.000519). 

# CPUE by season AND zone (plotted two different ways) --------------------

ggplot(all_carptraps, aes(x = season, y = cpue, fill = zone)) +
  geom_boxplot() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by season and zone")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

ggplot(all_carptraps, aes(x = zone, y = cpue, fill = season)) +
  geom_boxplot() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by season and zone")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())


# Fraser Lake ANOVA - cpue by zone ----------------------------------------
fraser_alltraps <- mutate(fraser_alltraps, 
                          bb_count = ifelse(species == "BB", count_fish, 0),
                          start_dt = as.POSIXct(paste(fraser_alltraps$start_date, fraser_alltraps$start_time), format="%m/%d/%Y %H:%M:%S"),
                     end_dt = as.POSIXct(paste(fraser_alltraps$end_date, fraser_alltraps$end_time), format="%m/%d/%Y %H:%M:%S"),
                     trap_dur = difftime(end_dt, start_dt), 
                     cpue = bb_count / as.numeric(trap_dur, na.rm = TRUE),
                     zone = factor(zone),
                     year = factor(year))

length(fraser_alltraps$year[fraser_alltraps$year == "2022"])

fraser_alltraps %>%
  na.omit() %>%
  ggplot(aes(x = zone, y = cpue)) +
  geom_boxplot(fill = "darkseagreen3") +
  xlab("Zone") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Fraser Lake burbot catch-per-unit-effort by trapping zone")+
  theme(plot.title = element_text(hjust = 0.5, position_nudge(x = 1))) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

fraser_alltraps %>%
  na.omit() %>%
  ggplot(aes(x = zone, y = bb_count, color = zone)) +
  geom_jitter(height = 0.1) +
  xlab("Zone") + 
  ylab("Number of burbot caught") +
  ggtitle("Number of burbot caught per trap by zone in Fraser Lake")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

library(car)

zon_aovf <- aov(cpue~zone, data = fraser_alltraps)
par(mfrow = c(1,2))
hist(zon_aovf$residuals)
qqPlot(zon_aovf$residuals, id = FALSE)
summary(zon_aovf)


# Fraser Lake - cpue by year --------------------------------------------

fraser_alltraps %>%
  #na.omit() %>%
  ggplot(aes(x = year, y = cpue)) +
  geom_boxplot(fill = "seagreen3") +
  xlab("Year") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Fraser Lake burbot catch-per-unit-effort by year")+
  theme(plot.title = element_text(hjust = 0.5, position_nudge(x = 1))) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

ggplot(fraser_alltraps, aes(x = year, y = cpue, color = year)) +
  geom_jitter() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Number of burbot caught per trap by trapping year")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

yr_aovf <- aov(cpue~year, data = fraser_alltraps)
par(mfrow = c(1,2))
hist(yr_aovf$residuals)
qqPlot(yr_aovf$residuals, id = FALSE)
summary(yr_aovf)


# Inter-lake comparison of CPUE -------------------------------------------

# bind together carp lake and fraser lake traps data

# Modify fraser traps 
fraser_alltraps <- fraser_alltraps %>% 
  mutate(lake_id = "Fraser Lake") %>%
  rename(season = year)

all_carptraps <- all_carptraps %>%
  subset(select = -c(Method, depth, temp, floy_id, length, weight, comment)) %>%
  rename(count_fish = num_fish)
  
# Add Cluculz Lake effort and catch:
cluculz_traps <- filter(traps_2022, lake_id == "Cluculz Lake") %>%
  mutate(season = "Spring 2022", 
         burbot = case_when(species == "BB" ~ "burbot caught", 
                                                    species != "BB" ~ "no burbot caught", 
                            is.na(species) ~ "no burbot caught"),
         zone = case_when(str_detect(trap_id, "Z3") ~ 3, str_detect(trap_id, "Z1") ~ 1)) %>%
  subset(select = -c(Method, depth, temp, floy_id, comment, bait_type, bait_wt,
                     length, weight)) %>%
  rename(count_fish = num_fish)
  
ca_fr_traps <- rbind(fraser_alltraps, all_carptraps, cluculz_traps) %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue))

ca_fr_traps %>%
  #na.omit() %>%
  ggplot(aes(x = lake_id, y = cpue)) +
  geom_boxplot() +
  xlab("Lake") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Comparison of burbot CPUE by lake") +
  theme(plot.title = element_text(hjust = 0.5, position_nudge(x = 1))) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.10)) +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

(fr_scatter <- ca_fr_traps %>%
  filter(lake_id == "Fraser Lake") %>%
  ggplot(aes(x = season, y = cpue)) +
  geom_point())

(car_scatter <- ca_fr_traps %>%
  filter(lake_id == "Carp Lake") %>%
  ggplot(aes(x = season, y = cpue)) +
  geom_point())

library(gridExtra)
grid.arrange(fr_scatter, car_scatter, ncol = 2)

car_hist <- ca_fr_traps %>%
  filter(lake_id == "Carp Lake") %>%
  ggplot(aes(x = cpue)) + 
  geom_histogram(bins = 15) +
  ggtitle("Carp Lake CPUE distribution")

fr_hist <- ca_fr_traps %>%
  filter(lake_id == "Fraser Lake") %>%
  ggplot(aes(x = cpue)) + 
  geom_histogram(bins = 15) +
  ggtitle("Fraser Lake CPUE distribution")

grid.arrange(car_hist, fr_hist, ncol = 2)

### CPUE tables
library(gt)
summary_tbl <- ca_fr_traps %>%
  rename(Lake = lake_id) %>%
  group_by(Lake, season) %>% 
  summarize("Number of traps set" = n_distinct(trap_id),
            "Trap hours" = round(sum((as.numeric(trap_dur)/60), na.rm = TRUE), digits = 0),
            "Total burbot caught" = sum(bb_count, na.rm = TRUE),
            "Mean CPUE" = mean(cpue),
            "SD of CPUE" = sd(cpue))

(summary_table <- gt(summary_tbl) %>%
  fmt_number(
    decimals = 4, drop_trailing_zeros = TRUE) %>%
  tab_header(title = "Burbot capture effort at Carp Lake, Fraser Lake, and Cluculz Lake") |>
  cols_label(starts_with("season")~""))

summary_tbl2 <- ca_fr_traps %>%
  rename(Lake = lake_id) %>%
  group_by(Lake) %>% 
  summarize("Mean CPUE" = mean(cpue),
            "SD of CPUE" = sd(cpue),
            "Number of traps set" = n_distinct(trap_id),
            "Total burbot caught" = sum(bb_count, na.rm = TRUE))
summary_table2 <- gt(summary_tbl2) %>%
  fmt_number(
    decimals = 5, drop_trailing_zeros = TRUE) %>%
  tab_header(title = "Burbot capture effort at Carp Lake and Fraser Lake")

#####
all_carptraps <- all_carptraps %>%
  mutate(cpue = if_else(is.na(cpue), 0, cpue))

summary_tbl2 <- all_carptraps %>%
  rename(Lake = lake_id) %>%
  group_by(season) %>% 
  summarize("Total traps set" = n_distinct(trap_id),
            "Total burbot caught" = sum(bb_count, na.rm = TRUE),
            "Mean CPUE" = mean(cpue, na.rm = TRUE),
            "SD" = sd(cpue, na.rm = TRUE))

total_row <- all_carptraps %>%
  summarize(
    season = "Total",
    `Total traps set` = n_distinct(trap_id),
    `Total burbot caught` = sum(bb_count, na.rm = TRUE),
    `Mean CPUE` = mean(cpue, na.rm = TRUE),
    SD = sd(cpue, na.rm = TRUE)
  )

summary_all <- bind_rows(summary_tbl2, total_row)

summary_table2 <- gt(summary_all) %>%
  fmt_number(
    columns = vars(`Mean CPUE`, `SD`),
    decimals = 3, drop_trailing_zeros = T) %>%
  tab_header(title = "Burbot trap effort and catch at Carp Lake")

#####

# percentage of traps catching 0, 1, 2, 3 burbot --------------------------
nrow(ca_fr_traps) # 998 total traps
(per_1bb <- nrow(filter(ca_fr_traps, bb_count == 1))/ nrow(ca_fr_traps) *100)
(per_2bb <- nrow(filter(ca_fr_traps, bb_count == 2))/nrow(ca_fr_traps) *100)
(per_3bb <- nrow(filter(ca_fr_traps, bb_count == 3))/ nrow(ca_fr_traps) *100)
(per_0bb <- ((nrow(filter(ca_fr_traps, is.na(bb_count))) + nrow(filter(ca_fr_traps, bb_count == 0))) / nrow(ca_fr_traps))*100)
