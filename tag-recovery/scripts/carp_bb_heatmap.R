# Code to create geographical heatmap of burbot catches at Carp Lake
# prepared for FFSBC annual progress report
# Feb 16, 2024
# Liz Hirsch


# Packages and source files ------------------------------------------------------------

library(ggplot2)
library(leaflet)
library(sp)
library(timetk)
library(leaflegend)

source("carp_lk/codes/processing/traps_2023_processing.R")
source("carp_lk/codes/processing/traps_2022_processing.R")


# map of Carp Lake --------------------------------------------------------

carpmap <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= -123.3667, lat=54.7833, popup="Carp Lake provincial park")
carpmap

# Add trap locations (2023)
leaflet(data = traps_2023) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", radius = 5)

# Add trap locations (2022)
leaflet(data = traps_2022) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", radius = 5, color = "red")
#2022 data included Fraser and Cluculz Lakes but we'll filter those out 

# combine 2022 and 2023 trap data -----------------------------------------

# some formatting to match columns:
traps_2022_formatted <- traps_2022 %>%
  subset(select = -c(bait_type, bait_wt)) %>%
  filter(lake_id == "Carp Lake")
traps_2022_formatted$temp <- as.numeric(traps_2022_formatted$temp)
  
all_carptraps <- bind_rows(traps_2022_formatted, traps_2023) %>%
  filter(lake_id == "Carp Lake") %>%
  mutate(start_date = as.Date(start_date, format = '%m/%d/%y')) %>%
  mutate(season = case_when(start_date >= '2022-05-30' & start_date <= '2022-06-23' ~ 'Spring 2022',
         start_date >= '2022-10-24' & start_date <= '2022-10-27' ~ 'Fall 2022',
         start_date >= '2023-05-23' & start_date <= '2023-06-16' ~ 'Spring 2023'))


# map of 2022-2023 traps, colour coded by season ----------------------------

pal <- colorFactor(c("navy", "gold", "seagreen"), domain = c("Spring 2022", "Fall 2022", "Spring 2023"))

leaflet(data = all_carptraps) %>% addTiles() %>%
addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", radius = 3, color = ~pal(season))


# map of traps colour-coded by burbot capture success ---------------------
# create column with value TRUE if any burbot were caught there, and FALSE if no burbot caught

all_carptraps <- all_carptraps %>%
  mutate(burbot = case_when(species == "BB" ~ "burbot caught", species != "BB" ~ "no burbot caught", is.na(species) ~ "no burbot caught"))

pal <- colorFactor(c("seagreen", "darkgrey"), domain = c("burbot caught", "no burbot caught"))

leaflet(data = all_carptraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", 
                   radius = 3, color = ~pal(burbot)) %>%
  addLegendFactor(position = "topright", shape = "rect", orientation = "horizontal", pal = pal, 
                  values = all_carptraps$burbot, title = NULL, 
                   width = 5, height = 10)


# Try color coding by depth! ---------------------------------
pal2 <- colorNumeric(palette = "GnBu", domain = all_carptraps$depth)

leaflet(data = all_carptraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", 
                   radius = 5, color = ~pal2(depth)) %>%
  addLegendNumeric(position = "topright", shape = "rect", orientation = "vertical", pal = pal2, values = all_carptraps$depth, title = "depth (m)", 
                   width = 5, height = 100)


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

pal3 <- colorFactor(palette = "Greys", domain = all_carptraps$zone)
#########################
#ZONES MAP:
carp_zonesmap <- leaflet(data = all_carptraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", 
                   radius = 3, color = ~pal3(zone)) %>%
  addLegendFactor(position = "topright", shape = "rect", orientation = "vertical", pal = pal3, 
                   values = all_carptraps$zone, title = "zone", 
                   width = 5, height = 100)

#coord_boatramp <- c(54.823854, -123.349105)
#########################

# Boxplot CPUE by zone, all traps 2022-2023 -------------------------------

ggplot(all_carptraps, aes(x = zone, y = cpue)) +
  geom_boxplot(fill = "darkseagreen") +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping zone")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

ggplot(all_carptraps, aes(x = zone, y = cpue, color = zone)) +
  geom_jitter() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping zone")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Paired") +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

# ANOVA (CPUE~zone) -------------------------------------------------------
library(car)

zon_aov <- aov(cpue~zone, data = all_carptraps)
par(mfrow = c(1,2))
hist(zon_aov$residuals)
qqPlot(zon_aov$residuals, id = FALSE)
summary(zon_aov)
# There does NOT appear to be a significant difference in mean CPUE between any of the 4 different 
# trapping zones in Carp Lake (p > 0.05). 

# CPUE by SEASON, all traps -----------------------------------------------

ggplot(all_carptraps, aes(x = season, y = cpue)) +
  geom_boxplot(fill = "seagreen3") +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping season")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "top", legend.title = element_text())

# jitter to look at modes
ggplot(all_carptraps, aes(x = season, y = cpue, color = season)) +
  geom_jitter() +
  xlab("") + 
  ylab("CPUE in burbot /trap hour") +
  ggtitle("Carp Lake burbot CPUE by trapping season")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
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

