
library(tidyverse)
library(leaflet)
library(sp)
library(leaflegend)

# Trap location and catch data --------------------------------------------

fraser_alltraps <- read_csv("carp_lk/data/raw/fraser_alltraps.csv")
fraser_alltraps$lat <- as.numeric(fraser_alltraps$lat)
fraser_alltraps$lon <- as.numeric(fraser_alltraps$lon)
leaflet(data = fraser_alltraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", radius = 5)

#
pal <- colorFactor(c("navy", "gold", "seagreen"), domain = c("2021", "2022", "2023"))

leaflet(data = fraser_alltraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = fraser_alltraps$trap_id, radius = 3, color = ~pal(year))


fraser_alltraps <- fraser_alltraps %>%
  mutate(burbot = case_when(species == "BB" ~ "burbot caught", species != "BB" ~ "no burbot caught", is.na(species) ~ "no burbot caught"))
pal <- colorFactor(c("seagreen", "darkgrey"), domain = c("burbot caught", "no burbot caught"))

leaflet(data = fraser_alltraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = "trapping points", label = "trapping points", 
                   radius = 3, color = ~pal(burbot)) %>%
  addLegendFactor(position = "topright", shape = "rect", orientation = "horizontal", pal = pal, 
                  values = fraser_alltraps$burbot, title = NULL, 
                  width = 5, height = 10)

# Burbot length and weight data -------------------------------------------
fraser_bb <- read_csv("carp_lk/data/raw/fraser_allbb.csv") %>%
  select(date_tagged, length, weight, year) %>%
  mutate(lake = "Fraser Lake")

carp_bb <- read_csv("carp_lk/data/raw/carp_bb_capture_histories.csv") %>%
  rename(date_tagged = "Date tagged", length = "Total length (cm)", weight = "Weight (g)")

carp_bb_measurements <- carp_bb %>%
  select(date_tagged, length, weight) %>%
  mutate(lake = "Carp Lake")

burbot <- rbind(carp_bb_measurements, fraser_bb)

(lenth_23_fr <- 
  ggplot(fraser_bb %>%
           mutate(length_mm = (length*10)), aes(x = length_mm)) + 
  geom_histogram(color = "navy", fill = "lightblue", bins = 15) + 
  ylab("Number of burbot") + xlab("Length (mm)") + 
  theme_bw() +
  #theme(panel.border = element_blank(), panel.grid = element_blank()) + 
  ggtitle("Length of burbot tagged at Fraser Lake, 2021-2023") + 
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  scale_x_continuous(breaks = seq(0, 800, by = 100)))

(weight_23_fr <- fraser_bb %>%
  subset(!is.na(weight)) %>%
  ggplot(aes(x = weight)) + geom_histogram(color = "darkgreen", fill = "darkseagreen1", bins = 15) + 
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  scale_x_continuous(breaks = seq(0, 7000, by = 500)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5)) +
        #panel.border = element_blank(), panel.grid = element_blank()) + 
  xlab("Weight (g)") + ylab("Number of burbot") +
  ggtitle("Weight of burbot tagged at Fraser Lake, 2021-2023"))

(lw_taggedbb_fr <- 
    ggplot(fraser_bb %>%
             mutate(length_mm = (length*10), year = as.factor(year)), aes(x = length_mm, y = weight, color = year)) +
    geom_smooth(method = "lm", se = TRUE, color = "darkseagreen1") +
    geom_point() +
    scale_color_brewer(palette = "Paired") +
    ggtitle("Length vs weight of tagged burbot, Fraser Lake 2021-2023") +
    xlab("Length (mm)") +
    ylab("Weight (g)") +
    scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
    scale_x_continuous(breaks = seq(0, 800, by = 100)) +
    theme_bw() +
    theme(plot.title = element_text(size=10)))
median(fraser_bb$length, na.rm = TRUE)
quantile(fraser_bb$length*10, na.rm = TRUE, 0.25)
quantile(fraser_bb$length*10, na.rm = TRUE, 0.75)

median(fraser_bb$weight, na.rm = TRUE)
quantile(fraser_bb$weight, na.rm = TRUE, 0.25)
quantile(fraser_bb$weight, na.rm = TRUE, 0.75)




# Carp Lake vs Fraser Lake burbot size ------------------------------------

ggplot(burbot, aes(x = lake, y = length*10, fill = lake)) +
  geom_boxplot() +
  ylab("Total length (mm)") +
  scale_y_continuous(breaks = seq(200, 1200, by = 100)) +
  xlab("Lake") +
  scale_fill_brewer(palette = "GnBu") +
  ggtitle("Burbot Total Length at Carp Lake and Fraser Lake") +
  theme_bw() +
  theme(legend.position = "none")

# T-TEST
t.test(length ~ lake, data = burbot)
sd(carp_bb$length)
mean(fraser_bb$length, na.rm = TRUE)
ggplot(burbot, aes(x = lake, y = weight, fill = lake)) +
  geom_boxplot() +
  ylab("Weight (g)") +
  xlab("Lake") +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  scale_fill_brewer(palette = "BuGn") +
  ggtitle("Burbot Weight at Carp Lake and Fraser Lake") +
  theme_bw() +
  theme(legend.position = "none")

# T-TEST
t.test(weight ~ lake, data = burbot)

##########################
# Map of Fraser Lake Zones ------------------------------------------------
fraser_alltraps <- fraser_alltraps %>%
  mutate(zone = factor(zone, levels = c("1", "2", "3", "4")))

pal3 <- colorFactor(palette = "Reds", domain = fraser_alltraps$zone)
#########################
#ZONES MAP:
leaflet(data = fraser_alltraps) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = fraser_alltraps$trap_id, label = fraser_alltraps$trap_id, 
                   radius = 3, color = ~pal3(zone)) %>%
  addLegendFactor(position = "topright", shape = "rect", orientation = "vertical", pal = pal3, 
                  values = fraser_alltraps$zone, title = "zone", 
                  width = 5, height = 100)

