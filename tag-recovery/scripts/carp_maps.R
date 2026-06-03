# code to generate maps of trap-sets and burbot catch locations at Carp Lake
library(tidyverse)
library(sf)
library(spatstat)
library(mapview)
library(maptools)
library(lattice)
library(leafpop)
library(viridis)

X2023_burbot_trap_sets <- read_csv("carp_lk/data/raw/2023_burbot_trap_sets.csv")

colnames(X2023_burbot_trap_sets) <- c("lake", "trap_waypoint", "start_date", "start_time", "end_date", "end_time",
                            "lat", "lon", "method", "depth", "temp", "species", "count_fish", "floy", "length",
                            "weight", "comment")

glimpse(X2023_burbot_trap_sets)
X2023_burbot_trap_sets$lon <- as.double(X2023_burbot_trap_sets$lon)

burbottraps <- filter(X2023_burbot_trap_sets, X2023_burbot_trap_sets$species == 'BB')
burbottraps_sf <- st_as_sf(burbottraps, coords = c("lat", "lon"), crs = 4326)

ggplot() +
  geom_sf(data = burbottraps_sf)+
  ggtitle("2023 Carp Lake burbot capture sites")

mapview(X2023_burbot_trap_sets, xcol = "lon", ycol = "lat", zcol = "species", crs = 4269, grid = FALSE)
plot(burbottraps_sf["species"])

