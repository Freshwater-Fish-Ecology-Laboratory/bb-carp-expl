library(tidyverse)
library(sf)
library(spatstat)
library(mapview)
library(maptools)

#1. Fraser Lake
#read files of latitude-longitude coordinates
traps_2022 <- read_csv("carp_lk/data/raw/2022_burbot_trap_sets.csv")
fraser_trap_map <- filter(traps_2022, traps_2022$`Lake name` == "Fraser Lake")
fraser_burbot_map <- filter(fraser_trap_map, fraser_trap_map$Species == 'BB')
#convert to shapefiles
fraser_sf <- st_as_sf(fraser_burbot_map, coords = c("Lat", "Lon"), crs = 4326)

ggplot() +
  geom_sf(data = fraser_sf)+
  ggtitle("Map of Fraser Lake burbot capture sites")

#look at the map ...
mapview(fraser_trap_map, xcol = "Lon", ycol = "Lat", crs = 4269, grid = FALSE)

############
# 2021
############
traps_2021 <- read_csv("data/raw/Burbot trap set data.csv")
traps_2021 <- traps_2021[rowSums(is.na(traps_2021)) != ncol(traps_2021), ]
fr_trap_map_21 <- filter(traps_2021, traps_2021$Lat != "not recorded" & traps_2021$Lon != "not recorded")
fr_burbot_21 <- filter(fr_trap_map_21, fr_trap_map_21$`Species Code` == "BB")
fraser_sf_21 <- st_as_sf(fr_burbot_21, coords = c("Lat", "Lon"), crs = 4326)
traps_2021$Lat <- as.numeric(traps_2021$Lat, na.rm = TRUE)
traps_2021$Lon <- as.numeric(traps_2021$Lon, na.rm = TRUE)

ggplot() +
  geom_sf(data = fraser_sf_21)+
  ggtitle("Map of Fraser Lake trap sites (2021)")

mapview(fr_burbot_21, xcol = "Lon", ycol = "Lat", crs = 4269, grid = FALSE)


#2. Carp Lake
carp_trap_map <- filter(traps_2022, traps_2022$`Lake name` == "Carp Lake")
#carp_burbot_map <- filter(carp_trap_map, carp_trap_map$Species == "BB")
carp_sf <- st_as_sf(carp_trap_map, coords = c("Lat", "Lon"), crs = 4326)

ggplot() +
  geom_sf(data = carp_sf)+
  ggtitle("Map of Carp Lake trap sites")

mapview(carp_trap_map, xcol = "Lon", ycol = "Lat", crs = 4269, grid = FALSE)

##Next, read the shapefiles into a format that can be used with the spatstat package
st_write(fraser_sf, "data/raw/fraser_trap_map.shp", driver = "ESRI Shapefile")
st_write(carp_sf, "data/raw/carp_trap_map.shp", driver = "ESRI Shapefile")

fraser_trap_map <- readShapeSpatial("data/raw/fraser_trap_map.shp")
class(fraser_trap_map)
fraser_trap_map_ppp <- as(fraser_trap_map, "ppp")
qt <- quadrat.test(fraser_trap_map_ppp)
print(qt)
nnd_fraser <- nndist(fraser_trap_map_ppp)
hist(nnd_fraser)

carp_trap_map <- readShapeSpatial("data/raw/carp_trap_map.shp")
carp_trap_map_ppp <- as(carp_trap_map, "ppp")
qt_c <- quadrat.test(carp_trap_map_ppp)
print(qt_c)
nnd_carp <- nndist(carp_trap_map_ppp)
hist(nnd_carp)

