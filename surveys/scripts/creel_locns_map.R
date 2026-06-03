library(tidyverse)
library(leaflet)
library(sp)
library(htmltools)
library(gt)
library(leaflet.extras)

# Read in data for creel burbot catch locations
creel_locns <- read_csv("carp_lk/data/gis/creel_catch_locns_codes2.csv")

factpal <- colorFactor("Dark2", creel_locns$code, n = 10)

extras <- data.frame(
  name = c("Campground", "Boat Ramp", "Bert's Cabin", "White Spruce Island", "Balsam Island", "Spirea Island"),
  lat = c(54.821664, 54.823900, 54.816167, 54.773054, 54.763177,  54.750386),      
  lng = c(-123.353383, -123.349043, -123.345413, -123.352342, -123.396993, -123.354626))

black_icon <- makeIcon(
  iconUrl = "https://maps.gstatic.com/intl/en_us/mapfiles/markers2/measle.png",  # tiny circular black dot
  iconWidth = 7,  # Half the typical size
  iconHeight = 7
)

# Add approximate locations of creel participants' burbot catches
leaflet(data = creel_locns) %>% addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~htmlEscape(code), radius = 3,
                   color = ~factpal(code)) %>%
  addMarkers(data = extras,
             lng = ~lng,
             lat = ~lat,
             icon = black_icon,
             label = ~name,
             labelOptions = labelOptions(
               noHide = TRUE,
               direction = "left",
               textOnly = T,
               style = list("font-size" = "12px", "color" = "black", "font-weight" = "bold")
             )) %>%
  addScaleBar(options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) 
#   
#   addLegend(pal = factpal, values = ~code, opacity = 1)


# Create a map of the catch locations from tag reports
tag_locns <- read_csv("carp_lk/data/gis/tag-report-locs.csv")

carpmap <- leaflet() %>%
  addTiles() %>%  
  addMarkers(lng= -123.3667, lat=54.7833, popup="Carp Lake Provincial Park")

leaflet(data = tag_locns) %>% addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~`Location Description`, radius = 3, color = "darkgreen", 
                   labelOptions = labelOptions(noHide = F,
                                               direction = "bottom", textOnly = T,
                                               style = list("font-size" = "8px", "color" = "black")))
  # addLegend(colors = ~`Location Description`, labels = ~`Location Description`, values = ~`Location Description`, opacity = 1)


gt(tag_locns)

# Table with locations and percentages of fishers
locs <- read.csv("carp_lk/data/creel/creel_locations_table.csv") %>%
  rename("Number of Fishers" = Number.of.fishers)

gt(locs) |>
  tab_header(
    title = "Carp Lake burbot fishing locations reported by survey participants")
