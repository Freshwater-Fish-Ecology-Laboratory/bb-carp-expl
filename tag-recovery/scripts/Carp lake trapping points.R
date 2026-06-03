
install.packages("tidyverse")
install.packages("sf")
install.packages("bcdata")
install.packages("lwgeom")
library(tidyverse)
library(sf)
library(bcdata)
library(lwgeom)
sf::sf_use_s2(FALSE) # this has to be executed or st_sample will throw an error


carp_bathy <- read_sf("data/gis/Carp bathymetric.kml") %>%
  mutate(depth_contour = as.numeric(str_extract(Name, "\\d+"))) 

carp_out <- filter(carp_bathy, depth_contour == 0)

# NOTE: Using 50 ft for inside contour because 25ft causes an error on line 66 
# saying there are intersecting points between 0 and 25ft contours. Please check.
carp_in <- filter(carp_bathy, depth_contour == 12)  

carp_bathy_out_in <- st_difference(carp_out, carp_in)
###running into errors here

# Check if polygon only includes area between contours 0 and 25 (will appear red)
plot(st_geometry(carp_bathy_out_in), col = "darkgreen")


# Set seed to make results reproducible
set.seed(123) 
# Set desired number of sampling points. Note that function will first sample
# within the whole lake, then we will filter by the area that includes the depths
# of interest. So you will want to set the number of sampling points quite a bit
# higher than your desired number of points for the area of interest.
np <- 1000
carp_points <- st_sf(geometry = st_sample(carp_bathy, size = np)) %>%
  st_filter(carp_bathy_out_in) %>%
  mutate(Name = paste("CARP7-", 1:n(), sep = ""))

# Plot to check sampled points
plot(st_geometry(carp_bathy_out_in))
plot(st_geometry(carp_points), add = TRUE, col = "darkgreen", cex = 0.25)

# Check how many points you ended up with. If not enough, set np to a higher 
# value above.
nrow(carp_points)

# Save points. The function will issue an error if file already exists. If you
# need to replace the file, delete it first.

# KML for visualization
select(carp_points, Name, geometry) %>%
  st_write("data/gis/carp_occ7_trapping_points.kml", driver = "kml", append = FALSE)

# GPX to upload to GPS
select(rename(carp_points, name = "Name"), name, geometry) %>%
  st_write("data/gis/carp_occ7_trapping_points.gpx", driver = "gpx", append = FALSE)



