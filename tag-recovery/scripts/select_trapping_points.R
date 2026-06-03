library(tidyverse)
library(sf)
library(bcdata)
library(lwgeom)
sf::sf_use_s2(FALSE) # this has to be executed or st_sample will throw an error

# Using QGIS created layers ----

## Fraser Lake ----
fraser_bathy <- bcdc_get_data("493fb840-1909-489e-91c8-1c9ce9ccee9c") %>%
  st_transform(crs = 4326) %>%
  filter(LAKE_GAZETTED_NAME == "Fraser Lake") %>%
  select(CONTOUR_DEPTH_M, geometry) %>%
  rename(depth_contour = CONTOUR_DEPTH_M)

fraser_out <- filter(fraser_bathy, depth_contour == 0)

fraser_in <- filter(fraser_bathy, depth_contour == 10)

fraser_bathy_out_in <- st_difference(fraser_out, fraser_in)

# Check if polygon only includes area between contours 0 and 10 (will appear red)
plot(st_geometry(fraser_bathy_out_in), col = "red")

# Set seed to make results reproducible
set.seed(123) 
# Set desired number of sampling points. Note that function will first sample
# within the whole lake, then we will filter by the area that includes the depths
# of interest. So you will want to set the number of sampling points quite a bit
# higher than your desired number of points for the area of interest.
np <- 1000
fraser_points <- st_sf(geometry = st_sample(fraser_bathy, size = np)) %>%
  st_filter(fraser_bathy_out_in) %>%
  mutate(Name = paste("FRATP-", 1:n(), sep = ""))

# Plot to check sampled points
plot(st_geometry(fraser_bathy))
plot(st_geometry(fraser_points), add = TRUE, col = "red", cex = 0.25)

# Check how many points you ended up with. If not enough, set np to a higher 
# value above.
nrow(fraser_points)

# Save points. The function will issue an error if file already exists. If you
# need to replace the file, delete it first.

# KML for visualization
select(fraser_points, Name, geometry) %>%
  st_write("data/gis/fraser_trapping_points.kml", driver = "kml")

# GPX to upload to GPS
select(rename(fraser_points, name = "Name"), name, geometry) %>%
  st_write("data/gis/fraser_trapping_points.gpx", driver = "gpx")


## Cluculz Lake ----
# NOTE: Cluculz depth are in feet.

cluculz_bathy <- read_sf("data/gis/cluculz_lake_bathymetric.kml") %>%
  mutate(depth_contour = as.numeric(str_extract(Name, "\\d+"))) # depth are in feet

cluculz_z1 <- read_sf("data/gis/cluculz_zone_1.kml")

cluculz_z2 <- read_sf("data/gis/cluculz_zone_2.kml")

cluculz_z3 <- read_sf("data/gis/cluculz_zone_3.kml")

# Check zones

plot(st_geometry(cluculz_bathy))

# Z1 is not covering a portion of the west end of the lake
plot(st_geometry(cluculz_z1), add = TRUE, col = "red")

plot(st_geometry(cluculz_z2), add = TRUE, col = "blue")

# Z3 overlaps a portion of Z2
plot(st_geometry(cluculz_z3), add = TRUE, col = "green")

cluculz_out <- filter(cluculz_bathy, depth_contour == 0)

# NOTE: Using 50 ft for inside contour because 25ft causes an error on line 66 
# saying there are intersecting points between 0 and 25ft contours. Please check.
cluculz_in <- filter(cluculz_bathy, depth_contour == 50)  

cluculz_bathy_out_in <- st_difference(cluculz_out, cluculz_in)

# Check if polygon only includes area between contours 0 and 25 (will appear red)
plot(st_geometry(cluculz_bathy_out_in), col = "red")

# Set seed to make results reproducible
set.seed(123) 
# Set desired number of sampling points. Note that function will first sample
# within the whole lake, then we will filter by the area that includes the depths
# of interest. So you will want to set the number of sampling points quite a bit
# higher than your desired number of points for the area of interest.
np <- 1000
cluculz_points <- st_sf(geometry = st_sample(cluculz_bathy, size = np)) %>%
  st_filter(cluculz_bathy_out_in) %>%
  mutate(Name = paste("CLUTP-", 1:n(), sep = ""))

# Plot to check sampled points
plot(st_geometry(cluculz_bathy_out_in))
plot(st_geometry(cluculz_points), add = TRUE, col = "red", cex = 0.25)


cluculz_z1_points <- st_intersection(cluculz_points, cluculz_z1) %>%
  mutate(Name = paste("CLU-Z1-", 1:n(), sep = ""))
# Plot to check sampled points
plot(st_geometry(cluculz_bathy_out_in))
plot(st_geometry(cluculz_z1_points), add = TRUE, col = "red", cex = 0.25)

cluculz_z2_points <- st_intersection(cluculz_points, cluculz_z2) %>%
  mutate(Name = paste("CLU-Z2-", 1:n(), sep = ""))
# Plot to check sampled points
plot(st_geometry(cluculz_bathy_out_in))
plot(st_geometry(cluculz_z2_points), add = TRUE, col = "red", cex = 0.25)

cluculz_z3_points <- st_intersection(cluculz_points, cluculz_z3) %>%
  mutate(Name = paste("CLU-Z3-", 1:n(), sep = ""))
# Plot to check sampled points
plot(st_geometry(cluculz_bathy_out_in))
plot(st_geometry(cluculz_z3_points), add = TRUE, col = "red", cex = 0.25)

cluculz_points <- bind_rows(cluculz_z1_points, cluculz_z2_points,
                            cluculz_z3_points)


# Check how many points you ended up with. If not enough, set np to a higher 
# value above.
nrow(cluculz_points)

# Save points. The function will issue an error if file already exists. If you
# need to replace the file, delete it first.

# KML for visualization
select(cluculz_points, Name, geometry) %>%
  st_write("data/gis/cluculz_trapping_points.kml", driver = "kml")

# GPX to upload to GPS
select(rename(st_zm(cluculz_points), name = "Name"), name, geometry) %>%
  st_write("data/gis/cluculz_trapping_points.gpx", driver = "gpx")



