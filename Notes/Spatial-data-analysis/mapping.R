# load libraries
library(tidyverse)
library(sf)
library(mapview)

# load the fish data
fishdat <- read_csv(here::here("Spatial-data-analysis", "fishdat.csv"))

# load the station data
statloc <- read_csv(here::here("Spatial-data-analysis", "statloc.csv"))

# load the sgdat shapefile
sgdat <- st_read(here::here("Spatial-data-analysis", "sgdat.shp"))

methods(class = 'sf')

sgdat <- st_read(here::here("Spatial-data-analysis", "sgdat.shp"), quiet = T)
sgdat

str(fishdat)
str(statloc)

# Join the data
alldat <- left_join(fishdat, statloc, by = 'Reference')

# create spatial data object
alldat <- st_as_sf(alldat, coords = c('Longitude', 'Latitude'), crs = 4326)

# examine the sf objec
alldat
str(alldat)

# check crs
st_crs(alldat)

# verify the polygon and point data have the same crs
st_crs(sgdat)
st_crs(alldat)

alldatutm <- alldat %>% 
  st_transform(crs = '+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs')
st_crs(alldatutm)

plot(alldat)
plot(sgdat)

plot(alldat$geometry)
plot(sgdat$geometry)

filt_dat <- alldat %>% filter(yr == 2016)
plot(filt_dat$geometry)

fish_crop <- filt_dat[sgdat, ]
plot(fish_crop$geometry)
fish_crop

fish_int <- st_intersection(alldat, sgdat)
plot(fish_int$geometry)
fish_int

fish_cnt <- fish_int %>% 
  group_by(FLUCCS) %>% 
  summarise(
    cnt = sum(Pinfish)
  ) 
fish_cnt

ggplot(fish_cnt, aes(x = FLUCCS, y = cnt)) + 
  geom_bar(stat = 'identity')

# use ggplot with sf objects
ggplot() + 
  geom_sf(data = sgdat, fill = 'green') + 
  geom_sf(data = alldat) 

mapview(sgdat, col.regions = 'green') +
  mapview(alldat, zcol = 'Gear')

# intersect data
tomap <- st_intersection(alldat, sgdat)

# make map
mapview(tomap, zcol = 'Pinfish')
mapview(tomap, zcol = 'Bluefish')

# join maps
mapview(sgdat) + mapview(tomap, zcol = 'Pinfish')
