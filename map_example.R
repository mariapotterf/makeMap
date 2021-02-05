

# Make a map in R
# --------------------------------------------

# Map of the study area
# follow example: https://geocompr.robinlovelace.net/adv-map.html

# oal: make a mpa of the study site for the NO, GE, Fi 
# beetle sample data
# Simple study site map

# uses 'tmap' library which is ment for thematic maps
# and vectors





# Date: 2021/02/04


library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge) # install.packages('spDataLarge',repos='https://nowosad.github.io/drat/', type='source')

# visualization packages
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

# Data of New Zaeland are included


# Plot New Zaeland data ---------------------------------------------------


# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

# Have both fill and borders shortcut: qtm(nz)  qtm = quick thematic map
map_nz = tm_shape(nz) + tm_polygons()  # tm_polygons() condenses tm_fill() + tm_borders()
class(map_nz)   # tmap object

# Add new layers 

map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)

nz_water = st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()

windows(3,3)
map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_dots()


# Layouts --------------------------------------------------
# add north arrow and scale 
map_nz + 
  tm_compass(type = "4star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 1)


