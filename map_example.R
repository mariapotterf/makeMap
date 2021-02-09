

# Make a map in R
# --------------------------------------------

# Map of the study area
# follow example: https://geocompr.robinlovelace.net/adv-map.html

# goal: make a mpa of the study site for the NO, GE, Fi 
# beetle sample data
# Simple study site map

# uses 'tmap' library which is ment for thematic maps, vectors and rasters



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

# tmap can work with objects: sf + dataframe, and with stars objects

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

map_nz1 =  map_nz +
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




# Example to use ggsn package ---------------------------------------------
# http://oswaldosantos.github.io/ggsn/

library(ggsn); library(ggplot2); library(sf)
data("domestic_violence")

(ggm1 <- ggplot(domestic_violence, aes(fill = Scaled)) +
    geom_sf() +
    scale_fill_continuous(low = "#fff7ec", high = "#7F0000"))


ggm1 +
  blank() +
  north(domestic_violence) +
  scalebar(domestic_violence, dist = 4, dist_unit = "km",
           transform = TRUE, model = "WGS84")










#  Example thematic map raster --------------------------------------------

# https://rdrr.io/cran/tmap/man/tm_raster.html

pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")


tm_shape(land, ylim = c(-88,88)) +
  tm_raster("cover_cls", palette = pal8, title = "Global Land Cover") +
  tm_shape(metro) + tm_dots(col = "#E31A1C") +
  tm_shape(World) +
  tm_borders(col = "black") +
  tm_layout(scale = .8, 
            legend.position = c("left","bottom"),
            legend.bg.color = "white", legend.bg.alpha = .2, 
            legend.frame = "gray50")


pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", "#8CBE8C",
           "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", "#9132E6",
           "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", "#FF0000",
           "#FFFFFF", "#5ADCDC")
tm_shape(land) +
  tm_raster("cover", palette = pal20, title = "Global Land Cover") + 
  tm_layout(scale=.8, legend.position = c("left","bottom"))




tm_shape(land, ylim = c(-88,88)) +
  tm_raster("trees", palette = "Greens", title = "Percent Tree Cover") +
  tm_shape(World) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom"), bg.color = "lightblue")

## Not run: 
tm_shape(land) +
  tm_raster("black") +
  tm_facets(by="cover_cls")



