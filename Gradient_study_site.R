

# Map of study site
# GE, NO, FI
# make inset map of europe
# add forest cover 
# individual points


rm(list = ls())



library(sf)           # Manage vector data
library(raster)       # Manage raster data
library(dplyr)
library(spData)
library(ggplot2)
library(patchwork)     # For multiple map layout



# Get input data
setwd("C:/MyTemp/2019_bioESS_coord/outMaps/2021_gradient/input")

# Read input shp and raster data --------------------------------
# shapefiles as st object
europe <- st_read(paste(getwd(), "Europe_ERTS.shp", sep = "/"))  # polygon
site   <- st_read(paste(getwd(), "pts_bioregions.shp", sep = "/")) # point
forest <- raster(paste(getwd(), "eea_r_3035_100_m_forest-area-2015_p_2015_v1_r1.tif", sep = "/"))

# Chcek attributes = columns
names(site)




# Process data ------------------------------------------------------------

# subset FI, GE and NO to add them as new layer
countries = europe %>% filter(NAME == "Finland"| NAME == "Norway"|NAME == "Germany")
#no = europe %>% filter(NAME == "Norway")
#ge = europe %>% filter(NAME == "Germany")


# project raster data
st_crs(europe)


# covert raster to dataframe to allow 
# plotting with ggplots2!!!
# ---------------------------
forest_df <- as.data.frame(forest, xy = TRUE) 



# Plot data
# will plot all columns that data hs, need to plot only geomtry
plot(europe["NAME"], fill = "grey")


# Plot in ggplot2
# shart with ggplot maps here: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# basic plotting
ggplot(data = europe) + 
  geom_sf() #+
#geom_sf(data = nz_height) +
# scale_x_continuous(breaks = c(170, 175))


# lots raster 
ggplot() +
  geom_raster(data = forest) + 
  coord_quickmap()






# Make a map
windows()
ggplot(data = europe) +
  geom_sf(color = 'black',
          fill = 'lightgrey') +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Europe", 
          subtitle = paste0("(", length(unique(europe$NAME)), " countries)")) +
  # get limits for the x and y axis - guess them from teh long (x) and lat (y)
  # we are on the east and nort so both values are +
  
  # geom_sf(data = countries, 
  #        fill = "grey10",
  #        color = 'grey10') +
  geom_sf(data = site, 
          aes(color = code)) +
  coord_sf(xlim = c(-1000000, 2000000),
           ylim = c( 4000000,  8000000))  # set zoom as the last one as I am adding new shp
#ylim = c(45, 72), expand = FALSE
#

