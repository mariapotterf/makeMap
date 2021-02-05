

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
library(ggsn)          # add scale, north arrow, works with ggplot2
#library(ggmap)         # add scale, north arrow...
#library(ggspatial)     # add static map elements: scale, north arror



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


# Reclassify bioblimatic regions for NO, FI, GE
# -	the German ones ‘temperate forest’, 
#	- the Finnish ones ‘boreal forest’ 
#	- the Norwegian ones ‘boreal&boreonemoral forest’
site <- site %>% 
  mutate(bioclim = case_when(Country  == "finland" ~ "boreal",
                             Country  == "norway"  ~ "boreal_boreonemoral",
                             Country  == "germany" ~ "temperate"))


# covert raster to dataframe to allow 
# plotting with ggplots2!!!
# ---------------------------
#forest_df <- as.data.frame(forest, xy = TRUE) 



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


# plots raster 
# raster needs to be saved as df which I can do
#ggplot() +
 # geom_raster(data = forest) + 
  #coord_quickmap()






# Make a map -----------------------------------------
# example on ggplot2 & inset map: https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/

windows()
p1 <- ggplot(data = europe) +
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
          aes(color = bioclim)) +
  # This boundary settings is mostly rial and error process
  coord_sf(xlim = c(-1000000, 2000000),
           ylim = c( 5000000,  7800000)) + # set zoom as the last one as I am adding new shp
#ylim = c(45, 72), expand = FALSE
  # theme_void() # # Set a completely blank theme, to get rid of all background and axis elements
  theme_bw() #+
 # ggspatial::annotation_north_arrow()


p1 +# blank() +
  ggsn::north(x.min = -1000000,  # define the min max values at the coordinates_sf
              x.max = 2000000,
              y.min = 5000000, #data = europe,
              y.max = 7800000,
              #location = 'topright',
              scale = 0.2, #  # scale: 0-1, proportion of teh size
              symbol = 3)  # get symbols by northSymbols()
  ggsn::scalebar(data = europe, 
                 dist = 100, 
                 dist_unit = "km",
           transform = FALSE)



  
  
  
  
  
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










# add north arrow and scale
p1 + ggspatial::annotation_scale(
  location = "tr",
  bar_cols = c("grey60", "white"),
  text_family = "ArcherPro Book"
) +
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
