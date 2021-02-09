
# -----------------------------------------------------
# Map of study site for Gradient paper (Burner, 2021) #
# -----------------------------------------------------
# What to plots:
#    site data      - points shapefile (vector)
#    forest cover   - raster 
#    Europe         - polygon shapefile



rm(list = ls())


# Libraries ---------------------------------------------------------------

library(sf)            # Manage vector data
library(raster)        # Manage raster data
library(dplyr)
library(ggplot2)
library(ggsn)          # add scale, north arrow, works with ggplot2
library(ggmap)         # add scale, north arrow...
library(ggspatial)     # add static map elements: scale, north arrow; works with ggplot2
#library(stars)         # convert raster to start to plot with ggplot


# Get input data
setwd("C:/MyTemp/2019_bioESS_coord/outMaps/2021_gradient/input")

# Read input shp and raster data --------------------------------
europe <- st_read(paste(getwd(), "Europe_ERTS.shp", sep = "/"))    # polygon
site   <- st_read(paste(getwd(), "pts_bioregions.shp", sep = "/")) # point

# Read forest as raster: 1) original and 2) cropped and reclassified raster in ArcGIS
# in R reclassification takes forever
# 1) original raster
forest_br <- raster(paste(getwd(), "eea_r_3035_100_m_forest-area-2015_p_2015_v1_r1.tif", sep = "/"),
                    NAvalue = 65535)


#2) cropped raster - in ArcGIS
forest_r <- raster(paste(getwd(), "forest_simple2_int_rcl.tif", sep = "/"))



# CHeck coordinate system of raster and polygons ---------------------------

# here, coordinate system is the same. if not, need to `st_transform(shapefile, EPSF_code)`.
# see more here: https://geocompr.robinlovelace.net/reproj-geo-data.html

# Process data ------------------------------------------------------------

# Reclassify bioclimatic regions names for NO, FI, GE
# -	the German     ‘temperate forest’, 
#	- the Finnish    ‘boreal forest’ 
#	- the Norwegian  ‘boreal&boreonemoral forest’
site <- site %>% 
  mutate(bioclim = case_when(Country  == "finland" ~ "boreal",
                             Country  == "norway"  ~ "boreal_boreonemoral",
                             Country  == "germany" ~ "temperate"))


# Use tmaps to plot sites over Europe -------------------------------------------------

# https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#multiple-shapes-and-layers
# http://fisherankney.com/spatial-analysis-greece
# Syntax of tmap()"
#  - first define the `shape` (= layer to plot), 
#      then define its characteristics in new line (shape -> raster, shape -> polygon, etc.)
# keep track what characteristics belong to which shape by intendation

library(tmap)      # thematic map package
library(tmaptools) # create bounding box for zoom in 
library(shinyjs)   # for interactive pallet vievwing


# CHeck the current bounding box: I want to zoom map in 
st_bbox(europe)

# Create my own bounding box = zoom in
# Guess values from tm_graticules() or tm_grid() values

# vector:                     # xmin,   ymin,    xmax,     ymax
zoom_box = tmaptools::bb(c( -800000,5000000, 1500000, 7800000))


# Explore palette options
# needs 'shinyjs' library
#tmaptools::palette_explorer()

# St up the plot mode: not interactive
tmap_mode("plot")   # # tmap_mode("view")  # for interactive zoom in and out


windows()

# Make a layered map: first define the shape and then plot it: add graticules, polygons, different layes...

tm_shape(europe,
         bbox = zoom_box) + # zoom in europe shp using new bounding box
    # Add graticules
    tm_graticules(col = "grey85",
                lwd = 0.5) +
    # Add grey polygons and border line
    tm_polygons() +
  # Add raster
tm_shape(forest_br) +
    tm_raster(#col = "eea_r_3035_100_m_forest-area-2015_p_2015_v1_r1.tif",
              col = "forest_simple2_int_rcl.tif",  # # name of the raster r after calluing raster(r) function
              palette = "darkgreen",
              title = "Forest cover") +
  # Add just countries borders on top  
tm_shape(europe) +
    tm_borders("black", lwd = .5) +
tm_shape(site) +
     tm_symbols(col = "bioclim",
                border.col = NA,
                shape = 16,  # solid circle, no outline
                size = 0.1, 
                palette = "YlOrRd", #,
                title.col = "Bioclimatic regions") +
  # Add north sign: no needed here if there is a graticule
# tm_compass(#type = "2star",
  #            position = c("left", "top"),
  #            bg.color = "white") + 
tm_scale_bar(bg.color="white") +
  # Modify the legend
tm_legend(bg.color="white",
            position = c("left", "top"),
            outside = TRUE) +
tm_layout(attr.outside=FALSE)





# Plot in ggplot2 --------------------------------------------------------------
# (just for record, nOt further used)
# start with ggplot maps here: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
## ggplot is ok to plot vector data, but might take forever to plot large raster, 
# as they need to be converted first to dataframe and then plotted;  so I do not do it.
# Also possible to convert it to  `stars` object and then plot, but also takes long time.
# Maybe need to increase `memory.limit(12000)`


# Further example on ggplot2 & inset map: https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/

# To add compass and scale, ggplot2 works with 'ggspatial' and 'ggsn' (both later)

ggplot(data = europe) + 
  geom_sf() 


# Get color blind palete
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


windows()
# First create a map object as in normal ggplot2; then we will add scale and north arrow using 2 packages 
p1 <- ggplot(data = europe) +
  geom_sf(color = 'black',
          fill = 'lightgrey',
          size = 0.3) +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Europe", 
          subtitle = paste0("(", length(unique(europe$NAME)), " countries)")) +
  geom_sf(data = site, 
          aes(color = bioclim)) +
  scale_colour_manual(values=cbbPalette) +
  scale_shape_manual(values=c(1,2,3)) +
  # This boundary settings is mostly trial and error process
  coord_sf(xlim = c(-1000000, 2000000),
           ylim = c( 5000000,  7800000)) + # set zoom as the last one as I am adding new shp
  #ylim = c(45, 72), expand = FALSE
  theme_bw() #+




# Check out ggspatial library ---------------------------------------------

p1 + 
  annotation_scale(location = "br", 
                   width_hint = 0.4,
                   style = "bar") +  # or style = "ticks"
  annotation_north_arrow(location = "bl", 
                         which_north = "false", 
                         pad_x = unit(3.6, "in"), 
                         pad_y = unit(2.9, "in"),
                         style = north_arrow_fancy_orienteering)
  
  
  



# Check out ggsn library --------------------------------------------------


p1 +# blank() +
  ggsn::north(x.min = -1000000,  # define the min max values at the coordinates_sf
              x.max = 2000000,
              y.min = 5000000, #data = europe,
              y.max = 7800000,
              #location = 'topleft',
              scale = 0.2, #  # scale: 0-1, proportion of teh size
              symbol = 3) +  # get symbols by northSymbols()
   ggsn::scalebar(data = europe,  # Do not know where scale bar is
               dist = 100, 
               dist_unit = "km",
               transform = FALSE)





