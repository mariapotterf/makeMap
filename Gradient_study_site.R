

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
#library(ggmap)        # add scale, north arrow...
library(ggspatial)     # add static map elements: scale, north arrow; works with ggplot2
library(stars)         # convert raster to start to plot with ggplot


# Get input data
setwd("C:/MyTemp/2019_bioESS_coord/outMaps/2021_gradient/input")

# Read input shp and raster data --------------------------------
# shapefiles as st object
europe <- st_read(paste(getwd(), "Europe_ERTS.shp", sep = "/"))  # polygon
site   <- st_read(paste(getwd(), "pts_bioregions.shp", sep = "/")) # point

# Read forest as raster and as stars object. 
# Raster works with `tmap`
# Stars wprks with  `ggplot2`
forest_br <- raster(   paste(getwd(), "eea_r_3035_100_m_forest-area-2015_p_2015_v1_r1.tif", sep = "/"))
#forest_s = read_stars(paste(getwd(), "eea_r_3035_100_m_forest-area-2015_p_2015_v1_r1.tif", sep = "/"), 
 #                   NA_value = 65535)

#cropped raster - in ArcGIS
forest_r <- raster(paste(getwd(), "forest_simple2_int_rcl.tif", sep = "/"))

forest_s = read_stars(paste(getwd(), "forest_simple2_int_rcl.tif", sep = "/"), 
                   NA_value = 65535)


# CHeck coordinate system of raster and polygons ---------------------------

#crs(site) <- crs(forest)
#crs(europe)


# Chcek attributes = columns
#names(site)


# Check raster attributes - it is too big, need to change it to plot in ggplot2
#hist(values(forest))


# Process data ------------------------------------------------------------

# subset FI, GE and NO to add them as new layer
countries = europe %>% filter(NAME == "Finland"| NAME == "Norway"|NAME == "Germany")

# Convert sf object to start object
# eu_star <- st_as_stars(europe)


# project raster data
# st_crs(europe)

# Convert raster to polygon
#r.to.poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(forest), 
     #                                    as_points = FALSE, merge = TRUE))


# Reclassify bioclimatic regions for NO, FI, GE
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
forest_df <- as.data.frame(forest_r, xy = TRUE) 

# First increase memory size:
#memory.limit(12000)



# Plot data
# will plot all columns that data hs, need to plot only geomtry
#plot(europe["NAME"], fill = "grey")


# Plot in ggplot2 --------------------------------------------------------------
# shart with ggplot maps here: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# basic plotting
ggplot(data = europe) + 
  geom_sf() #+
#geom_sf(data = nz_height) +
# scale_x_continuous(breaks = c(170, 175))


# plots raster 
windows()
ggplot() +
  geom_stars(data = forest_s, downsample = 10) #+
  scale_fill_manual(low= "white", high = "green")# +
  #coord_fixed() +
  
  

    
    
    

# Use R data for plotting -------------------------------------------------

# Use natural earth data : https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html  
    
    

# Reclassify raster values: now has 0,1,65535 which is NO data, only 1 = forest
#m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
#rclmat <- matrix(m, ncol=3, byrow=TRUE)

#forest[forest == 65535] <- NA
#forest[forest == 0]     <- NA
# get object size
#object.size(forest)

# INcrease memory size?
# Check memory limit: memory.limit() 
# memory.limit(size = 35000) 

# clean garbage in r: gc()

# Make a map -----------------------------------------
# example on ggplot2 & inset map: https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/


# Get color blind palete
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



windows()
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
  #scale_color_brewer(palette="Dark2") + 
  # This boundary settings is mostly rial and error process
  coord_sf(xlim = c(-1000000, 2000000),
           ylim = c( 5000000,  7800000)) + # set zoom as the last one as I am adding new shp
  #ylim = c(45, 72), expand = FALSE
  # theme_void() # # Set a completely blank theme, to get rid of all background and axis elements
  theme_bw() #+
 # ggspatial::annotation_north_arrow()



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
              #location = 'topright',
              scale = 0.2, #  # scale: 0-1, proportion of teh size
              symbol = 3)  # get symbols by northSymbols()
   ggsn::scalebar(data = europe, 
               dist = 100, 
               dist_unit = "km",
               transform = FALSE)



# Plot vector over raster -------------------------------------------------
  




# Get started with tmaps  -------------------------------------------------

# https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#multiple-shapes-and-layers
# http://fisherankney.com/spatial-analysis-greece

# Plot raster data in tmap?

# Shape in tmap is spatial object (class: sf, sp. stars, or raster)
# first define the shape (= layer to plot), then define its characteristics

#library(stars)

library(tmap)



windows()

# set zoom in: define the bounding box
st_bbox(europe)

# Create my own zoom in

# need to create bounding box
# values were guessed from teh graticules values
library(tmaptools)
library(shinyjs)


# vector:                     # xmin,   ymin,    xmax,     ymax
zoom_box = tmaptools::bb(c( -800000,5000000, 1500000, 7800000))


# Expolre palette options
# needs 'shinyjs' library
tmaptools::palette_explorer()

# St up the plot mode: not interactive
tmap_mode("plot")  

tmap_mode("view")  


#eu_map <- 
  tm_shape(europe,
           bbox = zoom_box) + # zoom in europe shp
    
    tm_graticules(col = "grey85",
                  #lty = "dashed",
                 lwd = 0.5) +
    tm_polygons() +
  # Add raster
  tm_shape(forest_r) +
    tm_raster(col = "forest_simple2_int_rcl.tif",  # # name of the raster r after calluing raster(r) function
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
    # Add north ign: no needed if there is graticule
  # tm_compass(#type = "2star",
  #            position = c("left", "top"),
  #            bg.color = "white") + 
  tm_scale_bar(bg.color="white") +
  # Modify the legend
    tm_legend(bg.color="white",
              position = c("left", "top"),
              outside = TRUE) +
    tm_layout(attr.outside=FALSE)




# Get compassd
data(NLD_muni)
qtm(NLD_muni, theme = "NLD") + tm_scale_bar(position=c("left", "bottom"),
                                            bg.color="red",
                                            text.size = 2,
                                            breaks = c(0, 100, 200, 300, 400, 500))



data("World")

tm_shape(World) +
  tm_polygons("gdp_cap_est")


# Make interactive mode of maps:

tmap_mode("view")


tm_shape(World) +
  tm_polygons("HPI")


# end inteactive mode; mode is to plotting
ttm()
tmap_mode("plot")




tm_shape(europe) +
  tm_fill() 

  
  
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

## Not run: 
pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", "#8CBE8C",
           "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", "#9132E6",
           "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", "#FF0000",
           "#FFFFFF", "#5ADCDC")
tm_shape(land) +
  tm_raster("cover", palette = pal20, title = "Global Land Cover") + 
  tm_layout(scale=.8, legend.position = c("left","bottom"))

## End(Not run)



tm_shape(land, ylim = c(-88,88)) +
  tm_raster("trees", palette = "Greens", title = "Percent Tree Cover") +
  tm_shape(World) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom"), bg.color = "lightblue")

## Not run: 
tm_shape(land) +
  tm_raster("black") +
  tm_facets(by="cover_cls")

## End(Not run)

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
