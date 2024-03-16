# create the map of health facilities 


# Load packages -----------------------------------------------------------
pacman::p_load(
  rio, 
  here,
  sf,            ## for working with geospatial data
  ggspatial,     ## for basemaps and north arrows
  raster,        ## for spatial formatting 
  prettymapr,    ## for basemaps
  tidyverse
)

# import data 

kenya_hf <- read_sf(here("Healthcare Facilities", "Healthcare_Facilities.shp"))


shape_plot <- ggplot() + 
  
  ## add the shapefile on top
  geom_sf(data = kenya_hf, 
          fill = NA,         # no fill
          colour = "red") 

############# POINTS  ##########################################################

combined_sf <- kenya_hf %>% 
  filter(`Type`%in%c(
                    "District Hospital",
                    "Health Center",
                    "Medical Center",
                    "Medical Clinic",
                    "Other Hospital",
                    "Provincial General Hospital",
                    "Sub-District Hospital")) %>% 
  drop_na(Latitude, Longitude) %>% 
  st_as_sf(                                               
    # define the coordinates based on lat/long variables
    coords = c("Longitude", "Latitude"),                             
    # set the coordinate reference system to WGS84
    crs = 4326,                                           
    # do not change string variables to factors 
    stringsAsFactors = FALSE                              
  )

# view the first 10 rows, first 5 columns, and the the geometry column
combined_sf$geometry

## plot points on the district shapes
shape_plot + 
  geom_sf(data = combined_sf)+
  labs(title = "Health locations")


## plot points on the district shapes, colored by outcome
shape_plot + 
  geom_sf(data = combined_sf,
          mapping = aes(color = fct_explicit_na(Type)))+
  labs(color = "Type",
       title = "Health facility type and location") + 
  theme_minimal()

############ BASE MAPS #########################################################



# get the bounding box for the shapefile 
bounding_box <- kenya_hf %>% 
  st_bbox()


# plot a base map including scale bar 
basemap <- ggplot() +
  # change the bounding box to an sf object
  # this defines the area to download map tiles for
  geom_sf(data = st_as_sfc(bounding_box)) +
  # download map tiles and add to the plot
  annotation_map_tile(
    # define what map tiles to use
    type =  "cartolight",
    # define folder to store tile images 
    cachedir = here::here("data", "map_tiles"),
    # define if should download tiles each time
    forcedownload = FALSE,
    # hide messages about download status and zoom
    progress = "none" )


# show basemap
basemap

# plot cases on top of basemap
basemap + 
  ## add the shapefile on top
  geom_sf(data = kenya_hf, 
          # no fill
          fill = NA,
          # black borders
          colour = "blue") + 
  geom_sf(data = combined_sf,
          mapping = aes( color =Type),
          size = 0.5) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "bl") +
  labs(fill = "Health facility type",
       title = "Mapping of health facilities in Kenya")




          