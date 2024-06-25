library(terra)
library(ggplot2)
library(sf)
library(readr)
library(farver)
library(rmapshaper)
library(stars)
library(tidyterra)
library(dplyr)
library(timechange)
library(tmap)

#1. Read and plot layers ####

#grid 
setwd("C:/LocalR")
meso_grid <- st_read("BC_meso_grid.shp")
grid_columbian <- st_read("BC_meso_grid_columbian.shp")
columbian_area <- ms_simplify(grid_columbian, keep = 0.01,
                              keep_shapes = FALSE)

#A.  density studies 
setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

df <- read_csv("DNA_data_MDB_11-27b.csv") # file with all density 
df$DATA_TYPE <- "DNA"
df <-  df %>% drop_na(c(Latitude_DD, Longitude_DD))

DNA_data_sf <-  sf::st_as_sf(df, coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326 ) %>% st_transform( crs=3005)

#only one coordinate per station per grid.

DNA_short <- DNA_data_sf %>% group_by(Project_name, Station_ID) %>% 
summarise(n = n(), .groups="drop")

bc_bound = bcmaps::bc_bound()
regs = bcmaps::nr_regions()

ggplot() +
  geom_sf(data = bc_bound)+
  geom_sf(data = DNA_data_sf, size= 0.3, color= 'red')+
  geom_sf(data = DNA_short, size= 0.1)

data("World")
tmap_mode("view")
tm_shape(World) +
  tm_polygons("HPI")

tm_shape(meso_grid) +
  tm_polygons("Wgrid") +
tm_shape(DNA_grid) +
  tm_symbols(col= "red", scale = 0.05)


# Intersect grid with points to obtain geometry of grids 

DNA_grid <- st_intersection(DNA_short, meso_grid)

ggplot() +
  geom_sf(data = bc_bound)+
  geom_sf(data = DNA_grid)

#B. Camera studies

#check 5937 project as the conversion from UTM to latlong is wrong, zone problem
setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

cam_df <- read_csv("camera_deployments_01-10.csv")
cam_df$DATA_TYPE <- "CAM"

Cam_data_sf <-  sf::st_as_sf(cam_df, coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326 ) %>% st_transform( crs=3005)

#C. academics camera studies
Academics_cam <- read_csv("academics_cam_feb7.csv")
# still working on final file 
Academics_cam$DATA_TYPE <- "CAM"
MDM_academics_plot <- vect(Academics_cam, geom=c("Longitude", "Latitude"), crs=crslatlong)
MDM_academics_sf <-  sf::st_as_sf(MDM_academics_plot)
MDM_academics_sf_albers <- st_transform(MDM_academics_sf, crs=3005)

# 2. Assigned grid number to each sampling site and group by grid ####

cameras_grid <- st_intersection(Camera_data_sf_albers, grid_sf)
#cameras_grid3 <- st_collection_extract(cameras_grid, "POLYGON")
#intersections_lp <- st_intersection(Camera_data_sf_albers, grid_columbian_sf)
academics_grid <- st_intersection(MDM_academics_sf_albers, grid_sf)
DNA_grid <- st_intersection(DNA_data_sf_albers, grid_sf)

sites_cam <- bind_rows(academics_grid, cameras_grid) 

#sites_cam %>%
#  group_by(across(-WID_12km)) %>%
#  summarise(n = n(), .groups="drop")

# 2A: Kootenays station missing when intersected with camera grid, fix later same with academics 4 stations missing### ####
# no data on cameras

Prueba <- DNA_grid %>% #camera grid, 
  group_by(Project_name) %>%
  tally()

project_scale <- DNA_grid %>% filter(Project_name == '3269')
zoom_to <- c(1336770, 733880.1)  



Prueba_plot = ggplot() +
  geom_sf(data = grid_sf, fill= NA)+
  geom_sf(data = project_scale, color= 'green')+
  ggplot2::coord_sf(crs = st_crs(3005), xlim = c(115000, 1325000),  
                    ylim = c(580000, 740000),  
                    expand = FALSE)
  #coord_sf(datum = st_crs(3005))
  #geom_sf(data = DNA_grid, color= 'red')+
  #coord_sf(crs = 3005)
  
+
  coord_sf(crs = 3005)
  #geom_sf(data = bc_bound, color= 'black', fill= NA) +
  geom_sf(data = all, color= 'yellow')
Prueba_plot

Prueba2 <- DNA_data_sf_albers %>% #camera_data_sf_albers
  group_by(Project_name) %>%
  tally()

all <- DNA_data_sf_albers %>%  # must be 0
  filter(!DNA_data_sf_albers$Station_ID %in% DNA_grid$Station_ID)
all

plot

missing_CAM = ggplot() +
  geom_sf(data = grid_sf, fill= NA)+
  geom_sf(data = DNA_data_sf_albers, color= 'green')+
  #geom_sf(data = bc_bound, color= 'black', fill= NA) +
  geom_sf(data = all, color= 'yellow')
missing_CAM


#3. Filter by columbian population ####

cameras_columbian <- st_intersection(columbian_area, sites_cam) # no intersection 
cameras_columbian_unique <-  cameras_columbian %>% distinct(WID_12km, .keep_all = TRUE)
DNA_columbian <- st_intersection(columbian_area, DNA_grid) # reducing smapling sites to grid cell of 12km
DNA_columbian_unique <-  DNA_columbian %>% distinct(WID_12km, .keep_all = TRUE) # reducing smapling sites to grid cell of 12km
sites_columbian <- bind_rows(cameras_columbian_unique, DNA_columbian_unique)
