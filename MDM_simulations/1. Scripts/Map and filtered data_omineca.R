#Omineca map

setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

DNA_data <- read_csv("DNA_data_MDB_03-06.csv",col_types = cols(Start_date = col_date(format = "%Y-%m-%d"), End_date = col_date(format = "%Y-%m-%d"))) %>% drop_na(c(Latitude_DD, Longitude_DD)) %>%  sf::st_as_sf(coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005) %>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])


DNA_data$DATA_TYPE <- "DNA"

cam_data <- read_csv("camera_deployments_01-10.csv")%>% drop_na(c(Latitude_DD, Longitude_DD)) %>%  dplyr::rename(Habitat_type= `Habitat type`)%>%  sf::st_as_sf(., coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005)%>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])

academics_data <- read_csv("academics_cam_feb7.csv") %>% dplyr::rename(Latitude_DD= Latitude, Longitude_DD = Longitude) %>% drop_na(c(Latitude_DD, Longitude_DD, End_Deployment_date)) %>%  sf::st_as_sf(., coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005)%>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])

sites_cam <- bind_rows(academics_data, cam_data) 

sites_cam$DATA_TYPE <- "CAM"

# 2. Read Area of interest- Fisher in Omineca #### 

setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Colaborations_Data_Shared/Omineca- WHA")

omineca <- st_read("polygon_aoi.shp")

# 3. Filter studies by area of interest ####

cameras_grid <- st_intersection(sites_cam, omineca) 
DNA_grid <- st_intersection(DNA_data, omineca)

#4. Filter studies focused on Fisher and remove studies with no animals identified ####

fisher_detections <- DNA_grid %>% dplyr::filter(Species== "Pekania pennanti") %>% group_by(Project_name) %>% dplyr::summarise(Fisher_n =n()) # identify what projects to include

DNA_grid_omineca <- DNA_grid %>% subset(Project_name == "Omineca-Lower Chilako" | Project_name == "JPRF") # studies from 2018 onwards

unique(cameras_grid$Project_name) # contains all cameras in columbian area, no need to filter any more. 

# read fisher detections from cameras

setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

all_sp_detections <- read_csv("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data/record_table_1416min_deltaT_2024-04-23.csv")

fisher_cam <- subset(all_sp_detections, Species== "Pekania pennanti")
fisher_cam <- subset(fisher_cam, Project_name == "Interior BC Moose & Predator Camera Study" | Project_name == "Fisher Exclusion Box Program" )

fisher_cam2 <- merge(fisher_cam, cameras_grid, by=c("Project"  "Station_ID")
