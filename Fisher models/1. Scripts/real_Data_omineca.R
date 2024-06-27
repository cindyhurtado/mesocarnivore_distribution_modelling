
## . clean data for DNA analysis 
library(terra)
library(ggplot2)
library(sf)
library(readr)
library(farver)
library(ggplot2)
library(rmapshaper)
library(stars)
library(tidyterra)
library(dplyr)
library(rmapshaper)
# X and Y data already obtained on Simulations. 

# Install packages that are not yet installed and load them using Pacman
pacman::p_load(ggplot2, tidyr, dplyr, readxl, knitr, lubridate, tidyverse, lme4, lattice, mgcv, pacman, sf)

process_data = function(){
  #1. Read compiled DNA and Camera files ####
  
  setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")
  
  DNA_data <- read_csv("DNA_data_MDB_03-06.csv",col_types = cols(Start_date = col_date(format = "%Y-%m-%d"), End_date = col_date(format = "%Y-%m-%d"))) %>% drop_na(c(Latitude_DD, Longitude_DD)) %>%  sf::st_as_sf(coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005) %>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])
  
  
  DNA_data$DATA_TYPE <- "DNA"
  
  cam_data <- read_csv("camera_deployments_01-10.csv")%>% drop_na(c(Latitude_DD, Longitude_DD)) %>%  dplyr::rename(Habitat_type= `Habitat type`)%>%  sf::st_as_sf(., coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005)%>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])
  
  academics_data <- read_csv("academics_cam_feb7.csv") %>% dplyr::rename(Latitude_DD= Latitude, Longitude_DD = Longitude) %>% drop_na(c(Latitude_DD, Longitude_DD, End_Deployment_date)) %>%  sf::st_as_sf(., coords= c('Longitude_DD', 'Latitude_DD'), crs= 4326, remove = FALSE ) %>% st_transform( crs=3005)%>% dplyr::mutate(UTM_X = sf::st_coordinates(.)[,1], UTM_Y = sf::st_coordinates(.)[,2])
  
  sites_cam <- bind_rows(academics_data, cam_data) 
  
  sites_cam$DATA_TYPE <- "CAM"
  
  # 2. Read Area of interest- Fisher Columbian Population #### 
  
  setwd("C:/LocalR")
  meso_grid <- st_read("BC_meso_grid.shp")
  grid_columbian <- st_read("BC_meso_grid_columbian.shp")
  columbian_area <- st_union(grid_columbian, by_feature = FALSE) %>% ms_simplify(., keep = 0.01, keep_shapes = FALSE)
  
  setwd("C:/LocalR")
  #setwd("C:/Users/cindy.hurtado/OneDrive - Government of BC/VM")
  subpopulations <- sf::st_read("BC_Fisher_populations_2024.gdb", layer = "Subpopulations")
  
  subpop <- ms_simplify(subpopulations, keep = 0.001,
                        keep_shapes = FALSE)
  
  Omineca <- subpop |> dplyr::filter(Subpop == "Omineca")
  
  
  ## add bufer around area 1km around
  
  #columbian_area <- st_buffer( columbian_area, 1000)
  Omineca <- st_buffer(Omineca, 1000)
  
  # 3. add grid information ####
  
  cameras_grid <- st_intersection(sites_cam, meso_grid) 
  DNA_grid <- st_intersection(DNA_data, meso_grid)
  
  #4. Filter studies by area of interest ####
  
  cameras_grid2 <- st_intersection( cameras_grid, Omineca) 
  DNA_grid2 <- st_intersection( DNA_grid, Omineca)
  
  #5. Filter studies focused on Fisher and remove studies with no animals identified ####
  
  fisher_detections <- DNA_grid2 %>% dplyr::filter(Species== "Pekania pennanti") %>% group_by(Project_name) %>% dplyr::summarise(Fisher_n =n()) # identify what projects to include
  
  summary_ind <- DNA_grid2 %>% filter(Species== "Pekania pennanti") %>% group_by(Project_name) %>% drop_na(Animal_ID) %>% summarise(Fisher_ID_n =n_distinct(Animal_ID))
  summary_ind
  
  DNA_grid_columbian <- DNA_grid2 %>% subset(Project_name == "williston" | Project_name == "Skeena" | Project_name == "JPRF") # studies from 2018 onwards
  
  cameras_grid2 # contains all cameras in columbian area, no need to filter any more. 
  
  # DNA_columbian_3k<-  DNA_grid_columbian %>% group_by(Project_name, Station_ID, .keep_all = TRUE)
  DNA_columbian_3k <- DNA_grid_columbian
  
  #DNA_columbian_3k$Species <- ifelse(DNA_columbian_3k$Species == "Pekania pennanti", 'Pekania pennanti', "NULL")
  
  # list of traps 
  
  #Unique_traps <-  as.data.frame(unique(DNA_columbian_3k$Station_ID))
  #colnames(Unique_traps) <- "Station_ID"
  #View(Unique_traps)
  #Enterprise_matrix2 <- Enterprise_matrix2 %>% drop_na(occasions)
  #k.En <-  max(DNA_columbian_3k$occasion) # only use up to 9 where pekania was detectd
  #J.s.En <-nrow(Unique_traps)  # number of traps
  
  # The hope is to make this into a list of identical tables.
  
  head(DNA_columbian_3k)
  
  # remove animals IDS that are not from Fisher, not filter as we need all stations 
  
  DNA_columbian_3k <- DNA_columbian_3k %>% 
    mutate(
      Animal_ID = case_when(
        Species == "Pekania pennanti" ~ Animal_ID,
        Species != ~ "Pekania pennanti" ~ NA
      )
    )
  
  DNA_columbian_3k <- DNA_columbian_3k %>% 
    dplyr::group_by(Project_name, Station_ID) %>% 
    dplyr::mutate(occasion = row_number(End_date)) %>%
    arrange(Project_name, Station_ID) %>% 
    drop_na(occasion) %>% 
    filter(!occasion %in% c(10,11))
  
  #3. Extract coordinates in Albers 
  coord.scale <- 1000
  
  X.s <- DNA_columbian_3k %>% select(Station_ID, UTM_X, UTM_Y) %>%  st_drop_geometry()
  
  X.s <-X.s[!duplicated(X.s), ]  %>%  ungroup() %>% select(UTM_X,UTM_Y)
  X.s <- X.s/coord.scale
  X.s <- as.matrix(X.s, dim = c(dim(X.s)[1], 2))
  X.s <- unname(X.s) # removed this to see if it helps with error
  
  X.o <- cameras_grid2 %>% select(Station_ID, UTM_X, UTM_Y) %>%  st_drop_geometry()
  
  X.o <-X.o[!duplicated(X.o), ]  %>%  ungroup() %>% select(UTM_X,UTM_Y)
  X.o <- X.o/coord.scale
  X.o <- as.matrix(X.o, dim = c(dim(X.s)[1], 2))
  X.o <- unname(X.o)
  
  #Create Y.s array
  
  # This script is a little exploration of what Cindy wants for the 'DNA_columbian_3k' table.
  
  
  # This script is a little exploration of what Cindy wants for the 'DNA_columbian_3k' table.
  
  # The hope is to make this into a list of identical tables.
  
  head(DNA_columbian_3k)
  
  
  # We need a row for unique animal IDs, columns will be unique station IDs filled by a YES (1) or NO (0)
  # for detections. Note that this is for ALL occasions and thus has some duplicated data in it.
  # It's a preliminary check of the code!
  # DNA_columbian_3k %>% 
  #   dplyr::ungroup() %>% 
  #   sf::st_drop_geometry() %>% 
  #   dplyr::filter(occasion == 1, Animal_ID == '1011') %>% 
  #   dplyr::filter(!duplicated(Animal_ID)) %>% 
  #   dplyr::filter(!is.na(Animal_ID)) %>% 
  #   dplyr::select(Animal_ID, Station_ID, Species) %>% 
  #   tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') %>% 
  #   # Depending on the species that fills in each cell, let's replace Pekania p. with 1, and everything 
  #   # else we'll replace with 0.
  #   dplyr::mutate(across(-Animal_ID, \(x) dplyr::case_when(
  #     is.na(x) ~ 0,
  #     x == 'Pekania pennanti' ~ 1,
  #     T ~ 0)))
  
  # This is the full 'framework' of the matrix - it has ALL stations and animal IDs.
  matrix_frame = DNA_columbian_3k %>% 
    ungroup() %>%
    sf::st_drop_geometry() %>% 
    dplyr::mutate(st_filler = 'goop') %>% 
    dplyr::select(Station_ID, Animal_ID, st_filler) %>%
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = Station_ID, values_from = st_filler, names_prefix = 'st_') %>% 
    dplyr::filter(!duplicated(Animal_ID)) %>%
    dplyr::filter(!is.na(Animal_ID))
  
  n.ind.En <- n_distinct(na.omit(matrix_frame$Animal_ID)) 
  
  # tbl_to_join = matrix_frame %>% 
  #   dplyr::select(-all_of(stations_already_in_this_occasion)) %>% 
  #   dplyr::mutate(across(-Animal_ID, \(x) x = NA))
  
  # For each 'occasion', make a table showing 1's and 0's by unique animal ID and stations as columns.
  tbls_by_occasion_list = 1:7 %>% 
    purrr::map( ~ {
      dat_for_occ = DNA_columbian_3k %>% 
        dplyr::filter(occasion == .x) %>% 
        dplyr::ungroup() %>% 
        sf::st_drop_geometry() %>% 
        dplyr::filter(!duplicated(Animal_ID)) %>% 
        dplyr::filter(!is.na(Animal_ID)) %>% 
        dplyr::select(Animal_ID, Station_ID, Species) %>% 
        tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') %>% 
        # Depending on the species that fills in each cell, let's replace Pekania p. with 1, and everything 
        # else we'll replace with 0.
        dplyr::mutate(dplyr::across(-Animal_ID, \(x) dplyr::case_when(
          is.na(x) ~ 0,
          x == 'Pekania pennanti' ~ 1,
          T ~ 0)))
      
      stations_already_in_this_occasion = names(dat_for_occ[,-1])
      
      tbl_to_join = matrix_frame %>% 
        dplyr::select(-all_of(stations_already_in_this_occasion)) %>% 
        dplyr::mutate(dplyr::across(-Animal_ID, \(x) x = 0))
      
      dat_for_occ %>% 
        dplyr::left_join(tbl_to_join) %>% 
        dplyr::bind_rows(tbl_to_join[!tbl_to_join$Animal_ID %in% dat_for_occ$Animal_ID,]) %>% 
        dplyr::mutate(dplyr::across(-Animal_ID, \(x) ifelse(is.na(x), 0, x)))
    })
  
  names(tbls_by_occasion_list) <- paste0('occ_',c(1:7))
  
  tbls_by_occasion_list$occ_1
  # tbls_by_occasion_list$occ_8
  tbls_by_occasion_list$occ_9
  
  # Let's reorder the stations for this output based on the order in the 'matrix_frame'
  # we made; the same thing for the order of rows by Animal_ID.
  all_dat = tbls_by_occasion_list %>% 
    dplyr::bind_rows(.id = 'occasion') %>% 
    dplyr::arrange(occasion,Animal_ID) %>% 
    dplyr::select(occasion, Animal_ID, names(matrix_frame)[-1])
  
  # Please use all_dat going forward!
  
  k.En <- 9
  J.s.En <-length(colnames(matrix_frame))-1 # not counting Animal ID
  ## create Y.s 
  Y.s <- array(0, c(n.ind.En, J.s.En, k.En)) 
  
  all_dat %>% 
    tidyr::pivot_longer(cols = -c(occasion, Animal_ID)) %>%  
    dplyr::group_by(Animal_ID) %>% 
    dplyr::summarise(total_value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(total_value != 0)
  # All 110 unique Animal_IDs have been detected at least once across the occasions and stations.
  
  m_occ1 <- all_dat %>% filter(occasion== "occ_1") %>% select(-c(Animal_ID, occasion))
  m_occ2 <- all_dat %>% filter(occasion== "occ_2") %>% select(-c(Animal_ID, occasion))
  m_occ3 <- all_dat %>% filter(occasion== "occ_3") %>% select(-c(Animal_ID, occasion))
  m_occ4 <- all_dat %>% filter(occasion== "occ_4") %>% select(-c(Animal_ID, occasion))
  m_occ5 <- all_dat %>% filter(occasion== "occ_5") %>% select(-c(Animal_ID, occasion))
  m_occ6 <- all_dat %>% filter(occasion== "occ_6") %>% select(-c(Animal_ID, occasion))
  m_occ7 <- all_dat %>% filter(occasion== "occ_7") %>% select(-c(Animal_ID, occasion))
  m_occ8 <- all_dat %>% filter(occasion== "occ_8") %>% select(-c(Animal_ID, occasion))
  m_occ9 <- all_dat %>% filter(occasion== "occ_9") %>% select(-c(Animal_ID, occasion))
  
  Y.s[,,1] <- array(data=unlist(m_occ1), dim= c(110, 1312))# remove Animal_ID
  Y.s[,,2] <- array(data = unlist(m_occ2), dim= c(110, 1312))
  Y.s[,,3] <- array(data = unlist(m_occ3), dim= c(110, 1312))
  Y.s[,,4] <- array(data = unlist(m_occ4), dim= c(110, 1312))
  Y.s[,,5] <- array(data = unlist(m_occ5), dim= c(110, 1312))
  Y.s[,,6] <- array(data = unlist(m_occ6), dim= c(110, 1312))
  Y.s[,,7] <- array(data = unlist(m_occ7), dim= c(110, 1312))
  Y.s[,,8] <- array(data = unlist(m_occ8), dim= c(110, 1312))
  Y.s[,,9] <- array(data = unlist(m_occ9), dim= c(110, 1312))
  # 
  # class(Y.s) <- "numeric"
  
  # Y.s[is.na(Y.s)] <- 0
  
  Y.s.enterprise <- Y.s
  
  # Define limits of the state-space and add it to the plot
  
  traps.scale <- rbind(X.o,X.s)
  
  library(scales)
  #coordinates <- select(Ssites_cariboo2, Long, Lat) ## scaled coordinates
  plot(traps.scale, xlab='V1', ylab='V2', frame=FALSE, las=1, pch=10, col='#002A64FF', asp=1)
  summary(traps.scale)
  xlim <- c(min(traps.scale[,1])+2, max(traps.scale[,1])+2)
  ylim <- c(min(traps.scale[,2])+2, max(traps.scale[,2])+2)
  
  
  #Do we need to make all NA to 0? NA shows trap not active
  
  # Session1_final[is.na(Session1_final)] <- 0
  # Session2_final[is.na(Session2_final)] <- 0
  # Session3_final[is.na(Session3_final)] <- 0
  
  
  # prueba <- colSums(Session1_final[,-1 ])
  # prueba2 <- colSums(Session2_final[,-1 ])
  # prueba3 <- colSums(Session3_final[,-1 ])
  
  
  ## GET OPERABILITY MATRIX
  
  Unique_traps <-  as.data.frame(unique(DNA_columbian_3k$Station_ID))
  colnames(Unique_traps) <- "Station_ID"
  
  Enterprise_matrix2 <- DNA_columbian_3k
  
  session1_trap <- Enterprise_matrix2 %>% subset(occasion==1) %>% dplyr::rename(Occ_1= occasion)
  session2_trap <- Enterprise_matrix2 %>% subset(occasion==2) %>%  dplyr::rename(Occ_2= occasion)
  session2_trap$Occ_2 <- 1
  
  session3_trap <- Enterprise_matrix2 %>% subset(occasion==3) %>% dplyr::rename(Occ_3= occasion)
  session3_trap$Occ_3 <- 1
  
  session4_trap <- Enterprise_matrix2 %>% subset(occasion==4) %>%  dplyr::rename(Occ_4= occasion)
  session4_trap$Occ_4 <- 1
  
  session5_trap <- Enterprise_matrix2 %>% subset(occasion==5) %>% dplyr::rename(Occ_5= occasion)
  session5_trap$Occ_5 <- 1
  
  
  O.s_df <- dplyr::left_join(Unique_traps, session1_trap) %>% dplyr::left_join(session2_trap) %>% dplyr::left_join(session3_trap, by= "Station_ID")%>% dplyr::left_join(session4_trap) %>% dplyr::left_join(session5_trap, by= "Station_ID") #%>% replace(is.na(.), 0) 
  
  
  O.s <- as.matrix(O.s_df[,-1]) ## operability matrix
  #O.s[is.na(O.s)] <- 0
  
  # colnames(Session1_final) <- NULL
  # colnames(Session2_final) <- NULL
  # colnames(Session3_final) <- NULL
  
  # Y.s[,,1] <- Session1_final[ ,-1]* session1_operability
  # Y.s[,,2] <- Session2_final[ ,-1]* session2_operability
  # Y.s[,,3] <- as.matrix((Session3_final[ ,-1]))* session3_operability
  
  # Maybe then add up based on grid, to have more recaptures in the same session in different traps?
  
  
  
  ## camera trap formating 
  
  
  # 9. Create O.o array for cameras ####
  
  #9.1. First determine when cameras where active 
  
  cameras_grid <- st_drop_geometry(cameras_grid2) %>% drop_na(Start_Deployment_date) # contains all cameras in columbian area, no need to filter any more. From steps 1-4 
  
  #2.4 Organize date columns
  
  cameras_grid$Start_Deployment_date <- ymd(cameras_grid$Start_Deployment_date)
  cameras_grid$End_Deployment_date <- ymd(cameras_grid$End_Deployment_date)
  cameras_grid$Cam_days <- interval(cameras_grid$Start_Deployment_date, cameras_grid$End_Deployment_date)/ddays(1)
  # this does not include the first day, different from operation matrix
  
  summary(cameras_grid$Cam_days)
  
  #remove any deployments without end dates
  
  tmp <- cameras_grid[is.na(cameras_grid$End_Deployment_date)==F,]
  
  #List by project to reduce NAs and stack days
  
  tmp.list <- split(tmp, f= tmp$Project_name)
  
  # wrapper <- function(df) {
  #   as.data.frame %>% list %>% return
  # }
  # cameras_grid_list <-  cameras_grid %>% group_by(Project_name) %>%
  #   do(res = wrapper(.)) 
  
  # Create an empty list to store our days, all projects have at least one individual identified, so empty list will always be filled with something. 
  
  length(tmp.list)
  
  daily_lookup <- list()
  daily_lookup2 <- list()
  daily_lookup3 <- list()
  daily_lookup4 <- list()
  daily_lookup5 <- list()
  daily_lookup6 <- list()
  daily_lookup7 <- list()
  
  #Cam operation for project 1
  
  for(i in 1:nrow(tmp.list[[1]])) {
    if(ymd(tmp.list[[1]]$Start_Deployment_date[i])!=ymd(tmp.list[[1]]$End_Deployment_date[i]))
    {
      daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp.list[[1]]$Start_Deployment_date[i]), ymd(tmp.list[[1]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[1]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup1<- bind_rows(daily_lookup)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup1 <- row_lookup1[duplicated(row_lookup1)==F,]
  
  # create operation matrix
  library(reshape2)
  row_lookup1$active <- 1
  CamOp1 <- dcast(row_lookup1, Station_ID ~ date, value.var = "active")
  # names1 <- c(paste0("V", 1:ncol(CamOp1)))
  # colnames(CamOp1) <- names1 # dont change the names yet cos you need this for a native join 
  # 
  #Cam operation for project 2
  
  for(i in 1:nrow(tmp.list[[2]])) {
    if(ymd(tmp.list[[2]]$Start_Deployment_date[i])!=ymd(tmp.list[[2]]$End_Deployment_date[i]))
    {
      daily_lookup2[[i]] <- data.frame("date"=seq(ymd(tmp.list[[2]]$Start_Deployment_date[i]), ymd(tmp.list[[2]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[2]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup2<- bind_rows(daily_lookup2)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup2 <- row_lookup2[duplicated(row_lookup2)==F,]
  
  # create operation matrix
  library(reshape2)
  row_lookup2$active <- 1
  CamOp2 <- dcast(row_lookup2, Station_ID ~ date, value.var = "active")
  # names2 <- c(paste0("V", 1:ncol(CamOp2)))
  # colnames(CamOp2) <- names2
  
  #Cam operation for project 3
  
  for(i in 1:nrow(tmp.list[[3]])) {
    if(ymd(tmp.list[[3]]$Start_Deployment_date[i])!=ymd(tmp.list[[3]]$End_Deployment_date[i]))
    {
      daily_lookup3[[i]] <- data.frame("date"=seq(ymd(tmp.list[[3]]$Start_Deployment_date[i]), ymd(tmp.list[[3]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[3]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup3<- bind_rows(daily_lookup3)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup3 <- row_lookup3[duplicated(row_lookup3)==F,]
  
  # create operation matrix
  row_lookup3$active <- 1
  CamOp3 <- dcast(row_lookup3, Station_ID ~ date, value.var = "active")
  # names3 <- c(paste0("V", 1:ncol(CamOp3)))
  # colnames(CamOp3) <- names3
  
  #Cam operation for project 4
  
  for(i in 1:nrow(tmp.list[[4]])) {
    if(ymd(tmp.list[[4]]$Start_Deployment_date[i])!=ymd(tmp.list[[4]]$End_Deployment_date[i]))
    {
      daily_lookup4[[i]] <- data.frame("date"=seq(ymd(tmp.list[[4]]$Start_Deployment_date[i]), ymd(tmp.list[[4]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[4]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup4<- bind_rows(daily_lookup4)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup4 <- row_lookup4[duplicated(row_lookup4)==F,]
  
  # create operation matrix
  row_lookup4$active <- 1
  CamOp4 <- dcast(row_lookup4, Station_ID ~ date, value.var = "active")
  # names4 <- c(paste0("V", 1:ncol(CamOp4)))
  # colnames(CamOp4) <- names4
  
  #Cam operation for project 5
  
  for(i in 1:nrow(tmp.list[[5]])) {
    if(ymd(tmp.list[[5]]$Start_Deployment_date[i])!=ymd(tmp.list[[5]]$End_Deployment_date[i]))
    {
      daily_lookup5[[i]] <- data.frame("date"=seq(ymd(tmp.list[[5]]$Start_Deployment_date[i]), ymd(tmp.list[[5]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[5]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup5<- bind_rows(daily_lookup5)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup5 <- row_lookup5[duplicated(row_lookup5)==F,]
  
  # create operation matrix
  row_lookup5$active <- 1
  CamOp5 <- dcast(row_lookup5, Station_ID ~ date, value.var = "active")
  # names5 <- c(paste0("V", 1:ncol(CamOp5)))
  # colnames(CamOp5) <- names5
  
  #Cam operation for project 6
  
  for(i in 1:nrow(tmp.list[[6]])) {
    if(ymd(tmp.list[[6]]$Start_Deployment_date[i])!=ymd(tmp.list[[6]]$End_Deployment_date[i]))
    {
      daily_lookup6[[i]] <- data.frame("date"=seq(ymd(tmp.list[[6]]$Start_Deployment_date[i]), ymd(tmp.list[[6]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[6]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup6<- bind_rows(daily_lookup6)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup6 <- row_lookup6[duplicated(row_lookup6)==F,]
  
  # create operation matrix
  library(reshape2)
  row_lookup6$active <- 1
  CamOp6 <- dcast(row_lookup6, Station_ID ~ date, value.var = "active")
  # names6 <- c(paste0("V", 1:ncol(CamOp6)))
  # colnames(CamOp6) <- names6
  
  #Cam operation for project 7
  
  for(i in 1:nrow(tmp.list[[7]])) {
    if(ymd(tmp.list[[7]]$Start_Deployment_date[i])!=ymd(tmp.list[[7]]$End_Deployment_date[i]))
    {
      daily_lookup7[[i]] <- data.frame("date"=seq(ymd(tmp.list[[7]]$Start_Deployment_date[i]), ymd(tmp.list[[7]]$End_Deployment_date[i]),  by= "days"), "Station_ID"=tmp.list[[7]]$Station_ID[i])}
  }
  
  # Merge the lists into a dataframe
  row_lookup7<- bind_rows(daily_lookup7)
  
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup7 <- row_lookup7[duplicated(row_lookup7)==F,]
  
  # create operation matrix
  row_lookup7$active <- 1
  CamOp7 <- dcast(row_lookup7, Station_ID ~ date, value.var = "active")
  # names7 <- c(paste0("V", 1:ncol(CamOp7)))
  # colnames(CamOp7) <- names7
  
  # list all matrices
  
  CamOpT <- list(CamOp1, CamOp2, CamOp3, CamOp4, CamOp5, CamOp6, CamOp7)
  
  
  for(i in 1:length(CamOpT)){
    
    CamOpT[[i]][CamOpT[[i]]==1] <- 0
  }
  
  # 9.2. Create occupancy matrix = O.o ###
  
  library(camtrapR)
  library(tidyverse)
  library(gridExtra)
  library(cowplot)
  
  # 2. Read in species data  
  
  setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")
  
  cam_sp <- read_csv("camera_species_11-02c.csv", col_types = cols(Date_Time = col_datetime()))
  
  academics_sp <- read_csv("academics_sp_feb7.csv", col_types = cols(Date_Time = col_datetime())) 
  
  sp_cam <- bind_rows(cam_sp, academics_sp) 
  
  sp_cam <- as.data.frame(sp_cam)
  
  # filter only the projects to use based on location
  
  sp_cam <- sp_cam %>% subset(Project_name== "South Chilcotin Mountains Camera Sampling" | Project_name == "Interior BC Moose & Predator Camera Study" | Project_name == "Itcha Ilgachuz"     | Project_name == "BC_SIMDeer" | Project_name == "Fisher Exclusion Box Program" | Project_name ==  "5234" | Project_name ==  "6115") # s
  
  # Check/Edit species names 
  
  unique(sp_cam$Species)
  
  # sp_cam$Species <-  recode(culverts_2023_2020$Species, `Small Rodent (Unknown spp)` = "Small Rodents", `Other` = "Birds",`Short-tailed Weasel (Ermine)`= "Short-tailed Weasel", `Shrew` = "Small Rodents", `Small Rodent-like (Unknown spp)`= "Small Rodents", `Bird`= "Birds")
  
  # Convert DateTime to character to run next script
  
  sp_cam$Date_Time2 <- as.character(sp_cam$Date_Time)
  
  sp_cam <- sp_cam %>% drop_na(Date_Time2) 
  
  # # 3. Create table with independent events 
  out_dir<- "I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data"
  
  Inp_eventscam <-  filterRecordTable(
    sp_cam,
    minDeltaTime = 24*59,
    deltaTimeComparedTo= "lastIndependentRecord",
    speciesCol = "Species",
    stationCol= "Station_ID",
    camerasIndependent = FALSE,
    recordDateTimeCol = "Date_Time2",
    recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
    removeDuplicateRecords = TRUE,
    # exclude= c("Unknown", "Other", "Setup"),
    timeZone= "US/Pacific",
    writecsv = TRUE,
    outDir= out_dir
  )
  
  ## filter by species
  
  fisher_cam <- subset(Inp_eventscam, Species== "Pekania pennanti")
  
  # generate a list making each project an element of the list. 
  
  tmp_cam <- split(Inp_eventscam, f= Inp_eventscam$Project_name)
  length(tmp_cam)  ## all detections
  
  tmp_fisher_cam <- split(fisher_cam, f= fisher_cam$Project_name) ## only fisher detections
  length(tmp_fisher_cam) # some projects had no fisher detections, make those 0 for all active days
  
  
  # generate the detection matrix of fisher camera detections
  
  fisher_cam_matrix <- list()
  
  for (i in 1:length(tmp_fisher_cam)){
    
    fisher_cam_matrix[[i]] <- tmp_fisher_cam[[i]]  %>% select(Station_ID, Date, Count) %>%  pivot_wider(id_cols = Station_ID, names_from = Date, values_from = Count)
    
  }
  
  
  # populate empty with fisher table
  
  for(i in 1:length(CamOpT)){
    CamOpT[[i]] <- CamOpT[[i]] %>% remove_rownames %>% column_to_rownames(var="Station_ID")
  }
  
  for(i in 1:length(fisher_cam_matrix)){
    fisher_cam_matrix[[i]] <- fisher_cam_matrix[[i]] %>% remove_rownames %>% column_to_rownames(var="Station_ID")
  }
  
  #we create a new matrix as big as CamOpT with the values of m.2 in it
  
  # loop didnt work because of problem with one of the matrices having different dimensions. Try again. 
  # 
  # mres <- list()
  # cam_matrix <- list()
  # 
  # for( i in 1:length(fisher_cam_matrix)){
  #   for(j in 3:length(CamOpT)){
  #     
  #     mres[[i]]<-CamOpT[[j]]
  #     mres[[i]][rownames(fisher_cam_matrix[[i]]),colnames(fisher_cam_matrix[[i]])]<-fisher_cam_matrix[[i]]
  #     #Then we use pmax
  #     cam_matrix[[i]] <- pmax(CamOpT[[j]],mres[[i]],na.rm=FALSE)
  #   }
  #   
  # }
  
  mres3<-CamOpT[[3]]
  mres3[rownames(fisher_cam_matrix[[1]]),colnames(fisher_cam_matrix[[1]])]<-fisher_cam_matrix[[1]]
  #Then we use pmax
  cam_matrix_3 <- pmax(CamOpT[[3]],mres3,na.rm=FALSE)
  names3 <- c(paste0("V", 1:ncol(cam_matrix_3)))
  colnames(cam_matrix_3) <- names3
  
  mres4<-CamOpT[[4]]
  mres4[rownames(fisher_cam_matrix[[2]]),colnames(fisher_cam_matrix[[2]])]<-fisher_cam_matrix[[2]]
  #Then we use pmax
  cam_matrix_4 <- pmax(CamOpT[[4]],mres4,na.rm=FALSE)
  names4 <- c(paste0("V", 1:ncol(cam_matrix_4)))
  colnames(cam_matrix_4) <- names4
  
  mres5<-CamOpT[[5]]
  mres5[rownames(fisher_cam_matrix[[3]]),colnames(fisher_cam_matrix[[3]])]<-fisher_cam_matrix[[3]]
  #Then we use pmax
  cam_matrix_5 <- pmax(CamOpT[[5]],mres5,na.rm=FALSE)  ## one camera with a fisher detectoin was removed from the locations, based on the map?
  names5 <- c(paste0("V", 1:ncol(cam_matrix_5)))
  colnames(cam_matrix_5) <- names5
  
  mres6<-CamOpT[[6]]
  mres6[rownames(fisher_cam_matrix[[4]]),colnames(fisher_cam_matrix[[4]])]<-fisher_cam_matrix[[4]]
  #Then we use pmax
  cam_matrix_6 <- pmax(CamOpT[[6]],mres6,na.rm=FALSE)
  names6 <- c(paste0("V", 1:ncol(cam_matrix_6)))
  colnames(cam_matrix_6) <- names6
  
  
  mres7<-CamOpT[[7]]
  mres7[rownames(fisher_cam_matrix[[5]]),colnames(fisher_cam_matrix[[5]])]<-fisher_cam_matrix[[5]]
  #Then we use pmax
  cam_matrix_7 <- pmax(CamOpT[[7]],mres7,na.rm=FALSE)
  names7 <- c(paste0("V", 1:ncol(cam_matrix_7)))
  colnames(cam_matrix_7) <- names7
  
  ## Remove the names of all columns and combine all matrices 
  
  names1 <- c(paste0("V", 1:ncol(CamOpT[[1]])))
  colnames(CamOpT[[1]]) <- names1
  
  names2 <- c(paste0("V", 1:ncol(CamOpT[[2]])))
  colnames(CamOpT[[2]]) <- names2
  
  ### merge all camera operation matrices
  library(plyr)
  O.o<- bind_rows(CamOpT[[1]], CamOpT[[2]], mres3, mres4, mres5, mres6, mres7)
  View(O.o)
  dim(O.o)
  
  O.o3 <- replace(O.o, is.na(O.o), 0)
  
  O.o2 <- as.matrix(O.o[])  
  rownames(O.o2) <- NULL
  
  # Add an extra dimension of size 1
  O <- array(0, c(dim(O.o)[1], dim(O.o3)[2], T)) 
  
  #namesEnterprise <- c(paste0("V", 1:J.s.En))
  colnames(O[,,1]) <-NULL
  
  O[,,1] <- as.matrix(O.o3)
  
  
  ## other variables needed for the model 
  M <- 1500
  T=1
  psi <- 0.3
  
  s <- array(NA, c(M, 2, T)) # empty array to fill with activity centers, 300 ind, sampled across 4 periods, 2 columns for coordinates. 
  z <- a <- matrix(NA, M, T) # empty matrix for population membership
  s[,,1] <- cbind(runif(M, xlim[1], xlim[2]), runif(M, ylim[1], ylim[2])) # random activity centers
  z[,1] <- rbinom(M, 1, psi) # create first year's pop with M from psi.
  ##
  dim(s)
  
  dim(Y.s)
  # We'll need to put the things we want exported from inside the function
  # into a 'return' statement. Ideally, we should name each of the exports.
  return(c(s,Z,Y.s = 'Y.s'))
}

# Run the function!
results = process_data()

