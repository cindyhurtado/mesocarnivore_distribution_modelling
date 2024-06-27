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

#1. Read and plot layers ####

#grid 
setwd("C:/LocalR")
meso_grid <- st_read("BC_meso_grid.shp")
grid_sf <-  sf::st_as_sf(meso_grid)
grid_columbian <- st_read("BC_meso_grid_columbian.shp")
grid_columbian_sf <-  sf::st_as_sf(grid_columbian)
#grid_columbian_vect <- vect(grid_columbian) # 3005 albers projection
# change projection 
#crslatlong <- "+proj=longlat" 
#grid_columbian_dec <- terra::project(grid_columbian_vect, crslatlong)

#A.  density studies 
setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

df <- read_csv("DNA_data_MDB_11-27b.csv") # file with all density studies 
df$DATA_TYPE <- "DNA"
#df <- subset(df, Project_name != "3289")
df.coords <- dplyr::select(df, x = Longitude_DD, y = Latitude_DD)
crslatlong <- "+proj=longlat" 
DNA_data <- vect(df, geom=c("Longitude_DD", "Latitude_DD"), crs=crslatlong) 
DNA_data_sf <-  sf::st_as_sf(DNA_data, remove= FALSE)
DNA_data_sf_albers <- st_transform(DNA_data_sf, crs=3005)

#B. Camera studies

#check 5937 project as the conversion from UTM to latlong is wrong, zone problem
setwd("I:/Ecosystems/Conservation Science/Species Conservation Science/Mesocarnivores/Projects/Mesocarnivore_Monitoring_Program/2.Data/Mesocarnivores DB/1. Master Data")

cam_df <- read_csv("camera_deployments_01-10.csv")
cam_df$DATA_TYPE <- "CAM"
cam.coords <- dplyr::select(cam_df, x = Longitude_DD, y = Latitude_DD)
Camera_data <- vect(cam_df, geom=c("Longitude_DD", "Latitude_DD"), crs=crslatlong) 
Camera_data_sf <-  sf::st_as_sf(Camera_data)
plot(Camera_data)

Camera_data_sf_albers <- st_transform(Camera_data_sf, crs=3005)

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

#3. Filter by population, cariboo ####

setwd("C:/LocalR")
subpopulations <- sf::st_read("BC_Fisher_populations_2024.gdb", layer = "Subpopulations")

subpop <- ms_simplify(subpopulations, keep = 0.001,
                      keep_shapes = FALSE)

cariboo <- subpop |> dplyr::filter(Subpop == "Cariboo")

cameras_cariboo <- st_intersection(cariboo, sites_cam) # no intersection 
cameras_cariboo_unique <-  cameras_cariboo %>% distinct(WID_12km, .keep_all = TRUE)
DNA_cariboo <- st_intersection(cariboo, DNA_grid) # reducing smapling sites to grid cell of 12km
DNA_cariboo_unique <-  DNA_cariboo %>% distinct(WID_12km, .keep_all = TRUE) # reducing smapling sites to grid cell of 12km
sites_cariboo <- bind_rows(cameras_cariboo_unique, DNA_cariboo_unique)

plot1 = ggplot() +
  #geom_sf(data = grid_sf, fill= NA)+
 # geom_sf(data = DNA_cariboo, color= 'green')+
  geom_sf(data = DNA_cariboo_unique, color= 'red')+
  #geom_sf(data = cameras_grid, color= 'purple')+
 # geom_sf(data = cameras_cariboo, color= 'black') +
  geom_sf(data = cameras_cariboo_unique, color= 'purple')#+
#geom_sf(data = Ssites_cariboo_unique, color= 'red')
plot1

#3. Extract coordinates in Albers 

Ssites_cariboo_albers<-  do.call(rbind, st_geometry(sites_cariboo)) %>%  as_tibble()
Ssites_cariboo2 <- cbind(st_drop_geometry(sites_cariboo), Ssites_cariboo_albers)
#Ssites_cariboo2 <- rename(Ssites_cariboo2, c(Long= V1, Lat= V2))coord.scale
coord.scale <- 1000
traps.scale <- as.data.frame(Ssites_cariboo_albers/coord.scale)

#4. Simulate data for cariboo region at 12km scale ####

# NEED TO SCALE COORDINATES TO REDUCE THE AREA IN THE STATE SPACE, plot X.s and Xo to make sure traps have the same scale ## 

#define statespace, using cariboo subpopulation data 

library(scales)
#coordinates <- select(Ssites_cariboo2, Long, Lat) ## scaled coordinates
plot(traps.scale, xlab='V1', ylab='V2', frame=FALSE, las=1, pch=10, col='#002A64FF', asp=1)
summary(traps.scale)

# Define limits of the state-space and add it to the plot

xlim <- c(min(traps.scale$V1)+2, max(traps.scale$V1)+2)
ylim <- c(min(traps.scale$V2)+2, max(traps.scale$V2)+2)

#xlim <- c(0,100)
#ylim <- c(0,100)


#rect(xlim[1], ylim[1], xlim[2], ylim[2], col=alpha('grey', 0.3), border=NA)

#population
M <- 300 #
psi <- 0.33 #data augmentation ?
gamma <-0.2 # per capita recruitment rate
phi <- 0.8 #survival probability from t-1 to t. 
sigma <- 5 #scale parameter 5km approximate movement of bears 

#sampling 
p0.s<-0.2 #detection probability SCR
p0.o<-0.2 #Detection probability PA, generally lower than SCR but can use higher too
K <- 10 #sampling occasions/ biweekly? 21 days in our sites

T<-4 # primary sampling periods

# sampling 
#cam_sites <-  Ssites_cariboo2 %>% group_by(DATA_TYPE) %>% summarise(n())
#J.s <- 25
J.s<-60 # placing 1761 hair traps on a 60 grids of 12km

#J.o <- 50
J.o<-44 # placing 158 camera traps on 44 grids of 12km 

#co <- seq((xlim[1]+2*sigma), (xlim[2]-2*sigma), length=sqrt(J.s)) #starting points for the grid

#X.s <- cbind(rep(co, each=length(co)), rep(co, times=length(co)))# 5 X 4 grid with spacing of 7.5km or 1.5 X sigma.  

#X.o <-cbind(runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)),runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)))

X.s <- subset(Ssites_cariboo2, DATA_TYPE== "DNA")
X.s <- select(X.s, V1, V2)
X.s <- as.data.frame(X.s/coord.scale)
X.s <- as.matrix(X.s, dim = c(dim(X.s)[1], 2))
X.s <- unname(X.s) # removed this to see if it helps with error

X.o <- subset(Ssites_cariboo2, DATA_TYPE== "CAM")
X.o <- select(X.o, V1, V2)
X.o <- as.data.frame(X.o/coord.scale)
X.o <- as.matrix(X.o, dim = c(dim(X.o)[1], 2))
X.o <- unname(X.o)

#cbind(runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)),runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma))) # 50 random points within the grid. 2*sigma determines what is not included in the sampling area. here its 10 and 90 are the limits, excluding 20% of cells. Thus the sampling area is 64000

###data generation ####

simdata <- function(M, psi, gamma, phi, p0.s,p0.o, sigma, 
                    xlim, ylim, X.s,X.o, K, T) {
  J.s <- nrow(X.s)   # number of SCR traps
  J.o <- nrow(X.o)   # number of PA traps
  s <- array(NA, c(M, 2, T)) # empty array to fill with activity centers, 300 ind, sampled across 4 periods, 2 columns for coordinates. 
  z <- a <- matrix(NA, M, T) # empty matrix for population membership
  s[,,1] <- cbind(runif(M, xlim[1], xlim[2]), runif(M, ylim[1], ylim[2])) # random activity centers
  z[,1] <- rbinom(M, 1, psi) # create first year's pop with M from psi.
  a[,1] <- z[,1]  # recruited in first year if z=1
  EB <- sum(z[,1])*gamma # Expected number of births
  delta <- EB / (M-sum(a[,1])) # Divided by number of available recruits
  if(delta > 1)
    stop("delta > 1")
  for(t in 2:T) {  # for subsequent occasions
    z[,t] <- rbinom(M, 1, z[,t-1]*phi + (1-a[,t-1])*delta) # in population if survived or recruited
    a[,t] <- apply(z[,1:t,drop=FALSE], 1, max) # available for recruitment? 
    if(sum(a[,t]) >= M) # makes sure individuals that exceed M aren't recruited
      stop("A > M")
    EB <- sum(z[,t])*gamma
    delta <- EB / (M - sum(a[,t]))
    if(delta > 1)
      stop("delta > 1")
    s[,1,t] <- s[,1,t-1] # constant activity centers
    s[,2,t] <- s[,2,t-1] # constant activity centers
  }
  
  ##for scr data
  yall.s <- array(0, c(M, J.s, K, T)) # create empty array to put in data
  for(j in 1:J.s) {
    for(k in 1:K) {
      for(t in 1:T) {
        d2.s <- (X.s[j,1] - s[,1,t])^2 + (X.s[j,2] - s[,2,t])^2
        p.s <- p0.s * exp(-d2.s/(2*sigma^2)) #detection prob half normal distrb.
        yall.s[,j,k,t] <- rbinom(M, 1, p.s*z[,t])
      }
    }
  }
  y.s <- yall.s[rowSums(yall.s) > 0,,,] # keep only those individuals that were detected =>1x
  O.s <- ifelse(apply(yall.s, 2:4, sum) > 0, 1, 0) # if detected =>1x, then O=1 
  ##for PA data
  ################
  yall.o <- array(0, c(M, J.o, K, T)) # create empty array to put in data
  # yall <- array(0, c(M, J, T))
  for(j in 1:J.o) {
    for(k in 1:K) {
      for(t in 1:T) {
        d2.o <- (X.o[j,1] - s[,1,t])^2 + (X.o[j,2] - s[,2,t])^2
        p.o <- p0.o * exp(-d2.o/(2*sigma^2))
        yall.o[,j,k,t] <- rbinom(M, 1, p.o*z[,t])
      }
    }
  }
  y.o <- yall.o[rowSums(yall.o) > 0,,,] # keep only those individuals that were detected =>1x
  O.o <- ifelse(apply(yall.o, 2:4, sum) > 0, 1, 0) # if detected =>1x, then O=1 
  return(list(yall.s=yall.s, yall.o=yall.o,y.s=y.s, O.s=O.s,y.o=y.o, O.o=O.o, z=z, s=s, X.s=X.s,X.o=X.o,
              xlims=xlim, ylims=ylim))
}
nsims <- 1
stub <- "test_IOM"
for(i in 1:nsims) {
  obj.i <- paste("dat.cariboo_", stub, "_",i, sep="")
  dat.i <- simdata(M=M, psi=psi, gamma=gamma, phi=phi,
                   p0.s=p0.s, #
                   p0.o=p0.o, #
                   sigma=sigma,
                   xlim=xlim, ylim=ylim, X.s=X.s, X.o=X.o, K=K, T=T)
  assign(obj.i, dat.i)
}






