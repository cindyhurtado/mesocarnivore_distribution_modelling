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

#Set state space

xlim <- c(0, 205) # real values based on sampling sites
ylim <- c(0, 325) # real values based on sampling sites

#population
M <- 1500
psi <- 0.3 #data augmentation ?
sigma <- 3 #scale parameter 5km approximate movement of bears 

#sampling 
p0.s<-0.3 #detection probability SCR, puntzi lake study
p0.o<-0.1 #Detection probability PA, generally lower than SCR but can use higher too
K <- 4 #sampling occasions/ biweekly? 21 days in our sites

T<-1 # primary sampling periods

# sampling 

J.s<-340 # placing 340 hair traps on a 60 grids of 12km

J.o<-141 # placing 141 camera traps on 44 grids of 12km 

co <- seq((xlim[1]+2*sigma), (xlim[2]-2*sigma), length=sqrt(J.s)) #starting points for the grid

X.s <- cbind(runif(J.s, (xlim[1]+2*sigma), (xlim[2]-2*sigma)),runif(J.s, (xlim[1]+2*sigma), (xlim[2]-2*sigma)))


X.o <-cbind(runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)),runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)))

###data generation ####
simdata <- function(M, psi, p0.s,p0.o, sigma, 
                    xlim, ylim, X.s,X.o, K, T) {
  J.s <- nrow(X.s)   # number of SCR traps
  J.o <- nrow(X.o)   # number of PA traps
  s <- array(NA, c(M, 2, T)) # empty array to fill with activity centers, 300 ind, sampled across 4 periods, 2 columns for coordinates. 
  z <- a <- matrix(NA, M, T) # empty matrix for population membership
  s[,,1] <- cbind(runif(M, xlim[1], xlim[2]), runif(M, ylim[1], ylim[2])) # random activity centers
  z[,1] <- rbinom(M, 1, psi) # create first year's pop with M from psi.
  ##for scr data
  yall.s <- array(0, c(M, J.s, K, T)) # create empty array to put in data
  for(j in 1:J.s) {
    for(k in 1:K) {
      for(t in 1:T) {
        d2.s <- (X.s[j,1] - s[,1, t])^2 + (X.s[j,2] - s[,2, t])^2
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
  # yall <- array(0, c(M, J))
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
nsims <- 3
stub <- "fisher_ICM_chilcotin_newN_"
for(i in 1:nsims) {
  obj.i <- paste("dat.chilcotin_", stub, "_",i, sep="")
  dat.i <- simdata(M=M, psi=psi, #gamma=gamma, phi=phi,
                   p0.s=p0.s, #
                   p0.o=p0.o, #
                   sigma=sigma,
                   xlim=xlim, ylim=ylim, X.s=X.s, X.o=X.o, K=K, T=T)
  assign(obj.i, dat.i)
}

