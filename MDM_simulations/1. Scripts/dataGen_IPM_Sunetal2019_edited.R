###################
##data parameters
###################

#statespace, square 10 000km2
xlim <- c(0, 100)
ylim <- c(0, 100)

#population
M <- 300 #
psi <- 0.33 #data augmentation ?
gamma <-0.2 # per capita recruitment rate
phi <- 0.8 #survival probability from t-1 to t. 
sigma <- 5 #scale parameter 5km approximate movement of bears

#sampling 
p0.s<-0.5 #detection probability SCR
p0.o<-0.2 #Detection probability PA, generally lower than SCR but can use higher too
K <- 10 #sampling occasions/ biweekly? 21 days in our sites

T<-4 # primary sampling periods

J.s<-25 # placing 25 hair traps on a grid of 900km2
J.o<-50 # placing 50 camera traps on a grid of 900km2

co <- seq((xlim[1]+7*sigma), (xlim[2]-7*sigma), length=sqrt(J.s)) #starting points for the grid

X.s <- cbind(rep(co, each=length(co)), rep(co, times=length(co)))# 5 X 4 grid with spacing of 7.5km or 1.5 X sigma.  

X.o <-cbind(runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma)),runif(J.o, (xlim[1]+2*sigma), (xlim[2]-2*sigma))) # 50 random points within the grid. 2*sigma determines what is not included in the sampling area. here its 10 and 90 are the limits, excluding 20% of cells. Thus the sampling area is 64000

####################
###data generation
####################

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
stub <- "test_IM"
for(i in 1:nsims) {
  obj.i <- paste("dat.", stub, "_",i, sep="")
  dat.i <- simdata(M=M, psi=psi, gamma=gamma, phi=phi,
                   p0.s=p0.s, #
                   p0.o=p0.o, #
                   sigma=sigma,
                   xlim=xlim, ylim=ylim, X.s=X.s, X.o=X.o, K=K, T=T)
  assign(obj.i, dat.i)
}

