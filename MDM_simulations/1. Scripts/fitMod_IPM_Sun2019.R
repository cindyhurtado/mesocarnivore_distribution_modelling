### Fit various models

##########################
### fit marginal closed model
##########################

#.libPaths()
#.libPaths('C:/Users/CHURTADO/AppData/R') # this is a new path
#.libPaths('C:/Users/CHURTADO/AppData/Local/R/win-library/4.3') 

library(rjags)
library(jagsUI)

M<-300
init_simple <- function() {
  zi <- matrix(0L, M, jdat.i$T)
  zi[1:(dim(y)[1])] <- 1 # give 1's to indviduals who were detected by SCR
  sii <- apply(y, c(1,2), sum)
  si <- cbind(runif(M, xlims[1], xlims[2]),
              runif(M, ylims[1], ylims[2]))
  for(i in 1:nrow(sii)) {
    si[i,1] <- mean(X.s[sii[i,] > 0, 1])
    si[i,2] <- mean(X.s[sii[i,] > 0, 2])
  }
  list(z = zi, 
       s = si,
       p0.S=0.2, p0.O=0.2,
       sigma=5)
}
pars <- c("N","psi","p0.S","p0.O","sigma","Never")

for(i in 1:nsims){
  name.i <- paste("dat.", stub, "_", i, sep = "")
  obj.i <- get(name.i)
  out.i <- paste("out.", stub, "_", i, sep = "")
  y <- obj.i$y.s[, , , 1] # observed SCR data for first T
  dim.y <- dim(y)
  y.orig <- array(0L, c(dim.y[1] + 1, dim.y[2], dim.y[3]))
  y.orig [1:nrow(y), , ] <-
    y # observed data augmented only with 1 row
  O <- obj.i$O.o[, , 1]
  X.s <- as.matrix(obj.i$X.s)
  X.o <- as.matrix(obj.i$X.o)
  xlims <- obj.i$xlims
  ylims <- obj.i$ylims
  jdat.i <- list(
    y.orig = y.orig,
    n = nrow(y.orig) - 1,
    O = O,
    M = M,
    #M=dim.y[1],
    J.s = dim.y[2],
    X.s = X.s,
    J.o = dim(O)[[1]],
    X.o = X.o,
    K = dim.y[3],
    T = 1,
    xlims = xlims,
    ylims = ylims
  )
  out <-
    jags(
      "MarginalModel_closed.JAG",
      data = jdat.i,
      inits = init_simple,
      parallel = TRUE, n.cores= 18,
      n.chains = 3,
      n.burnin = 2000,
      n.adapt = 100,
      n.iter = 5000,
      parameters.to.save = pars
    )
  assign(out.i, out)
  save(list = out.i, file = paste(out.i, ".gzip", sep = ""))
  rm(name.i, obj.i, out.i, out)
}


# this model run and worked using data from the simulations. 

##########################
### fit marginal open model
##########################
library("jagsUI")
init_simple <- function() {
  zi <- matrix(0L, M, jdat.i$T)
  zi[1:(5 * (dim(y)[1])), ] <- 1
  sii <- apply(y, c(1, 2), sum)
  si <- cbind(runif(M, xlims[1], xlims[2]),
              runif(M, ylims[1], ylims[2]))
  for (i in 1:nrow(sii)) {
    si[i, 1] <- mean(X.s[sii[i, ] > 0, 1])
    si[i, 2] <- mean(X.s[sii[i, ] > 0, 2])
  }
  list(
    z = zi,
    s = si,
    gamma = 0.2,
    phi = 0.8,
    p0.S = 0.05,
    p0.O = 0.2,
    sigma = 5
  )
}
pars <-
  c("N", "psi", "p0.S", "p0.O", "sigma", "Never", "gamma", "b", "phi")

for (i in 1:nsims) {
  name.i <- paste("dat.", stub, "_", i, sep = "")
  obj.i <- get(name.i)
  out.i <- paste("out.", stub, "_", i, sep = "")
  y <- obj.i$y.s # observed SCR data for all T
  dim.y <- dim(y)
  y.orig <-
    array(0L, c(dim.y[1] + 1, dim.y[2], dim.y[3], dim.y[4]))
  y.orig [1:nrow(y), , , ] <-
    y # observed data augmented only with 1 row
  O <- obj.i$O.o[, , ]
  X.s <- as.matrix(obj.i$X.s)
  X.o <- as.matrix(obj.i$X.o)
  xlims <- obj.i$xlims
  ylims <- obj.i$ylims
  jdat.i <- list(
    y.orig = y.orig,
    n = nrow(y.orig) - 1,
    O = O,
    M = M,
    J.s = dim.y[2],
    X.s = X.s,
    J.o = dim(O)[[1]],
    X.o = X.o,
    K = dim.y[3],
    T = dim.y[4],
    xlims = xlims,
    ylims = ylims
  )
  out <-
    jags(
      "margMulti_IPM.JAG",
      data = jdat.i,
      inits = init_simple,
      parallel = TRUE, n.cores= 18,
      n.chains = 3,
      n.burnin = 2000,
      n.adapt = 100,
      n.iter = 5000,
      parameters.to.save = pars
    )
  assign(out.i, out)
  save(list = out.i, file = paste(out.i, ".gzip", sep = ""))
  rm(name.i, obj.i, out.i, out)
}

jags.View(out.test_IM_1)
