
##########################
### fit marginal closed model
##########################

#.libPaths()
#.libPaths('C:/Users/CHURTADO/AppData/R') # this is a new path
#.libPaths('C:/Users/CHURTADO/AppData/Local/R/win-library/4.3') 

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations")
start.time <- Sys.time()

library(rjags)
library(jagsUI)

stub <- "omineca_bern_10k"
nsims <- 1
M <- 1500
init_simple <- function() {
  zi <- matrix(0L, M, jdat.i$T)
  zi[1:M] <- 1 #  zi[1:(4* dim(y)[1])] <- 1 give 1's to indviduals who were detected by SCR
  sii <- apply(y, c(1,2), sum)
  si <- cbind(runif(M, xlim[1], xlim[2]),
              runif(M, ylim[1], ylim[2]))
  for(i in 1:nrow(sii)) {
    si[i,1] <- mean(X.s[sii[i,] > 0, 1])
    si[i,2] <- mean(X.s[sii[i,] > 0, 2])
  }
  list(z = zi, 
       s = si,
       p0.S=0.3, p0.O=0.1,
       sigma=3)
}
pars <- c("N","psi","p0.S","p0.O","sigma","Never")

for(i in 1:nsims){
  name.i <-"omineca_RD_bern"
  #obj.i <- get(name.i)
  out.i <- paste("out.", stub, "_", i, sep = "")
  y <- Y.s # observed SCR data for first T
  dim.y <- dim(y)
  y.orig <- array(0L, c(dim.y[1] + 1, dim.y[2], dim.y[3]))
  y.orig [1:nrow(y), , ] <-
    y # observed data augmented only with 1 row
  O <- O.o
  X.s <- as.matrix(X.s)
  X.o <- as.matrix(X.o)
  xlims <- xlim
  ylims <- ylim
  jdat.i <- list(
    y.orig = y.orig,
    n = nrow(y.orig) - 1,
    O = O,
    M = M,
    #M=dim.y[1],
    J.s = dim.y[2],
    X.s = X.s,
    J.o = dim(O)[1],
    X.o = X.o,
    K = dim.y[3],
    K.o= dim(O)[2],
    T = 1,
    xlims = xlim,
    ylims = ylim
  )
  out <-
    jags(
      "margSingle_IM_fisher7.JAG",
      data = jdat.i,
      inits = init_simple,
      parallel = TRUE, n.cores= 10,
      n.chains = 3,
      n.burnin = 3000,
      n.adapt = 1000,
      n.iter = 10000,
      parameters.to.save = pars
    )
  assign(out.i, out)
  save(list = out.i, file = paste(out.i, "omineca_RD_bern_10k.Rdata", sep = ""))
  rm(name.i, out.i, out)
}

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

