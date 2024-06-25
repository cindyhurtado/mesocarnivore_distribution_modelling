##########################
### fit marginal open model
##########################
setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations")
start.time <- Sys.time()

library("jagsUI")
init_simple <- function() {
  zi <- matrix(0L, M, jdat.i$T)
  zi[1: (4 *(dim(y)[1])), ] <- 1 # deleted 5* in 1:5*(dim(y)[1]))
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
    p0.S = p0.s,
    p0.O = p0.o,
    sigma = 5
  )
}
pars <-
  c("N", "psi", "p0.S", "p0.O", "sigma", "Never", "gamma", "b", "phi")

for (i in 1:nsims) {
  name.i <- paste("dat.chilcotin_", stub, "_", i, sep = "")
  obj.i <- get(name.i)
  out.i <- paste("open.chilcotin_", stub, "_", i, sep = "")
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
      n.burnin = 5000,
      n.adapt = 2000,
      n.iter = 50000,
      parameters.to.save = pars
    )
  assign(out.i, out)
  save(list = out.i, file = paste(out.i, "50k.RData", sep = ""))
  rm(name.i, obj.i, out.i, out)
}

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

jags.View(open.chilcotin_test_IOM_1)
