##########################
### fit marginal closed model
##########################

#.libPaths()
#.libPaths('C:/Users/CHURTADO/AppData/R') # this is a new path
#.libPaths('C:/Users/CHURTADO/AppData/Local/R/win-library/4.3') 

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/For Paul/Chilcotin new model")
start.time <- Sys.time()

# library(rjags)
# library(jagsUI)
library(nimble)

stub <- "closed_model_chilcotin"

load("Chilcotin_realdata.Rdata")
source("EfficientNimbleModel.R")

M <- 1500
init_simple <- function() {
  z <- c(rep(NA, nobs), rbinom(M - nobs, size = 1, prob = 0.25))

  sii <- apply(y, c(1,2), sum)
  si <- cbind(runif(M, xlims[1], xlims[2]),
              runif(M, ylims[1], ylims[2]))
  for(i in 1:nrow(sii)) {
    si[i,1] <- mean(X.s[sii[i,] > 0, 1])
    si[i,2] <- mean(X.s[sii[i,] > 0, 2])
  }
  list(z = z, 
       s = si,
       p0.S=0.1, p0.O=0.05,
       sigma=3)
}
pars <- c("N","psi","p0.S","p0.O","sigma")

# name.i <- paste("dat.", stub, "_", i, sep = "")
#obj.i <- get(name.i)
name.i <- "chilcotin_RD_nimble"
out.i <- paste("out.", stub, "_", i, sep = "")
y <- y.binom # observed SCR data for first T, ex Y.s
dim.y <- dim(y)
y.orig <- array(0L, c(dim.y[1] + 1, dim.y[2]))
y.orig [1:nrow(y), ] <-  y # observed data augmented only with 1 row
O <- O.binom2# used to be O
nobs <- sum(rowSums(y.binom) > 0)

constants <- list(
  X.s = as.matrix(X.s),
  X.o = as.matrix(X.o),
  xlims = xlim,
  ylims = ylim,
  M = M,
  J.s = nrow(X.s), ## hair snag
  J.o = nrow(X.o),
  nocc.o = n.occ.o,
  nocc.s = n.occ.s
)

data <- list()
data$y <- matrix(0, nrow = M, ncol = constants$J.s)
data$y[1:nrow(y.orig),] <- y.orig  
data$O <- O
data$z <- c(rep(1, nobs), rep(NA, M - nobs))

## Can be slow sometimes to build and compile the model:
Rmodel <- nimbleModel(scr_occ_model, constants, data, inits = init_simple())
#Rmodel <- nimbleModel(scr_count_model, constants, data, inits = init_simple()) ## Poisson version I mentioned.
conf <- configureMCMC(Rmodel, control = list(adaptFactorExponent = 0.25)) ## Better default rw adapter factor (I think). 
conf$setMonitors(pars)

## Block sample the animal locations:
conf$removeSamplers('s')
for(i in 1:M){
	conf$addSampler(target = paste0('s[', i, ', 1:2]'),
			type = 'RW_block', silent = TRUE, control = list(adaptFactorExponent = 0.25))
}

## Now compile model and mcmc into C++
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel) ## didnt work
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

## Just run mcmc locally and see how it does.
Cmcmc$run(niter = 100)
mvSamples <- Cmcmc$mvSamples
samples <- as.matrix(mvSamples)
## Can look at as an MCMC object. 
## Make sure things are working.
out <- as.mcmc(samples) 

## Then you can run their wrapper function which will be very similar to the jags original code.
samples <- runMCMC(Cmcmc, niter = 10000, nchains = 1, inits = init_simple(), samplesAsCodaMCMC = TRUE)



