#!/usr/bin/env Rscript

start.time <- Sys.time()

print(
    paste(
         "Starting run at",round( Sys.time() )
         ) 
    )

##########################
#3B._Simulation_FISHER_chilcotin_bash.R### fit marginal closed model
##########################

myargs <- commandArgs(trailingOnly = TRUE) #be careful in the future with ARGUMENTS (eg. A01... A11)that dont match the logger ID name!!!

print(myargs)
k <- myargs


getwd()

print(
    paste(
         "loading Rdata for each bash parallel run at",round( Sys.time() )
         ) 
    )
    
    
load("Chilcotin_simulated_data.RData")


    
library(rjags)
library(jagsUI)
start.time <- Sys.time()


M <- 1500
n.burnin <- 1000
n.adapt <- 3000
n.iter <- 5000




print(
    paste(
         "M val", M ,"at", round( Sys.time() )
         ) 
    )
    
print(
    paste(
         "Creating function at",round( Sys.time() )
         ) 
    ) 
    
            
init_simple <- function() {
  zi <- matrix(0L, M, jdat.i$T)
  zi[1:M] <- 1 #  zi[1:(4* dim(y)[1])] <- 1 give 1's to indviduals who were detected by SCR
  sii <- apply(y, c(1,2), sum)
  si <- cbind(runif(M, xlims[1], xlims[2]),
              runif(M, ylims[1], ylims[2]))
  for(i in 1:nrow(sii)) {
    si[i,1] <- mean(X.s[sii[i,] > 0, 1])
    si[i,2] <- mean(X.s[sii[i,] > 0, 2])
  }
  list(z = zi, 
       s = si,
       p0.S=0.3, p0.O=0.1,
       sigma=3)
}
print(
    paste(
         "Defining parameters at",round( Sys.time() )
         ) 
    ) 
    
pars <- c("N","psi","p0.S","p0.O","sigma","Never")

print(pars)

print(
    paste(
         "Producing naming objects for run",k, "at", round( Sys.time() )
         ) 
    ) 
    
    
## remove for loop which is added in my commands in bash 
#for (i in as.numeric(myargs)){
  name.i <- paste0("dat.", stub, "_", k)
  obj.i <- get(name.i)
  out.i <- paste0("out.", stub, "_", k)
  y <- obj.i$y.s # observed SCR data for first T
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
  
  print(
    paste(
         "Running jags model at ",round( Sys.time() )
         ) 
    ) 
    
  out <-
   jagsUI::jags(
      "margSingle_IM_fisher.JAG",
      data = jdat.i,
      inits = init_simple,
      parallel = TRUE, n.cores= 3,
      n.chains = 3,
      n.burnin = n.burnin,
      n.adapt = n.adapt,
      n.iter = n.iter,
      parameters.to.save = pars
    )
    
    print(
    paste(
         "Assigning name to jags model at ",round( Sys.time() )
         ) 
    )  
    
out$summary

  #assign(out.i, out)
  
  print(
    paste(
         "Saving output of jags model at sas RData",round( Sys.time() )
         ) 
    ) 
save(out,
   file = paste0(out.i, "PSI_1-0_bash.RData"))
   
  #rm(name.i, obj.i, out.i, out)
#}
 print(
    paste(
         "Saving output of jags model as RDS at ",round( Sys.time() )
         ) 
    ) 
    
saveRDS(out,  file = paste0(out.i,"PSI_1-0_bash.RDS"))

end.time <- Sys.time()
time.elapsed <- round(end.time - start.time,2)
print(time.elapsed)

