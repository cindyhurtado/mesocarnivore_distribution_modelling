require(nimble)

calcDistSquared <- nimbleFunction(
  run = function(s = double(1), traps = double(2), z = double()){
    returnType(double(1))
    if(z == 0)
      return(numeric(value = 0, length = dim(traps)[1]))

      dist2 <- (traps[,1] - s[1])^2 + (traps[,2] - s[2])^2
      return(dist2)
  }
)

calcTrapDetection_HN = nimbleFunction(
  run = function(distsSquared = double(1), sigma = double(), z = double()){ #, detfn = character(0, default = "halfnormal")){
    returnType(double(1))
    if(z == 0)
      return(numeric(value = 0, length = dim(distsSquared)[1]))
    # if(detfn == "halfnormal")
    return(exp(-0.5*distsSquared/sigma^2))
  }
)

dTrapPoisson_NoID = nimbleFunction(
  run = function(x = double(), p = double(1), lam0 = double(), T = double(), z = double(1), log = integer(0, default = 0)){ #, detfn = character(0, default = "halfnormal")){
    returnType(double())
    H <- lam0*sum(p[z == 1])
    ans <- x*log(T*H) - T*H
    if(log) 
      return(ans)
    else
      return(exp(ans))
  }
)

## Single trap across all animals distr function.
dTrapBinom_NoID = nimbleFunction(
  run = function(x = double(), p = double(1), lam0 = double(), z = double(1), 
      nocc = double(), log = integer(0, default = 0)){
    returnType(double())
    logpmiss = sum(log(1-lam0*p[z==1]))
    ans <- (nocc-x)*logpmiss + x*log(1-exp(logpmiss))
    if(log) 
      return(ans)
    else
      return(exp(ans))
  }
)

dTrapBinom = nimbleFunction(
  run = function(x = double(1), p = double(1), lam0 = double(), z = double(), 
      nocc = double(1), detfn = double(0, default = 1), log = integer(0, default = 0)){ #, detfn = character(0, default = "halfnormal")){
    returnType(double())
    if(z == 0){
      ans <- 0
    }else{
      if(detfn == 1) ## "halfnormal"
      pobs <- p*lam0
      if(detfn == 2) ## hazard halfnormal
        pobs <- 1-exp(-lam0*pobs)  ## This one is consistent with poisson halfnormal model.
        
      if(sum(x) == 0) {
        ans <- sum(nocc*log(1-pobs))
      }else{
        ans <- sum( x*log(pobs) + (nocc-x)*log(1-pobs) )
      }
    }
    if(log) 
      return(ans)
    else
      return(exp(ans))
  }
)

## Combined SCR and OCC model built for speed.
scr_occ_model <- nimbleCode({
  psi ~ dbeta(1, 1)    # M*psi = E[N(1)]
  p0.S ~ dbeta(1, 1)
  p0.O ~ dbeta(1, 1)
  
  sigma ~ dgamma(6, 4) # followed burgar et. al. 2018 for calculations, 5 to 300km2
  N <- sum(z[])     # Number of females
    
  for(i in 1:M) {
    z[i] ~ dbern(psi)

    s[i,1] ~ dunif(xlims[1], xlims[2])
    s[i,2] ~ dunif(ylims[1], ylims[2])

    d2.s[i,1:J.s] <- calcDistSquared(s = s[i, 1:2], traps = X.s[1:J.s, 1:2], z = z[i])
    d2.o[i,1:J.o] <- calcDistSquared(s = s[i, 1:2], traps = X.o[1:J.o, 1:2], z = z[i])

    ## This has purposefully left out p0 for computation speed.
    p.S[i, 1:J.s] <- calcTrapDetection_HN(distsSquared = d2.s[i,1:J.s], sigma = sigma, z = z[i])
    p.O[i, 1:J.o] <- calcTrapDetection_HN(distsSquared = d2.o[i,1:J.o], sigma = sigma, z = z[i])
    
    ## p0 goes in here as it didn't impact that other part.
    y[i,1:J.s] ~ dTrapBinom(p = p.S[i,1:J.s], lam0 = p0.S, nocc = nocc.s[1:J.s], z = z[i], detfn = 1)
  }#m
  
  for(j in 1:J.o) { #for every trap
      O[j] ~ dTrapBinom_NoID(p = p.O[1:M, j], lam0 = p0.O, nocc = nocc.o[j], z = z[1:M]) 
    }#j.o
  }#m
)

## This model input has counts instead of binomials
scr_count_model <- nimbleCode({
  psi ~ dbeta(1, 1)    # M*psi = E[N(1)]
  lam ~ unif(0, 100)  ## removed p.O + p.S
  
  sigma ~ dgamma(6, 4) # followed burgar et. al. 2018 for calculations, 5 to 300km2
  N <- sum(z[])     # Number of females
    
  for(i in 1:M) {
    z[i] ~ dbern(psi)

    s[i,1] ~ dunif(xlims[1], xlims[2])
    s[i,2] ~ dunif(ylims[1], ylims[2])

    d2.s[i,1:J.s] <- calcDistSquared(s = s[i, 1:2], traps = X.s[1:J.s, 1:2], z = z[i])
    d2.o[i,1:J.o] <- calcDistSquared(s = s[i, 1:2], traps = X.o[1:J.o, 1:2], z = z[i])

    ## This has purposefully left out p0 for computation speed.
    p.S[i, 1:J.s] <- calcTrapDetection_HN(distsSquared = d2.s[i,1:J.s], sigma = sigma, z = z[i])
    p.O[i, 1:J.o] <- calcTrapDetection_HN(distsSquared = d2.o[i,1:J.o], sigma = sigma, z = z[i])
    
    ## p0 goes in here as it didn't impact that other part.
    y[i,1:J.s] ~ dTrapBinom(p = p.S[i,1:J.s], lam0 = lam, nocc = nocc.s[1:J.s], z= z[i], detfn = 2)
  }#m
  
  ## This is Chandler and Royle 2013
  for(j in 1:J.o) { #for every trap
      O[j] ~ dTrapPoisson_NoID(p = p.O[1:M, j], lam0 = lam, T = nocc.o[j], z = z[1:M]) 
    }#j.o
  }#m
)
