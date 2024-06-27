model{
  #this model is for 2 years
  ############# these are shared across years. flat priors all around.
  #there are 6 populations(called Rits, here), each with their own psi 
  #in this project there was knowledge to inform the psi priors for some Rits/pops.
  #each Rit also has its own M
  psi[1]~dunif(0,1)             
  psi[2]~dunif(46/MperRit[2],1) 
  psi[3]~dunif(22/MperRit[3],1) 
  psi[4]~dunif(0,1)             
  psi[5]~dunif(0,1)             
  psi[6]~dunif(18/MperRit[6],1) 
  
  #detection
  alpha0 ~ dnorm(0, 0.1)
  alpha2 ~ dnorm(0,0.1) #coefficient for detection covariate C
  
  #sigma
  sigma~dunif(0, 3) # flat prior
  alpha1<-1/(2*sigma*sigma) # movement scale, euclidean distance stuff
  
  ##########For 2016
  for(i in 1:M_2016){ # per individual in 2016
    #the individual i is part of superpopulation M according to the Rit-specific psi.
    z_2016[i]~dbern(psi[ritidNP_2016[i]])
    
    s_2016[i,1]~dunif(1, xlim[2]) # x coordinate of activity center
    s_2016[i,2]~dunif(1, ylim[2]) # y coordinate of activity center
    
    #this is the "habitat" mask stuff across the statespace. Values in the habitat mask can be 0 up to the number of populations you are modeling
    #0 means it is not habitat at all, 1 up to the number of pops defines what population/area that statespace shall belong to
    #ppOK gets the habitat mask value at the individual's location
    #which population each observed indivdiual is from must be known, and provided to the model as data. here, in ritidNP
    ppOK_2016[i] <- habmat[trunc( s_2016[i,1]), trunc(s_2016[i,2])] # habitat check. does this pixel have a 0 or a ritland number? not hab or hab
    pOK_2016[i] <-ifelse(ppOK_2016[i]==ritidNP_2016[i],1,0) # only set pOK to 1 if the ritland of the proposed pixel for the individual matches the known individual's ritland
    OK_2016[i] ~ dbern(pOK_2016[i])   # only allow if pOK =1;because we set OK[i] = 1, the ones trick
    
    for(j in 1:J_2016){ #for each trap location in 2016
      d_2016[i,j]<-pow(pow(s_2016[i,1]-X_2016[j,1], 2)+pow(s_2016[i,2]-X_2016[j,2],2), 0.5) #standard calculation for distance between activity centers and trap locations
      
      for (k in 1:K_2016[j]){ #for each occasion per session
        y_2016[i,j, k]~dbin(p_2016[i,j, k],1)
        p_2016[i,j, k]<-z_2016[i]*p0_2016[i,j, k]*exp(-alpha1*d_2016[i,j]*d_2016[i,j])
        logit(p0_2016[i,j, k])<-alpha0+alpha2*C_2016[i, j, k] #detection prob
        
      } # K_2016
    } # J_2016
  } # M_2016
  
  
  ########## repeat but for 2017, with separate M's, z's and s's but still use the corresponding same psi.
  for(a in 1:M_2017){ # per individual
    z_2017[a]~dbern(psi[ritidNP_2017[a]])
    
    s_2017[a,1]~dunif(1, xlim[2]) # x coordinate of activity center
    s_2017[a,2]~dunif(1, ylim[2]) # y coordinate of activity center
    ppOK_2017[a] <- habmat[trunc( s_2017[a,1]), trunc(s_2017[a,2])] # habitat check
    pOK_2017[a] <-ifelse(ppOK_2017[a]==ritidNP_2017[a],1,0) # only set pOK to 1 if the ritland of the proposed pixel for the individual matches the individual's ritland
    OK_2017[a] ~ dbern(pOK_2017[a])   # OK[a] = 1, the ones trick
    
    for(b in 1:J_2017){ #for each trap location
      d_2017[a,b]<-pow(pow(s_2017[a,1]-X_2017[b,1], 2)+pow(s_2017[a,2]-X_2017[b,2],2), 0.5) #standard calculation for distance between activity centers and trap locations
      
      for (c in 1:K_2017[b]){ #for each occasion per session
        y_2017[a,b, c]~dbin(p_2017[a,b, c],1)
        p_2017[a,b, c]<-z_2017[a]*p0_2017[a,b, c]*exp(-alpha1*d_2017[a,b]*d_2017[a,b])
        logit(p0_2017[a,b, c])<-alpha0+alpha2*C_2017[a, b, c] #detection prob function of project-specific detection probability and some covariate C
        
      } # K_2017
    } # J_2017
  } # M_2017
  
  #######this is the same for both years
  A<-sum(habmat)# ((xlim[2]-xlim[1])*(ylim[2]-ylim[1])) # area of statespace
  
  ##########For 2016 determine N and D based on ritlands
  for (t in 1:6){ # per session
    N[t] <- inprod(z_2016[1:M_2016],ritDummy_2016[,t])
    D[t] <- (N[t]/A)
  } # T
  ##########For 2017 determine N and D based on the same ritlands
  for (t in 7:12){ # per session
    N[t] <- inprod(z_2017[1:M_2017],ritDummy_2017[,t-6])
    D[t] <- (N[t]/A)
  } # T
  
} 


#MperRit is a vector with an M for each of the different pops. The M's can be different from each other
#bigM is the sum of that vector
#ritidNP is the populuation assignment of each indivdiaul in each M: basically, repeat the population number M times. 
#ritDummy is a matrix of bigM by # populations. 1's indicate which pop each ind is from.
#OK is just a series of 1s for the ones trick.
jagsNP.data<-list(xlim=xlim, ylim=ylim, MperRit=MperRit,
                  y_2017=y3dNP_2017, X_2017=as.matrix(X_2017), K_2017=K_2017, M_2017=bigM_2017, ritidNP_2017=ritidNP_2017, J_2017=J_2017, C_2017=C_2017,ritDummy_2017=ritDummy_2017,
                  y_2016=y3dNP_2016, X_2016=as.matrix(X_2016), K_2016=K_2016, M_2016=bigM_2016, ritidNP_2016=ritidNP_2016, J_2016=J_2016, C_2016=C_2016,ritDummy_2016=ritDummy_2016,
                  habmat=mask_rit$habMat,
                  OK_2016=rep(1,bigM_2016),OK_2017=rep(1,bigM_2017))

jagsNP.inits_list<- list(alpha0=rnorm(1,-4, 0.4), sigma=1, alpha2=rnorm(1, -4, 0.4),
                         psi=rep(0.5,6),  s_2016=sstNP_2016, z_2016=zNP_2016,
                         s_2017=sstNP_2017, z_2017=zNP_2017)

#or really monitor whatever you want :) 
jagsNP.parameters<-c("alpha0",  "sigma", "alpha2", "psi", "N",# "D", #"alpha1" not necessary
                     "s_2016", "z_2016","s_2017", "z_2017") #"ritidNP_2016","ritidNP_2017",
