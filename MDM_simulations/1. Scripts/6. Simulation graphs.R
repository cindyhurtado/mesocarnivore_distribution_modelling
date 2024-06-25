#graphs IM cariboo simulations###
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggplot2)
library(grid)
library(gridExtra)
library(miceadds)

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/2. Outputs")

#Chilcotin 3k-5k simulations ####

#1. Read files with simulations

file_list3k <- list.files("VM_simulations_chilcotin_3k",  full.names="TRUE")
file_list5k <- list.files("VM_simulation_chilcotin_5k",  full.names="TRUE")

# this loops read the 1 object saved in each simulations, no more objects can be saved in that Rdata file or it will get one at random 

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#list of all simulations for chilcotin

chilcotin.sim3k <- lapply(file_list3k, load_obj)
chilcotin.sim5k <- lapply(file_list5k, load_obj)
# Check if M is enough by looking at N.ever alive output
# Change p.o and p.s init parameters to 0.05
# simulate with fisher parameters and lower p.o and p.s

#1. Select the model 

#Abundance <- list()
N.df_3k <- list()
plots_3k <- list()

for(i in 1:length(chilcotin.sim3k)) {    
  N.df_3k[[i]] <- as.data.frame(chilcotin.sim3k[[i]]$sims.list$N)
  colnames(N.df_3k[[i]]) <- "N"
  }

N.df_all3k <- purrr::map_df(N.df_3k, data.frame, .id = 'name') ## comparison 
N.df_all3k$group <- paste("3k-", N.df_all3k$name)

## 5k chilcotin simulations

N.df_5k <- list()
plots_5k <- list()

for(i in 1:length(chilcotin.sim5k)) {    
  N.df_5k[[i]] <- as.data.frame(chilcotin.sim5k[[i]]$sims.list$N)
  colnames(N.df_5k[[i]]) <- "N"
}

N.df_all5k <- purrr::map_df(N.df_5k, data.frame, .id = 'name') ## comparison 
N.df_all5k$group <- paste("5k-", N.df_all5k$name)


N.df_ch_all <- bind_rows(N.df_all3k, N.df_all5k)

chilcotin_simulations <- ggplot(N.df_ch_all, aes(x = group, y= N))+ geom_violin() + geom_hline(yintercept=500, linetype="dashed",  color = "red", size=1) + ylab("Pop. estimate")+ xlab("Simulations") #+ theme( axis.text.x=element_blank())

chilcotin_simulations+ coord_flip()


## plot psi

psi <- list()
psi.df <- list()
plots_psi <- list()

for(i in 1:length(chilcotin.sim)) {    
  psi.df[[i]] <- as.data.frame(chilcotin.sim[[i]]$sims.list$psi)
  colnames(psi.df[[i]]) <- "psi"
#  psi[[i]] <- psi.df[[i]] %>% pivot_longer(cols = psi)
#  plots_psi[[i]] <- ggplot(psi[[i]], aes(x = reorder(name, sort(as.numeric(name))), y = value))+ geom_violin() + geom_hline(yintercept=0.3, linetype="dashed",  color = "red", size=1)+ xlab("") + ylab("Psi") #+ theme( axis.text.x=element_blank())
}

#grid.arrange(grobs= plots_psi, ncol=6)

psi_all <- purrr::map_df(psi.df, data.frame, .id = 'name')
psi_all_plot <- ggplot(psi_all, aes(x = reorder(name, sort(as.numeric(name))), y= psi))+ geom_violin() + geom_hline(yintercept=0.3, linetype="dashed",  color = "red", size=1) + ylab("Psi")+ xlab("Simulations")
# + theme( axis.text.x=element_blank())
psi_all_plot

## plot sigma 

sigma <- list()
sigma.df <- list()
plots_sigma<- list()

for(i in 1:length(chilcotin.sim)) {    
  sigma.df[[i]] <- as.data.frame(chilcotin.sim[[i]]$sims.list$sigma)
  colnames(sigma.df[[i]]) <- "sigma"
#  sigma[[i]] <- sigma.df[[i]] %>% pivot_longer(cols = sigma)
#  plots_sigma[[i]] <- ggplot(sigma[[i]], aes(x = reorder(name, sort(as.numeric(name))), y = value))+ geom_violin() + geom_hline(yintercept= 3, linetype="dashed",  color = "red", size=1)+ xlab("") + ylab("sigma") #+ theme( axis.text.x=element_blank())
}

#grid.arrange(grobs= plots_sigma, ncol=6)

sigma_all <- purrr::map_df(sigma.df, data.frame, .id = 'name')
sigma_all_plot <- ggplot(sigma_all, aes(x = reorder(name, sort(as.numeric(name))), y= sigma))+ geom_violin() + geom_hline(yintercept=3, linetype="dashed",  color = "red", size=1) + ylab("sigma")+ xlab("Simulations") #+ theme( axis.text.x=element_blank())
sigma_all_plot

# Sampling design simulations ####

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/2. Outputs")

#1. Read files with simulations

scenario1_hair_filelist <- list.files("Simulation scenario1-hair",  full.names="TRUE")
Scenario2_cam_filelist  <- list.files("Simulation scenario2-cam",  full.names="TRUE")
Scenario3_haircam_filelist  <- list.files("Simulation scenario3-haircam",  full.names="TRUE")

# this loops read the 1 object saved in each simulations, no more objects can be saved in that Rdata file or it will get one at random 

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#list of all simulations for chilcotin

scenario1 <- lapply(scenario1_hair_filelist, load_obj)
scenario2 <- lapply(Scenario2_cam_filelist, load_obj)
scenario3 <- lapply(Scenario3_haircam_filelist, load_obj)

#2. Violin plots

#Scenario 1
#Abundance_S1 <- list()
N.df_S1 <- list()
plots_S1 <- list()

for(i in 1:length(scenario1)) {    
  N.df_S1[[i]] <- as.data.frame(scenario1[[i]]$sims.list$N)
  colnames(N.df_S1[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_S1 <- purrr::map_df(N.df_S1, data.frame, .id = 'name') ## comparison 
N.df_all_S1$group <- paste("S1-", N.df_all_S1$name)

#Scenario 2

N.df_S2 <- list()
plots_S2 <- list()

for(i in 1:length(scenario2)) {    
  N.df_S2[[i]] <- as.data.frame(scenario2[[i]]$sims.list$N)
  colnames(N.df_S2[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_S2 <- purrr::map_df(N.df_S2, data.frame, .id = 'name') ## comparison 
N.df_all_S2$group <- paste("S2-", N.df_all_S2$name)

#Scenario 3

N.df_S3 <- list()
plots_S3 <- list()

for(i in 1:length(scenario3)) {    
  N.df_S3[[i]] <- as.data.frame(scenario3[[i]]$sims.list$N)
  colnames(N.df_S3[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_S3 <- purrr::map_df(N.df_S3, data.frame, .id = 'name') ## comparison 
N.df_all_S3$group <- paste("S3-", N.df_all_S3$name)

N.df_all <- bind_rows(N.df_all_S1, N.df_all_S2) #N.df_all_S3)

scenario_simulations <- ggplot(N.df_all, aes(x = group, y= N))+ geom_violin() + geom_hline(yintercept=500, linetype="dashed",  color = "red", linewidth=1) + ylab("Pop. estimate")+ xlab("Simulations")

+ theme( axis.text.x=element_blank())

scenario_simulations

# Conservation Regions simulations ####

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/2. Outputs")

#1. Read files with simulations

filelist_CH <- list.files("VM_simulations_chilcotin_3k",  full.names="TRUE")
filelist_CA  <- list.files("VM_simulations_cariboo_3k",  full.names="TRUE")
filelist_OM  <- list.files("VM_simulations_omineca_3k",  full.names="TRUE")

# this loops read the 1 object saved in each simulations, no more objects can be saved in that Rdata file or it will get one at random 

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#list of all simulations for chilcotin

chilcotin <- lapply(filelist_CH, load_obj)
cariboo <- lapply(filelist_CA, load_obj)
omineca <- lapply(filelist_OM, load_obj)

#2. Violin plots

#Chilcotin

N.df_CH <- list()

for(i in 1:length(chilcotin)) {    
  N.df_CH[[i]] <- as.data.frame(chilcotin[[i]]$sims.list$N)
  colnames(N.df_CH[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_CH <- purrr::map_df(N.df_CH, data.frame, .id = 'name') ## comparison 
N.df_all_CH$group <- paste("CH-", N.df_all_CH$name)

#Cariboo
N.df_CA <- list()

for(i in 1:length(cariboo)) {    
  N.df_CA[[i]] <- as.data.frame(cariboo[[i]]$sims.list$N)
  colnames(N.df_CA[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_CA <- purrr::map_df(N.df_CA, data.frame, .id = 'name') ## comparison 
N.df_all_CA$group <- paste("CA-", N.df_all_CA$name)


#Omineca

N.df_OM <- list()

for(i in 1:length(omineca)) {    
  N.df_OM[[i]] <- as.data.frame(omineca[[i]]$sims.list$N)
  colnames(N.df_OM[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_OM <- purrr::map_df(N.df_OM, data.frame, .id = 'name') ## comparison 
N.df_all_OM$group <- paste("OM-", N.df_all_OM$name)


N.df_all_CR <- bind_rows(N.df_all_CH, N.df_all_CA,N.df_all_OM ) #N.df_all_S3)

CR_simulations <- ggplot(N.df_all_CR, aes(x = group, y= N))+ geom_violin() + geom_hline(yintercept=500, linetype="dashed",  color = "red", linewidth=1) + ylab("Pop. estimate")+ xlab("Simulations")

#+ theme( axis.text.x=element_blank())

CR_simulations + coord_flip()

# Detection probability changes ####

setwd("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/2. Outputs")

#1. Read files with simulations

filelist_D03 <- list.files("VM_simulation_chilcotin_5k",  full.names="TRUE")
filelist_D01 <- list.files("Detection_prob_01",  full.names="TRUE")
filelist_D05  <- list.files("Detection_prob_05",  full.names="TRUE")

# this loops read the 1 object saved in each simulations, no more objects can be saved in that Rdata file or it will get one at random 

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#list of all simulations for chilcotin

chilcotin_03 <- lapply(filelist_D03, load_obj)
chilcotin_01 <- lapply(filelist_D01, load_obj)
chilcotin_05 <- lapply(filelist_D05, load_obj)

#2. Violin plots

#Chilcotin 03

N.df_03 <- list()

for(i in 1:length(chilcotin_03)) {    
  N.df_03[[i]] <- as.data.frame(chilcotin_03[[i]]$sims.list$N)
  colnames(N.df_03[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}


N.df_all_03 <- purrr::map_df(N.df_03, data.frame, .id = 'name') ## comparison 
N.df_all_03$group <- paste("03-", N.df_all_03$name)

firsts <- c("03- 1",  "03- 3",  "03- 5",  "03- 7" , "03- 9",  "03- 11","03- 13",  "03- 15"," 3-17", "3-019")
N.df_all_03b <- N.df_all_03%>% filter(group %in% firsts)


#Chilcotin 01

N.df_01 <- list()

for(i in 1:length(chilcotin_01)) {    
  N.df_01[[i]] <- as.data.frame(chilcotin_01[[i]]$sims.list$N)
  colnames(N.df_01[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_01 <- purrr::map_df(N.df_01, data.frame, .id = 'name') ## comparison 
N.df_all_01$group <- paste("01-", N.df_all_01$name)

#Chilcotin 05

N.df_05 <- list()

for(i in 1:length(chilcotin_05)) {    
  N.df_05[[i]] <- as.data.frame(chilcotin_05[[i]]$sims.list$N)
  colnames(N.df_05[[i]]) <- "N"
  #Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
}

N.df_all_05 <- purrr::map_df(N.df_05, data.frame, .id = 'name') ## comparison 
N.df_all_05$group <- paste("05-", N.df_all_05$name)

N.df_all_DP <- bind_rows(N.df_all_01, N.df_all_03b,N.df_all_05 ) 

CR_simulations_DP <- ggplot(N.df_all_DP, aes(x = group, y= N))+ geom_violin() + geom_hline(yintercept=500, linetype="dashed",  color = "red", linewidth=1) + ylab("Pop. estimate")+ xlab("Simulations")

#+ theme( axis.text.x=element_blank())

CR_simulations_DP + coord_flip()



#3. To look at traceplots in the plots windows 

trace.list_S1 <- list()


for(i in 1:length(scenario1)) {    
  
  trace.list_S1[[i]] <-traceplot(scenario1[[i]] ,param = "N")} 

#lapply(trace.list3k,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})

#c("p0.S", "N", "p0.O", "psi"))

trace.list_S2 <- list()

for(i in 1:length(scenario2)) {    
  
  trace.list_S2[[i]] <-traceplot(scenario2[[i]] ,param= "N")
}

trace.list_S3 <- list()

for(i in 1:length(scenario2)) {    
  
  trace.list_S3[[i]] <-traceplot(scenario3[[i]] ,param= "N")
}






### Columbian simulation plots ####

columbian.list <- list()
columbian.list[[1]] <- out.fisher_IMC_columbian_1
columbian.list[[2]]  <- out.fisher_IMC_columbianB_1


Abundance <- list()
N.df <- list()
plots <- list()

for(i in 1:length(columbian.list)) {    
  N.df[[i]] <- as.data.frame(columbian.list[[i]]$sims.list$N)
  colnames(N.df[[i]]) <- "N"
  Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)
  plots[[i]] <- ggplot(Abundance[[i]], aes(x = name, y = value))+     geom_violin() + geom_hline(yintercept=500, linetype="dashed",  color = "red", size=1)+ xlab("") + ylab("Pop. estimate") + theme( axis.text.x=element_blank())
}

grid.arrange(grobs= plots, ncol=2)

#2. View model output table 
jags.View(model)

#3. rough plot of parameters 

trace.list3k <- list()


for(i in 1:length(chilcotin.sim3k)) {    

  trace.list3k[[i]] <-traceplot(chilcotin.sim3k[[i]] ,param = "N")} 

#lapply(trace.list3k,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})

#c("p0.S", "N", "p0.O", "psi"))

trace.list5k <- list()

for(i in 1:length(chilcotin.sim5k)) {    
  
  trace.list5k[[i]] <-traceplot(chilcotin.sim5k[[i]] ,param= "N")
}




trace.list1 <- traceplot(out.fisher_ICM_chilcotin_newN__1 ,param= c("p0.S", "N", "p0.O", "psi"))
trace.list[[2]] <- traceplot(cariboo.list[[2]] ,param= c("p0.S", "N", "p0.O", "psi"))
trace.list[[3]] <- traceplot(cariboo.list[[3]] ,param= c("p0.S", "N", "p0.O", "psi"))
trace.list[[4]] <- traceplot(cariboo.list[[4]] ,param= c("p0.S", "N", "p0.O", "psi"))
trace.list[[5]] <- traceplot(cariboo.list[[5]] ,param= c("p0.S", "N", "p0.O", "psi"))

grid.arrange(grobs= trace.list, ncol=2)

traceplot(m2,param= c("p0.S", "N", "p0.O"))
traceplot(model,param= "deviance")
traceplot(model2,param= "deviance")

whiskerplot(m2, parameters = c("N"))
whiskerplot(model3, parameters = c("N"))
whiskerplot(model, parameters = c("p0.S"))
whiskerplot(model2, parameters = c("p0.S"))
whiskerplot(model, parameters = c("p0.O"))
whiskerplot(model2, parameters = c("p0.O"))

#3. create a violin plot 

N.df <- as.data.frame(m2$sims.list$N)
colnames(N.df) <- c("N1")
Abundance <- N.df %>% pivot_longer(cols = c(N1)) 

ggplot(Abundance, aes(x = name, y = value))+ geom_violin()

# 4. Start here with new models
m2 <- out.fisher_ICM_chilcotin_newN__1

model.list <- list()
model.list[[1]] <- out.fisher_ICM_chilcotin_newN__1
model.list[[2]] <- out.fisher_ICM_chilcotin_newN__2
model.list[[3]] <- m2
Abundance <- list()
N.df <- list()

for(i in 1:length(model.list)) {    
N.df[[i]] <- as.data.frame(model.list[[i]]$sims.list$N)
colnames(N.df[[i]]) <- "N"
Abundance[[i]] <- N.df[[i]] %>% pivot_longer(cols = N)

}
plot1<- ggplot(Abundance[[1]], aes(x = name, y = value))+ 
  geom_violin() + ylim(0, 1000) geom_hline(yintercept=500, linetype="dashed", 
                             color = "red", size=2)
plot2<- ggplot(Abundance[[2]], aes(x = name, y = value))+ 
  geom_violin() + geom_hline(yintercept=500, linetype="dashed", 
                               color = "red", size=2)
plot3<- ggplot(Abundance[[3]], aes(x = name, y = value))+ 
  geom_violin()+ geom_hline(yintercept=500, linetype="dashed", 
                            color = "red", size=2)
plot4<- ggplot(Abundance[[4]], aes(x = name, y = value))+ 
  geom_violin()

plot <- list(plot1, plot2, plot3)#, plot4)

n <- length(plot)
grid.arrange(grobs= plot, ncol=3)


plot1


