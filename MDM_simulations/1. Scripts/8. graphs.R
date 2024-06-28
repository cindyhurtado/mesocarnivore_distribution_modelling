#graphs outputs with real data###
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggplot2)
library(grid)
library(gridExtra)
library(miceadds)
library(dotwhisker)
#omineca model


setwd("C:/LocalR/mesocarnivore_distribution_modelling/Fisher models/2. Outputs")

#1. Read output files 

file_list<- list.files(pattern = "\\.Rdata$",  full.names="TRUE")


# this loops read the 1 object saved in each simulations, no more objects can be saved in that Rdata file or it will get one at random 

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#2. Graph for population estimates 

outputs <- lapply(file_list, load_obj)

N.df_RD <- list()

for(i in 1:length(outputs)) {    
  N.df_RD[[i]] <- as.data.frame(outputs[[i]]$sims.list$N)
  colnames(N.df_RD[[i]]) <- "N"
  N.df_RD[[i]]$'90%' <-quantile(outputs[[i]]$sims.list$N, prob= 0.9)
  N.df_RD[[i]]$'10%' <-quantile(outputs[[i]]$sims.list$N, prob= 0.1)
  N.df_RD[[i]]$'50%' <-quantile(outputs[[i]]$sims.list$N, prob= 0.5)
  N.df_RD[[i]]$SD <- sd(outputs[[i]]$sims.list$N)
}


#confidence interval 1.28*SD

names(N.df_RD) <- c("Cariboo", "Chilcotin","Omineca10", "Omineca")

N.df_allRD <- purrr::map_df(N.df_RD, data.frame, .id = 'name') ## comparison 
N.df_allRD$group <- paste(N.df_allRD$name)

RD_simulations <- ggplot(N.df_allRD, aes(x = group, y= X50.))+ geom_point() + geom_errorbar(aes(ymin= X10., ymax= X90.), width=0.2) + xlab("Conservation Region")

#+ ylab("Pop. estimate")

RD_simulations2 <- ggplot(N.df_allRD, aes(x = group, y= X50.))+ geom_point() + geom_errorbar(aes(ymin= X50.- (1.28*SD), ymax= X50. + (1.28*SD)), width=0.2) + xlab("Conservation Region")

RD_simulations +scale_y_continuous(name="Pop. estimate", limits=c(0, 1500))
RD_simulations2 +scale_y_continuous(name="Pop. estimate", expand = c(0, 0), breaks = seq(0, 1500, by = 300), limits=c(0, 1500))

traceplot(out.cariboo_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))
traceplot(out.chilcotin_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))
traceplot(out.omineca_bern_10k_1,param= c("p0.S", "N", "p0.O", "psi", "sigma"))
traceplot(out.omineca_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))

#3. Graphs psi 


Psi_RD <- list()

for(i in 1:length(outputs)) {    
  Psi_RD[[i]] <- as.data.frame(outputs[[i]]$sims.list$psi)
  colnames(Psi_RD[[i]]) <- "Psi"
  Psi_RD[[i]]$'90%' <-quantile(outputs[[i]]$sims.list$psi, prob= 0.9)
  Psi_RD[[i]]$'10%' <-quantile(outputs[[i]]$sims.list$psi, prob= 0.1)
  Psi_RD[[i]]$'50%' <-quantile(outputs[[i]]$sims.list$psi, prob= 0.5)
  Psi_RD[[i]]$SD <- sd(outputs[[i]]$sims.list$pis)
}

names(Psi_RD) <- c("Cariboo", "Chilcotin","Omineca10", "Omineca")

Psi_all <- purrr::map_df(Psi_RD, data.frame, .id = 'name') ## comparison 
Psi_all$group <- paste(Psi_all$name)

PSI_RD_plot <- ggplot(Psi_all, aes(x = group, y= X50.))+ geom_point() + geom_errorbar(aes(ymin= X50.- (1.28*SD), ymax= X50. + (1.28*SD)), width=0.2) + xlab("Conservation Region")

PSI_RD_plot +scale_y_continuous(name="Pop. estimate", limits=c(0, 1))
PSI_RD_plot +scale_y_continuous(name="Psi", expand = c(0, 0), breaks = seq(0, 1, by = ), limits=c(0, 1))



savePlot(filename = "Rplot",
         type = c("wmf", "emf", "png", "jpg", "jpeg", "bmp",
                  "tif", "tiff", "ps", "eps", "pdf"),
         device = dev.cur(),
         restoreConsole = TRUE)

traceplot(out.omineca_bern_10k_1 ,param= "N")


whiskerplot(out.omineca_bern_10k_1, parameters = c("N"))
dwplot(out.omineca_bern_10k_1) # summary of results and chains
