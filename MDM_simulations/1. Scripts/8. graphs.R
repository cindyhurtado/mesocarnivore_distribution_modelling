#graphs outputs with real data###
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggplot2)
library(grid)
library(gridExtra)
library(miceadds)

#omineca model

load("C:/LocalR/mesocarnivore_distribution_modelling/Fisher models/2. Outputs/out.omineca_bern_1omineca_RD_bern.RData")

traceplot(out.omineca_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))

traceplot(out.omineca_bern_1 ,param= "N")
whiskerplot(out.omineca_bern_1, parameters = c("N"))

N.df <- as.data.frame(out.Omineca_1$sims.list$N)
colnames(N.df) <- "N"
N.df$Name <- "Omineca" 

ggplot(N.df, aes(x= Name, y=N))+ geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
  
  
Pos.df <- as.data.frame(out.Omineca_1$sims.list$p0.S)
colnames(Pos.df) <- "Pos"
Pos.df$Name <- "Omineca" 

ggplot(Pos.df, aes(x= Name, y=Pos))+ geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")

# Cariboo model 

load("C:/LocalR/mesocarnivore_distribution_modelling/Fisher models/2. Outputs/out.cariboo_bern_1cariboo_RD_bern.RData")

traceplot(out.cariboo_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))

traceplot(out.cariboo_bern_1 ,param= "N")
whiskerplot(out.cariboo_bern_1, parameters = c("N"))


# Chilcotin model 

load("C:/LocalR/mesocarnivore_distribution_modelling/Fisher models/2. Outputs/out.cariboo_bern_1cariboo_RD_bern.RData")

traceplot(out.cariboo_bern_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))

traceplot(out.cariboo_bern_1 ,param= "N")
whiskerplot(out.cariboo_bern_1, parameters = c("N"))



Pos.df <- as.data.frame(out.Omineca_1$sims.list$p0.S)
colnames(Pos.df) <- "Pos"
Pos.df$Name <- "Omineca" 

ggplot(Pos.df, aes(x= Name, y=Pos))+ geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")






