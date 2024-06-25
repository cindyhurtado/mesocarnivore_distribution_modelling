#graphs outputs with real data###
library(tidyverse)
library(ggplot2)
library(jagsUI)
library(ggplot2)
library(grid)
library(gridExtra)
library(miceadds)

#omineca model

load("C:/LocalR/mesocarnivore_distribution_modelling/MDM_simulations/omineca_realdata_NA_OS.RData")

traceplot(out.Omineca_1 ,param= c("p0.S", "N", "p0.O", "psi", "sigma"))

traceplot(out.Omineca_1 ,param= "N")
whiskerplot(out.Omineca_1, parameters = c("N"))

N.df <- as.data.frame(out.Omineca_1$sims.list$N)
colnames(N.df) <- "N"
N.df$Name <- "Omineca" 

ggplot(N.df, aes(x= Name, y=N))+ geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
  
  
Pos.df <- as.data.frame(out.Omineca_1$sims.list$p0.S)
colnames(Pos.df) <- "Pos"
Pos.df$Name <- "Omineca" 

ggplot(Pos.df, aes(x= Name, y=Pos))+ geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")




