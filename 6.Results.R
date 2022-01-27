################################################################################
#IN THIS DO FILE: TABLES AND FIGURES TO DISPLAY RESULTS FROM SIMULATION. 
#THIS FILE IS VERY MUCH AT ITS BEGINNING STILL; ANY MODIFICATION IS WELCOME.
################################################################################
##LOAD PACKAGES, SET DIRECTORY, LOAD RESULTS
################################################################################
library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)
library(cowplot)

rm(list=ls())

setwd("C:/Users/micha/Documents/Git-RStudio/SimIndLangCan")

#starting populations
sp <- readRDS("startingpopulations")

#languages
languages <- unique(sp$language)

#intergenerational transmission, slope, & population number
int.nb <- readRDS("inttrans")

#simulation results
results <- readRDS("first25_30runs")
################################################################################
#LANGUAGE PROFILES
################################################################################
#number of speakers by year
simres <- results %>% group_by(period,language,run) %>% summarise(alive=sum(alive),births=sum(births))

#5 year periods
simres$period5 <- floor((simres$period-1)/5)*5+1

#language number (1 to n)
int.nb$number <- 1:length(int.nb$language)

#take 25 smallest languages
first25 <- int.nb %>% arrange(speaker) %>% slice(1:25)

#Function to create plots########################################

plots <- function(n){
  
  ##name
  name <- first25$language[n]
  
  #projection results
  projection <- filter(simres,language==name)
  
  #average across runs
  projection <- left_join(projection,projection %>% group_by(period) %>% summarise(average=mean(alive)),by="period")
  
  #rate of intergenational transmission
  projection <- left_join(projection,projection %>% group_by(period5) %>% summarise(itr=mean(births)/mean(alive)*1000),by="period5")
  
  #age distribution
  agedist <- filter(sp,language==name)
  agedist$speaker <- round(agedist$speaker)
  
  #initial size
  size1 <- round(filter(int.nb,language==name)$speaker)
  
  #final size
  size2 <- unique(round(filter(projection,period==2100)$average))
  minyr2100 <- min(filter(projection,period==2100)$alive)
  maxyr2100 <- max(filter(projection,period==2100)$alive)
  
  #dormancy
  dormancy <- round(length(filter(projection,period==2100,alive==0)$language)/max(projection$run),2)
  
  #plot trends
  trends <- ggplot(projection,aes(period,alive,group=run))+
    geom_line(color="grey")+
    geom_line(aes(period,average))+
    theme_bw()+
    xlab("Year")+
    ylab("Number of speakers")
  
  #plot age distribution
  pyramid <- ggplot(agedist,aes(age,speaker))+
    geom_col()+
    coord_flip()+
    theme_bw()+
    ylab("Number of speakers")+
    xlab("Age")+
    scale_x_continuous(breaks=c(seq(0,100,20)))
  
  #plot intergenerational transmission rate
  itr <- ggplot(projection,aes(period5,itr))+
    geom_line()+
    theme_bw()+
    xlab("Year")+
    ylab("Rate (x 1,000)")+
    scale_y_continuous(limits=c(0,25))

  info <- paste(name,"\nSpeakers in 2016: ",size1,"\nSpeakers in 2100: ",size2," (",minyr2100,"-",maxyr2100,")\nDormancy risk in 2100: ",dormancy,sep="")
  
  #arrange in grid
  plot_grid(trends,pyramid,NULL,itr,
            labels=c("Trends",
                     "Distribution by age in year 2016",
                     info,
                     "Intergenerational transmission rate"),
            ncol=2,rel_heights=c(3,2))
  
  ggsave(paste(name,".tiff",sep=""),height=7,width=12)
  
  }

lapply(1:25,function(x) plots(x))

#######################################################################################################
