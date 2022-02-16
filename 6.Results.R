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
library(lcmm)

rm(list=ls())

setwd("C:/Users/micha/Documents/Git-RStudio/SimIndLangCan")

#set ggplot2 theme for consistency
theme_set(theme_bw())  

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
############################################################
#Estimate trajectories
############################################################
#average across runs and inital speaker number in the year 2016
trajectories <- left_join(simres %>% group_by(period5,language) %>% summarise(average=mean(alive)),
                        simres %>% group_by(language) %>% filter(period==2016) %>% summarise(alive2016=mean(alive)))

#proportion of initial number over time
trajectories$proportion <- round(trajectories$average / trajectories$alive2016 *100, 1)

#add language number
trajectories$nb <- rep(1:25,85/5)

#reconvert year to years after 2016
trajectories$period0 <- trajectories$period5-2016

#visualize pathways
ggplot(trajectories,aes(period5,proportion,group=nb))+
  geom_line()

#model with splines and 3 groups
model3splines <- lcmm(proportion ~ period0, random = ~ period0, subject = 'nb', 
                      mixture = ~ period0, ng = 3, idiag = TRUE, data=trajectories,
                      link="splines")

#mixutre model with splines and 4 groups
model4splines <- lcmm(proportion ~ period0, random = ~ period0, subject = 'nb', 
                      mixture = ~ period0, ng = 4, idiag = TRUE, data=trajectories,
                      link="splines")

#mixutre model with splines and 5 groups
model5splines <- lcmm(proportion ~ period0, random = ~ period0, subject = 'nb', 
                      mixture = ~ period0, ng = 5, idiag = TRUE, data=trajectories,
                      link="splines")

#Compare models 
model3splines
model4splines
model5splines

#the model with 3 groups fits best; put in data frame
results <- data.frame(period0=seq(0,80,5))
results.raw <- predictY(model3splines, newdata=results, var.time="period0", draws = T)
results <- data.frame(period5=rep(seq(2016,2096,5),3),
                      predicted=results.raw$pred[1:(17*3)],
                      lower=results.raw$pred[(17*3+1):(17*3*2)],
                      upper=results.raw$pred[(17*3*2+1):(17*3*3)],
                      group=rep(c("1","2","3"),each=17))

#assign the groups in the initial data
trajectories$group <- as.character(rep(model3splines$pprob[,2],17))

#merge the two sets
results <- left_join(results,select(trajectories,period5,proportion,group,nb))

#plot
ggplot(results)+
  geom_line(aes(period5,predicted,group=group,color=group),size=1)+
  geom_line(aes(period5,proportion,group=nb),color="grey")+
  geom_line(aes(period5,lower,group=group,color=group),linetype="dashed")+
  geom_line(aes(period5,upper,group=group,color=group),linetype="dashed")+
  ylab("Change in the number of speakers \n (proportional to the initial population size)")+
  xlab("Year")

#breakdown by group        
table(model3splines$pprob[,2])

#######################################################################################
#Attempt considering each run for each language
#######################################################################################
#new data set with proportion of speakers to number in 2016
trajectories <- simres %>% ungroup() %>% select(-period) %>% distinct(period5,language,run,.keep_all=T)
initial <- simres %>% ungroup() %>% filter(period==2016) %>% rename(initial=alive) %>% select(language,initial) %>% distinct()
trajectories <- left_join(trajectories,initial,by="language")
trajectories$proportion <- trajectories$alive/trajectories$initial

#add language number
trajectories$nb <- rep(1:(25*30),17)

#reconvert year to years after 2016
trajectories$period0 <- trajectories$period5-2016

ggplot(trajectories,aes(period5,proportion,group=nb))+
  geom_line()

#model with splines and 3 groups
model3splines <- lcmm(proportion ~ period0, random = ~ period0, subject = 'nb', 
                      mixture = ~ period0, ng = 3, idiag = TRUE, data=trajectories,
                      link="splines")

#put in data frame
results <- data.frame(period0=seq(0,80,5))
results.raw <- predictY(model3splines, newdata=results, var.time="period0", draws = T)
results <- data.frame(period5=rep(seq(2016,2096,5),3),
                      predicted=results.raw$pred[1:(17*3)],
                      lower=results.raw$pred[(17*3+1):(17*3*2)],
                      upper=results.raw$pred[(17*3*2+1):(17*3*3)],
                      group=rep(c("1","2","3"),each=17))

#assign the groups in the initial data
trajectories$group <- as.character(rep(model3splines$pprob[,2],17))

#merge the two sets
results <- left_join(results,select(trajectories,period5,proportion,group,nb))

#plot
ggplot(results)+
  geom_line(aes(period5,predicted,group=group,color=group),size=1)+
  geom_line(aes(period5,proportion,group=nb),color="grey")+
  geom_line(aes(period5,lower,group=group,color=group),linetype="dashed")+
  geom_line(aes(period5,upper,group=group,color=group),linetype="dashed")+
  ylab("Change in the number of speakers \n (proportional to the initial population size)")+
  xlab("Year")

#breakdown by group        
table(model3splines$pprob[,2])