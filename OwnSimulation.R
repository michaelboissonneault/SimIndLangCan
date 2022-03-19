################################################################################
#IN THIS DO FILE: EXECUTION OF SIMULATIONS. OUTPUT IS NUMBER OF SPEAKERS FOR EACH
#COHORT BORN BETWEEN 1901 AND 2100 AND EACH YEAR BETWEEN 2016 AND 2100.
#CONTENT:
#1.PACKAGES AND DATA
#2.DEFINE FERTILITY AND MORTALITY FUNCTIONS
#2.SET SIMULATION PARAMETERS
#3.DEFINE FUNCTION FOR RUNNING SIMULATIONS
#4.RUN SIMULATION

################################################################################
#1.PACKAGES AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)

#set ggplot2 theme for consistency
theme_set(theme_bw())  

#starting populations
sp <- readRDS("startingpopulations")

#fertility parameters
fert.pm <- readRDS("fert.pm")

#mortality parameters
inuit.mort.pm <- readRDS("inuit.mort.pm")
indian.mort.pm <- readRDS("indian.mort.pm")

#intergenerational transmission, slope, & population number
slope <- readRDS("xitr")

#languages (from smallest to largest)
languages <- slope %>% arrange(speaker) %>% pull(language)

#year birth in starting population
sp$birthgroup <- 2011-sp$age

#Create data frame with one line per speaker, with information on language and year of birth
prep.fct <- function(x,z){
  
  language <- languages[z]
  birthyr <- floor(runif(round(filter(sp,language==languages[z],birthgroup==x)$speaker),x,x+5))
  return(data.frame(language=rep(language,length(birthyr)),
                    birthyr=birthyr))
  
  }

df <- bind_rows(lapply(seq(1916,2011,5), function(a)
  lapply(1:length(languages), function(b) prep.fct(a,b))))

################################################################################
#2.DEFINE FUNCTIONS
################################################################################
#function specifying the age- and year-specific probability of given birth to a speaker
birth.prob <- function(x,y){
  
  c <-  fert.pm[[1]] + fert.pm[[5]]*(y-2016)
  m <-  fert.pm[[2]] + fert.pm[[6]]*(y-2016)
  s1 <- fert.pm[[3]] + fert.pm[[7]]*(y-2016)
  s2 <- fert.pm[[4]] + fert.pm[[8]]*(y-2016)
  
  rate <- c * exp(-1*((x - m) / (ifelse(x<=m, s1, s2)*x))^2)
  
  rate[x>=50] <- 0
  
  return(rate)
  
  }

#function specifying the age- and year-specific probability of dying
mort.prob <- function(x,y){

  A <- exp(15.3363 + -0.01218625*y)
  B <- 0.02152155 + 0.000044244*y
  C <- exp(20.998 + -.01265521*y)
  
  return((A*exp(B*x) / (1 + A*exp(B*x)) + C) / 5)
  
  }

################################################################################
#3.SIMULATION
################################################################################
#Function to perform simulation
sim <- function(languagename,runnumber){

  #select birth years for language z
  pop <- list(df %>% filter(language==languagename) %>% arrange(birthyr) %>% pull(birthyr))
  
  #select slope
  s <- slope %>% filter(language==languagename) %>% pull(slope)
   
  #simulations
  for (i in 1:(2100-2016)){
  
    if(i>length(pop))break
    
    pop[[i+1]] <- c(unlist(lapply(pop[[i]], function(x) if(runif(1)>mort.prob(2015+i-x,2015+i)){x}else{})),
                    unlist(lapply(pop[[i]], function(x) if(runif(1)<birth.prob(2015+i-x,2015+i)*
                                                           ifelse(length(pop[[i]][pop[[i]]>=(x-2) & pop[[i]]<=(x+2)])*s>1,1,length(pop[[i]][pop[[i]]>=(x-2) & pop[[i]]<=(x+2)])*s))
                      {2015+i}else{})))
    
    } 
    
  return(bind_rows(lapply(1:length(pop), function(y) data.frame(language=languagename,
                                                                year=2015+y,
                                                                born=pop[[y]]+1,
                                                                run=runnumber))))
  
}

#set number of runs
runs <- 1000

#Perform simulation and save results
lapply(1:runs, function(x) lapply(languages, function(y) saveRDS(sim(y,x),paste(y,x,sep=""))))

#load simulation results and calculate number of speakers alive in each year, for each run
alive <- bind_rows(lapply(1:runs, function(x) lapply(languages, function(y) readRDS(paste(languages,x,sep="")) %>% group_by(year,language,run) %>% summarise(n=n()))))

#data frame with zero speakers
addzerospeakers <- function(languagename,runnumber){
  
  reps <- 85-length(filter(alive,language==languagename,run==runnumber)$year)
  
  if(reps>0){
  data.frame(year=rev(2100:(2100-reps+1)),
             language=rep(languagename,reps),
             run=runnumber,
             n=0)
  }else{}
  
  }

zerospeakers <- bind_rows(lapply(languages, function(x) lapply(1:runs, function(y) addzerospeakers(x,y))))

#merge with whole results
alive <- bind_rows(alive,zerospeakers)

#extinction probability and mean number of speakers in year 2100
results <- alive %>% group_by(language) %>% 
  filter(year==2100,n>0) %>% 
  summarise(extinct.prob=1-n()/runs) %>% 
  right_join(alive %>% 
               group_by(language) %>% 
               filter(year==2100) %>%
               summarise(mean.speaker=mean(n))) 
  
  #change na for 1.00 probability
  results$extinct.prob <- ifelse(is.na(results$extinct.prob),1,results$extinct.prob) 

  #arrange by extinction probability, number of speakers
  results <- results %>% arrange(-extinct.prob,mean.speaker)
  
#Plot evolution number of speakers through years
ggplot(alive,aes(year,n,group=run))+
  geom_line()+
  xlim(2016,2100)+
  facet_wrap(.~language,scales="free")

#Plot distribution of speakers across runs in year 2100
ggplot(filter(alive,year==2100),aes(n))+
  geom_histogram()+
  facet_wrap(.~language,scales="free")
