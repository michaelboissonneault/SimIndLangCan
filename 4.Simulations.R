################################################################################
#IN THIS DO FILE: EXECUTION OF SIMULATIONS. OUTPUT IS NUMBER OF SPEAKERS FOR EACH
#COHORT BORN BETWEEN 1909 AND 2099 AND EACH YEAR BETWEEN 2014 AND 2099.
#CONTENT:
#1.PACKAGES AND DATA
#2.DEFINE FERTILITY AND MORTALITY FUNCTIONS
#3.RUN SIMULATIONS AND SAVE RESULTS 
#4.LOAD, COMBINE AND AGGREGATE RESULTS 
################################################################################
#1.PACKAGES AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)

#starting populations
sp <- readRDS("Parameters/startingpopulations")

#fertility parameters
fert.pm <- readRDS("Parameters/fert.pm")

#mortality parameters
inuit.mort.pm <- readRDS("Parameters/inuit.mort.pm")
indian.mort.pm <- readRDS("Parameters/indian.mort.pm")

#intergenerational transmission, slope, & population number
xitr <- readRDS("Parameters/xitr")

#languages (from smallest to largest)
languages <- xitr %>% arrange(speaker) %>% pull(language)

#year birth in starting population
sp$birthgroup <- 2009-sp$age

#Create data frame with one line per speaker, with information on language and year of birth
prep.fct <- function(x,z){
  
  language <- languages[z]
  birthyr <- floor(runif(round(filter(sp,language==languages[z],birthgroup==x)$speaker),x,x+5))
  return(data.frame(language=rep(language,length(birthyr)),
                    birthyr=birthyr))
  
}

df <- bind_rows(lapply(seq(1914,2009,5), function(a)
  lapply(1:length(languages), function(b) prep.fct(a,b))))

################################################################################
#2.DEFINE FERTILITY AND MORTALITY FUNCTIONS
################################################################################
#function specifying the age- and year-specific probability of given birth to a speaker
birth.prob <- function(x,y){
  
  c <-  fert.pm[[1]] + fert.pm[[5]]*(y-2014)
  m <-  fert.pm[[2]] + fert.pm[[6]]*(y-2014)
  s1 <- fert.pm[[3]] + fert.pm[[7]]*(y-2014)
  s2 <- fert.pm[[4]] + fert.pm[[8]]*(y-2014)
  
  rate <- c * exp(-1*((x - m) / (ifelse(x<=m, s1, s2)*x))^2)
  
  rate[x>=50] <- 0
  
  return(rate)
  
}

#function specifying the age- and year-specific probability of dying: inuit population
inuit.mort.prob <- function(x,y){
  
  A <- exp(inuit.mort.pm[[1]][1] + inuit.mort.pm[[1]][2]*y)
  B <- inuit.mort.pm[[2]][1] + inuit.mort.pm[[2]][2]*y
  C <- exp(inuit.mort.pm[[3]][1] + inuit.mort.pm[[3]][2]*y)
  
  return((A*exp(B*x) / (1 + A*exp(B*x)) + C) / 5)
  
}

#function specifying the age- and year-specific probability of dying: indian population
indian.mort.prob <- function(x,y){
  
  A <- exp(indian.mort.pm[[1]][1] + indian.mort.pm[[1]][2]*y)
  B <- indian.mort.pm[[2]][1] + indian.mort.pm[[2]][2]*y
  C <- exp(indian.mort.pm[[3]][1] + indian.mort.pm[[3]][2]*y)
  
  return((A*exp(B*x) / (1 + A*exp(B*x)) + C) / 5)
  
}

################################################################################
#3.RUN SIMULATIONS AND SAVE RESULTS 
################################################################################
#Function to perform simulation
sim <- function(languagename,runnumber){
  
  #select birth years for language z
  pop <- list(df %>% filter(language==languagename) %>% arrange(birthyr) %>% pull(birthyr))
  
  #select slope
  s <- xitr %>% filter(language==languagename) %>% pull(slope)
  
  #select mortality function
  mort.prob <- ifelse(languagename=="Inuvialuktun"|languagename=="Inuinnaqtun"|languagename=="Inuktitut",inuit.mort.prob,indian.mort.prob)
  
  #simulations
  for (i in 1:(2099-2013)){
    
    if(i>length(pop))break
    
    pop[[i+1]] <- c(unlist(lapply(pop[[i]], function(x) if(runif(1)>mort.prob(2013+i-x,2013+i)){x}else{})),
                    unlist(lapply(pop[[i]], function(x) if(runif(1)<birth.prob(2013+i-x,2013+i)*
                                                           ifelse(length(pop[[i]][pop[[i]]>=(x-2) & pop[[i]]<=(x+2)])*s>1,1,length(pop[[i]][pop[[i]]>=(x-2) & pop[[i]]<=(x+2)])*s))
                    {2013+i}else{})))
    
  } 
  
  return(bind_rows(lapply(1:length(pop), function(y) data.frame(language=languagename,
                                                                year=2013+y,
                                                                born=pop[[y]]+1,
                                                                run=runnumber))))
  
}

#remove Cree languages from language vector (because we do not include them in the simulations)
languages <- setdiff(languages,c("Moose Cree","Swampy Cree","Woods Cree","Plains Cree","East Cree","Naskapi"))

#set number of runs
runs <- 10^3

#Perform simulation and save results
lapply(1:runs, function(x) lapply(languages, function(y) saveRDS(sim(y,x),paste("Results/",y,x,sep=""))))

################################################################################
#4.LOAD, COMBINE AND AGGREGATE RESULTS 
################################################################################
#load simulation results and calculate number of speakers alive in each year, for each run
alive <- bind_rows(lapply(1:runs, function(x) 
  lapply(languages, function(y) readRDS(paste("Results/",y,x,sep="")) %>% group_by(year,language,run) %>% summarise(n=n()))))

#save results
saveRDS(alive,paste("Results/alive_noCree_",runs,"runs",sep=""))
