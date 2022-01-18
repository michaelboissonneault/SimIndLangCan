################################################################################
#IN THIS DO FILE: EXECUTION OF SIMULATIONS
#CONTENT:
  #1.PACKAGES AND DATA
  #2.SET SIMULATION PARAMETERS
  #3.DEFINE FUNCTION FOR RUNNING SIMULATIONS
  #4.RUN SIMULATION
################################################################################
#LOAD PACKAGES, SET DIRECTORY, LOAD PARAMETERS
################################################################################
rm(list=ls())

library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)

setwd("C:/Users/micha/Dropbox/Work/Projects/LinguisticDiversity/Canada/")

#starting populations
sp <- readRDS("Code/startingpopulations")

#fertility parameters & function
fert.list <- readRDS("Code/fertilityparameters")
fert.fct <- readRDS("Code/fertilityfunction")

#mortality parameters & function
inuit.list <- readRDS("Code/mortalityparameters")[[1]]
indian.list <- readRDS("Code/mortalityparameters")[[2]]

mort.inuit <- readRDS("Code/mortalityfunctions")[[1]]
mort.indian <- readRDS("Code/mortalityfunctions")[[2]]

#intergenerational transmission, slope, & population number
int.nb <- readRDS("Code/int.nb")

################################################################################
#SET SIMULATION PARAMETERS
################################################################################
#Defining simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/2016", endDate="31/12/2100")

#Definition of maximal age
maxAge <- 110

#non-absorbing and absorbing states
sex <- "f"
fert <- c("0","1+")
stateSpace <- expand.grid(sex=sex,fert=fert)
absStates <- c("dead")

#Birth range
birth.dates.cat <- chron(dates=c(paste("1/1/",seq(1901,2096,5),sep="")),
                         format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))

#Initial state for new borns
initStates <- matrix(c("f","0"),nrow=1)

#Probability to be in state x at birth
initStatesProb <- 1

#age vector
age <- seq(0,110,5)

################################################################################
#DEFINE FUNCTIONS FOR RUNNING SIMULATIONS
################################################################################
overlappingcohorts <- function(x,y){
  
  #Transition pattern and assignment of functions specifying transition rates
  fertTrMatrix <- cbind(c("0->1+","1+->1+"),c("fert.fct","fert.fct"))
  allTransitions <- fertTrMatrix
  absTransitions <- rbind(c("dead",ifelse(y>=17 & y<=19,"mort.inuit","mort.indian")))
  transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
                                            absTransitions=absTransitions,
                                            stateSpace=stateSpace)
  
  #transitions triggering birth event
  fertTr <- fertTrMatrix[,1]
  
  #we simulate separately the cohorts alive in 2016 and those born after
  
  #Cohorts in 2016
  initpop <- list()
  
  for (z in 1:23){
    
    df <- filter(sp,age==rev(age)[z],language==languages[y])
    
    #slope for int. trans.
    slope <- df$slope
    
    #group size
    n <- round(df$speaker)
    
    if(n>0){
      
      #intergenerational transmission
      it <- ifelse(log10(n) * slope > 1, 1, log10(n) * slope)
    
      #generate birth dates
      birth.dates <- dates(birth.dates.cat[z]) + runif(n,min=0,max=birth.dates.cat[2]-birth.dates.cat[1])
      
      #data frame with initial population
      initPop <- data.frame(ID=1:n,birthDate=birth.dates,initState="f/0")
      
      #Execute microsimulation
      simpop <- micSimParallel(initPop=initPop,
                               transitionMatrix=transitionMatrix,
                               absStates=absStates,
                               initStates=initStates,
                               initStatesProb=initStatesProb,
                               maxAge=maxAge,
                               simHorizon=simHorizon,
                               fertTr=fertTr,
                               cores=8,
                               seeds=round(runif(1,min=0, max=10^10)))
      
      #convert ID number to numeric
      simpop$ID <- as.numeric(simpop$ID)
      
      #number of births per year
      births <- lapply(seq(dmy('1-1-2016'),dmy('1-1-2100'),by='year'), function(t)
        round(length(filter(simpop,ID<=n,To=="f/1+",transitionTime>=t,transitionTime<t %m+% years(1))$ID)*it))
      
      #number alive per year
      alive <- lapply(seq(dmy('1-1-2016'),dmy('1-1-2100'),by='year'), function(t)
        n - length(filter(simpop,ID<=n,To=="dead",transitionTime<t)$ID))
      
    }else{
      
      births <- 0
      alive <- 0
      
    }
    
    #put in data frame
    initpop[[z]] <- data.frame(language=languages[y],period=2016:2100,period5=rep(seq(2016,2096,5),each=5),cohort=2011-rev(age)[z],alive=unlist(alive),births=unlist(births),run=x)
    
    }
  
  transientpop <- list(bind_rows(initpop))
  
  #17 periods of 5 years between 2016 and 2100
  for (z in 1:17){
    
    #Define simulation horizon
    simHorizon <- setSimHorizon(startDate=birth.dates.cat[z+23], endDate="31/Dec/2100")
    
    #slope for int. trans.
    slope <- unique(filter(sp,language==languages[y])$slope)
    
    #cohort size
    n <- sum(filter(transientpop[[z]],language==languages[y],period5==z*5+2011)$births)
    
    if(n>0){
      
      #intergenerational transmission
      it <- ifelse(log10(n) * slope > 1, 1, log10(n) * slope)
      
      #generate birth dates
      birth.dates <- dates(birth.dates.cat[z+23]) + runif(n,min=0,max=birth.dates.cat[2]-birth.dates.cat[1])
      
      #data frame with initial population
      initPop <- data.frame(ID=1:n,birthDate=birth.dates,initState="f/0")
      
      #Execute microsimulation
      simpop <- micSimParallel(initPop=initPop,
                               transitionMatrix=transitionMatrix,
                               absStates=absStates,
                               initStates=initStates,
                               initStatesProb=initStatesProb,
                               maxAge=maxAge,
                               simHorizon=simHorizon,
                               fertTr=fertTr,
                               cores=8,
                               seeds=round(runif(1,min=0, max=10^10)))
      
      #convert ID number to numeric
      simpop$ID <- as.numeric(simpop$ID)
      
      #number of births per year
      births <- unlist(lapply(seq(dmy('1-1-2016'),dmy('1-1-2100'),by='year'), function(t)
        round(length(filter(simpop,ID<=n,To=="f/1+",transitionTime>=t,transitionTime<t %m+% years(1))$ID)*it)))
      
      #number alive per year
      alive <- unlist(lapply(seq(dmy('1-1-2016'),dmy('1-1-2100'),by='year'), function(t)
        n - length(unique(filter(simpop,ID<=n,birthDate>t)$ID)) - length(filter(simpop,ID<=n,To=="dead",transitionTime<t)$ID)))
      
    }else{
      
      births <- 0
      alive <- 0
      
    }
    
    #make data frame
    cohort <- data.frame(language=languages[y],period=2016:2100,period5=rep(seq(2016,2096,5),each=5),cohort=rep(z*5+2011,85),alive=alive,births=births,run=x)
    
    #add to list
    transientpop[[z+1]] <- bind_rows(transientpop[[z]],cohort)
    
  }

  saveRDS(transientpop[[18]],paste("Results/finalpop",languages[y],x,sep=""))
  
}

################################################################################
#EXECUTE SIMULATIONS
################################################################################
#specify number of runs
runs <- 3

#run simulations
lapply(runs,function(a) lapply(c(28,9,34,46,35,51,37), function(b) overlappingcohorts(a,b)))
