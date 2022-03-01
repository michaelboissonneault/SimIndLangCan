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

library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)
  
#starting populations
sp <- readRDS("startingpopulations") 
  
#fertility parameters
fert.pm <- readRDS("fert.pm")

#mortality parameters
inuit.mort.pm <- readRDS("inuit.mort.pm")
indian.mort.pm <- readRDS("indian.mort.pm")

#intergenerational transmission, slope, & population number
int.nb <- readRDS("xitr")

#languages
languages <- unique(int.nb$language)

################################################################################
#2.DEFINE FERTILITY AND MORTALITY FUNCTIONS
################################################################################
#fertility######################################################################
#function
fert.fct <- function(age,calTime,duration){
  
  c <-  fert.pm[[1]] + fert.pm[[5]]*(calTime-2016)
  m <-  fert.pm[[2]] + fert.pm[[6]]*(calTime-2016)
  s1 <- fert.pm[[3]] + fert.pm[[7]]*(calTime-2016)
  s2 <- fert.pm[[4]] + fert.pm[[8]]*(calTime-2016)
  
  rate <- c * exp(-1*((age - m) / (ifelse(age<=m, s1, s2)*age))^2) 
  
  rate[age>=50] <- 0
  
  return(rate)
  
}

#Inuit mortality################################################################
inuit.mort.fct <- function(age,calTime,duration){
  
  A <- exp(inuit.mort.pm[[1]][1] + inuit.mort.pm[[1]][2]*calTime)
  B <- inuit.mort.pm[[2]][1] + inuit.mort.pm[[2]][2]*calTime
  C <- exp(inuit.mort.pm[[3]][1] + inuit.mort.pm[[3]][2]*calTime)
  
  return((A*exp(B*age) / (1 + A*exp(B*age)) + C) / 5)
  
}

#Indian mortality##############################################################
indian.mort.fct <- function(age,calTime,duration){
  
  A <- exp(indian.mort.pm[[1]][1] + indian.mort.pm[[1]][2]*calTime)
  B <- indian.mort.pm[[2]][1] + indian.mort.pm[[2]][2]*calTime
  C <- exp(indian.mort.pm[[3]][1] + indian.mort.pm[[3]][2]*calTime)
  
  return((A*exp(B*age) / (1 + A*exp(B*age)) + C) / 5)
  
}

################################################################################
#3.SET SIMULATION PARAMETERS
################################################################################
#Defining simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/2016", endDate="31/12/2125")

#Definition of maximal age
maxAge <- 110

#non-absorbing and absorbing states
sex <- "f"
fert <- c("0","1+")
stateSpace <- expand.grid(sex=sex,fert=fert)
absStates <- c("dead")

#Birth range
birth.dates.cat <- chron(dates=c(paste("1/1/",seq(1916,2121,5),sep="")),
                         format=c(dates="d/m/Y"), 
                         out.format=c(dates="d/m/year"))

#Initial state for new borns
initStates <- matrix(c("f","0"),nrow=1)

#Probability to be in state x at birth
initStatesProb <- 1

#age vector
age <- seq(0,95,5)

#Number of cores to run the simulations
cores <- 2

################################################################################
#DEFINE FUNCTION FOR RUNNING SIMULATIONS
################################################################################
overlappingcohorts <- function(x,y){
  
  #Transition pattern and assignment of functions specifying transition rates
  fertTrMatrix <- cbind(c("0->1+","1+->1+"),c("fert.fct","fert.fct"))
  allTransitions <- fertTrMatrix
  absTransitions <- rbind(c("dead",ifelse(y>=17 & y<=19,"inuit.mort.fct","indian.mort.fct")))
  transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
                                            absTransitions=absTransitions,
                                            stateSpace=stateSpace)
  
  #transitions triggering birth event
  fertTr <- fertTrMatrix[,1]
  
  #we simulate first the cohorts alive in 2016, then those born after
  
  #Cohorts alive in 2016
  initpop <- list()
  
  for (z in 1:20){
    
    df <- filter(sp,age==rev(age)[z],language==languages[y])
    
    #slope for int. trans.
    slope <- filter(int.nb,language==languages[y])$slope
    
    #group size
    n <- round(df$speaker)
    
    if(n>0){
      
      #intergenerational transmission
      it <- ifelse(n*slope > 1, 1, n*slope)
    
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
                               cores=cores,
                               seeds=round(runif(1,min=0, max=10^10)))
      
      #convert ID number to numeric
      simpop$ID <- as.numeric(simpop$ID)
      
      #number of births per year
      births <- lapply(seq(dmy('1-1-2016'),dmy('1-1-2125'),by='year'), function(t)
        round(length(filter(simpop,ID<=n,To=="f/1+",transitionTime>=t,transitionTime<t %m+% years(1))$ID)*it))
      
      #number alive per year
      alive <- lapply(seq(dmy('1-1-2016'),dmy('1-1-2125'),by='year'), function(t)
        n - length(filter(simpop,ID<=n,To=="dead",transitionTime<t)$ID))
      
    }else{
      
      births <- 0
      alive <- 0
      
    }
    
    #put in data frame
    initpop[[z]] <- data.frame(language=languages[y],period=2016:2125,period5=rep(seq(2016,2121,5),each=5),cohort=2011-rev(age)[z],alive=unlist(alive),births=unlist(births),run=x)
    
    }
  
  transientpop <- list(bind_rows(initpop))
  
  #22 periods of 5 years between 2016 and 2125
  for (z in 1:22){
    
    #Define simulation horizon
    simHorizon <- setSimHorizon(startDate=birth.dates.cat[z+20], endDate="31/Dec/2125")
    
    #cohort size
    n <- sum(filter(transientpop[[z]],language==languages[y],period5==z*5+2011)$births)
    
    if(n>0){
      
      #intergenerational transmission
      it <- ifelse(n*slope > 1, 1, n*slope)
      
      #generate birth dates
      birth.dates <- dates(birth.dates.cat[z+20]) + runif(n,min=0,max=birth.dates.cat[2]-birth.dates.cat[1])
      
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
                               cores=cores,
                               seeds=round(runif(1,min=0, max=10^10)))
      
      #convert ID number to numeric
      simpop$ID <- as.numeric(simpop$ID)
      
      #number of births per year
      births <- unlist(lapply(seq(dmy('1-1-2016'),dmy('1-1-2125'),by='year'), function(t)
        round(length(filter(simpop,ID<=n,To=="f/1+",transitionTime>=t,transitionTime<t %m+% years(1))$ID)*it)))
      
      #number alive per year
      alive <- unlist(lapply(seq(dmy('1-1-2016'),dmy('1-1-2125'),by='year'), function(t)
        n - length(unique(filter(simpop,ID<=n,birthDate>t)$ID)) - length(filter(simpop,ID<=n,To=="dead",transitionTime<t)$ID)))
      
    }else{
      
      births <- 0
      alive <- 0
      
    }
    
    #make data frame
    cohort <- data.frame(language=languages[y],period=2016:2125,period5=rep(seq(2016,2121,5),each=5),cohort=rep(z*5+2011,110),alive=alive,births=births,run=x)
    
    #add to list
    transientpop[[z+1]] <- bind_rows(transientpop[[z]],cohort)
    
  }

  saveRDS(transientpop[[23]],paste("finalpop",languages[y],x,sep=""))
  
}

################################################################################
#EXECUTE SIMULATIONS
################################################################################
#specify number of runs
runs <- 1:100

#language number (1 to n)
int.nb$number <- 1:length(int.nb$language)

#run simulations (a is the number of runs, b the language number)
lapply(runs,function(a) 
  lapply(1:max(int.nb$number), 
         function(b) overlappingcohorts(a,b))) 

#save in one dataframe
results_whole_100runs <- bind_rows(lapply(1:5, function(a) lapply(1:max(int.nb$number),function(b)
  readRDS(paste("finalpop",languages[b],a,sep=""))
)))

saveRDS(results_whole_100runs,"results_whole_100runs")

#attempt#########################################################################
lapply(1:1,function(a) 
  lapply(1, 
         function(b) overlappingcohorts(a,b))) 

df <- bind_rows(lapply(1:1, function(a) lapply(1,function(b)
  readRDS(paste("finalpop",languages[b],a,sep=""))
)))

r <- df %>% group_by(period) %>% summarise(alive=sum(alive),births=sum(births))

ggplot(r)+
  geom_line(aes(period,alive))
