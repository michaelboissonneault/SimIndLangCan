################################################################################
#IN THIS DO FILE: DEFINITION OF PARAMETRIC FUNCTIONS DESCRIBING THE MORTALITY PATTERNS 
#OF THE INUIT AND INDIAN POPULATIONS, BETWEEN 2016 AND 2100 AND FOR ALL AGES.
#FUNCTIONS ARE SPECIFIED BASED ON PARAMETERS ESTIMATED FROM THE PROJECTION 
#OF THE UN 2019 POPULATION PROSPECTS FOR LOWER MIDDLE INCOME COUNTRIES (INUIT POPULATION)
#AND UPPER MIDDLE INCOME COUNTRIES (INDIAN POPULATION).
#SIMULATIONS ARE RUN TO DETERMINE WHETHER THE LIFE EXPECTANCY RESULTING FROM THE SIMULATIONS
#CORRESPOND TO THE LIFE EXPECTANCY IN THE UN PROJECTIONS.
#CONTENT:
  #1.PACKAGES AND DATA
  #2.INUIT POPULATION
    #2.1 PARAMETER ESTIMATES
    #2.2 SIMULATION 
  #3.INDIAN POPULATION
    #3.1 PARAMETER ESTIMATES
    #3.2 SIMULATION 
################################################################################
#1.PACKAGES AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)
library(MortalityLaws)

#life tables, world population prospects 2019 
wpp2019lt <- read.csv("WPP2019_Life_Table_Medium.csv")

################################################################################
#2. DEFINITION OF PARAMETERS FOR SIMULATION AND ESTIMATIONS
################################################################################
#Defining simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/2000", endDate="31/12/2109")

#Definition of maximal age
maxAge <- 110

# Defintion of nonabsorbing and absorbing states
sex <- c("f")
stateSpace <- sex
attr(stateSpace,"name") <- "sex"
absStates <- "dead"

#Definition of an initial population 
dts <- c("31/12/1999")
birthDates <- chron(dates=dts,format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))
initStates <- "f"
n <- 10^4
initPop <- data.frame(ID=1:n,birthDate=rep(birthDates,n),initState=rep(initStates,n))

#Age vector
x <- c(0,1,seq(5,100,5)) 

#year vector
y <- seq(2018,2098,5)

################################################################################
#3.INUIT POPULATION
################################################################################
#3.1 PARAMETER ESTIMATES#######################################################
qx <- lapply(y, function(x) 
  filter(wpp2019lt,Location=="Lower-middle-income countries",Sex=="Total",MidPeriod==x)$qx)

#Kannisto-Makeham model with Poisson loss function provides OK fit (especially at older ages) and is very simple
pm <- lapply(1:17, function(i) coefficients(MortalityLaw(x = x,
                                                         qx = qx[[i]],
                                                         law = "kannisto_makeham",
                                                         opt.method = "poissonL")))

#put parameters in dataframe
pm.df <- data.frame(year=rep(y,each=3),
                    label=rep(c("A","B","C"),17),
                    value=unlist(pm))
                      
#visualize change over time
ggplot(pm.df,aes(year,value))+
  geom_point()+
  facet_wrap(.~label,scales="free")

#estimate the change in parameter values across years using a linear model, excluding outliers 
A <- coefficients(lm(log(filter(pm.df,label=="A")$value) ~ y))
B <- coefficients(lm(filter(pm.df,label=="B")$value ~ y))
C <- coefficients(lm(log(filter(pm.df,label=="C")$value) ~ y))

#save parameters
saveRDS(list(A,B,C),"inuit.mort.pm")

#3.2. SIMULATION##################################################################
le <- list()
  
for (i in 1:17){
    
  #year in function set to remain the same throughout a cohort's life
  mort.fct <- function(age,calTime,duration){
    
    A <- exp(A[1] + A[2]*y[i])
    B <- B[1] + B[2]*y[i]
    C <- exp(C[1] + C[2]*y[i])
    
    return((A*exp(B*age) / (1 + A*exp(B*age)) + C)/5)
    
  }
    
  #Transition pattern and assignment of functions specifying transition rates
  absTransitions <- c("dead","mort.fct")
  transitionMatrix <- buildTransitionMatrix(allTransitions=NULL,absTransitions=absTransitions, stateSpace=stateSpace)
  
  #specify number of cores on computer
  cores <- 2
  
  #Execute microsimulation 
  pop <- micSimParallel(initPop=initPop, transitionMatrix=transitionMatrix, absStates=absStates, 
                        maxAge=maxAge, simHorizon=simHorizon,cores=cores,seeds=round(runif(1,min=0,max=10^10)))
    
  le[[i]] <- mean(pop$transitionAge,na.rm=T)
    
  }
  
#compare results with values contained in UN projection data
value <- c(filter(wpp2019lt,Location=="Lower-middle-income countries",Sex=="Total",AgeGrpStart==0,MidPeriod>=2018)$ex,unlist(le))
label <- rep(c("UN model","Own model"),each=length(y))
comp <- data.frame(year=y,label=label,value=value)
ggplot(comp,aes(year,value,group=label,linetype=label))+
  geom_line()+
  theme_bw()

################################################################################
#3.INDIAN POPULATION
################################################################################
#3.1 PARAMETER ESTIMATION#######################################################
qx <- lapply(y, function(x) 
  filter(wpp2019lt,Location=="Upper-middle-income countries",Sex=="Total",MidPeriod==x)$qx)

#Kannisto-Makeham model with Poisson loss function 
pm <- lapply(1:17, function(i) coefficients(MortalityLaw(x = x,
                                                         qx = qx[[i]],
                                                         law = "kannisto_makeham",
                                                         opt.method = "poissonL")))

#put parameters in dataframe
pm.df <- data.frame(year=rep(y,each=3),
                    label=rep(c("A","B","C"),17),
                    value=unlist(pm))

#visualize change over time
ggplot(pm.df,aes(year,value))+
  geom_point()+
  facet_wrap(.~label,scales="free")

#estimate the change in parameter values across years using a linear model, excluding outliers 
A <- coefficients(lm(log(filter(pm.df,label=="A",year!=2063,year!=2068)$value) ~ c(seq(2018,2058,5),seq(2073,2098,5))))
B <- coefficients(lm(filter(pm.df,label=="B",year!=2063,year!=2068)$value ~ c(seq(2018,2058,5),seq(2073,2098,5))))
C <- coefficients(lm(log(filter(pm.df,label=="C",year!=2063,year!=2068)$value) ~ c(seq(2018,2058,5),seq(2073,2098,5))))

#save parameters
saveRDS(list(A,B,C),"indian.mort.pm")

#3.2. SIMULATION##################################################################
le <- list()

for (i in 1:17){
  
  #year in function set to remain the same throughout a cohort's life
  mort.fct <- function(age,calTime,duration){
    
    A <- exp(A[1] + A[2]*y[i])
    B <- B[1] + B[2]*y[i]
    C <- exp(C[1] + C[2]*y[i])
    
    return((A*exp(B*age) / (1 + A*exp(B*age)) + C)/5)
    
  }
  
  #Transition pattern and assignment of functions specifying transition rates
  absTransitions <- c("dead","mort.fct")
  transitionMatrix <- buildTransitionMatrix(allTransitions=NULL,absTransitions=absTransitions, stateSpace=stateSpace)
  
  #specify number of cores on computer
  cores <- 2
  
  #Execute microsimulation 
  pop <- micSimParallel(initPop=initPop, transitionMatrix=transitionMatrix, absStates=absStates, 
                        maxAge=maxAge, simHorizon=simHorizon,cores=cores,seeds=round(runif(1,min=0,max=10^10)))
  
  le[[i]] <- mean(pop$transitionAge,na.rm=T)
  
}

#compare results with values contained in UN projection data
value <- c(filter(wpp2019lt,Location=="Upper-middle-income countries",Sex=="Total",AgeGrpStart==0,MidPeriod>=2018)$ex,unlist(le))
label <- rep(c("UN model","Own model"),each=length(y))
comp <- data.frame(year=y,label=label,value=value)
ggplot(comp,aes(year,value,group=label,linetype=label))+
  geom_line()+
  theme_bw()
