################################################################################
#IN THIS DO FILE: DEFINITION OF PARAMETRIC FUNCTIONS THAT DETERMINE THE FERTILITY
#OF THE WHOLE ABORIGINAL POPULATION, BETWEEN 2016 AND 2100 AND FOR ALL AGES.
#FUNCTIONS ARE SPECIFIED BASED ON THE PROJECTION 
#OF THE UN 2019 POPULATION PROSPECTS FOR LOWER MIDDLE INCOME COUNTRIES
#SIMULATIONS ARE RUN TO DETERMINE WHETHER THE TFR RESULTING FROM THE SIMULATIONS
#CORRESPOND TO THE TFR IN THE UN PROJECTIONS.
#CONTENT:
  #1.PACKAGES AND DATA
  #2.PARAMETER ESTIMATION
  #3.DEFINITION OF FUNCTION
  #4.SAVE PARAMETERS AND FUNCTION
  #5.SIMULATIONS
################################################################################
#1.DIRECTORY, PACKAGES AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)

#UN population projection 2019 for World bank group lower middle income countries
unproj <- read.csv("UNFertilityLowerMiddleIncome.csv",sep=";")
unproj <- pivot_longer(unproj,names_to = "age",values_to = "value",-X)

################################################################################
#2. ESTIMATING PARAMETERS
################################################################################
#age in middle
age <- seq(17.5,47.5,5)

#year in middle
year <- seq(2017.5,2097.5,5)

#values divided by 1000 because UN gives rates per 1000, and by 2 because population is composed of women only
rates <- unproj$value/1000/2

#data frame
df <- data.frame(age=age,year=rep(year,each=length(age)),rates=rates)

#add 0 values for age 12.5 and 52.5
df <- arrange(bind_rows(df,data.frame(age=12.5,year=year,rates=0),data.frame(age=52.5,year=year,rates=0)),year,age)

#new age vector
age <- unique(df$age)

#visualize change over years
ggplot(df,aes(age,rates))+
  geom_point()+
  facet_wrap(~year)

#peristera kostaki model for fertility (2007 Dem. Res.)
#year 2017.5
y <- filter(df,year==2017.5)$rates
pk2017 <- coef(nls(y ~ c * exp(-1*((age - m) / (ifelse(age<=26, s1, s2)*age))^2),
                     start=list(c=.09, m=26, s1=.4, s2=.3)))

#year 2097.5
y <- filter(df,year==2097.5)$rates
pk2097 <- coef(nls(y ~ c * exp(-1*((age - m) / (ifelse(age<=33, s1, s2)*age))^2),
                   start=list(c=.06, m=33, s1=.65, s2=.15)))

#we assume linear change between year 2017 and 2097
#find slopes of change in each coefficient
mc <- (pk2017[1] - pk2097[1]) / (2017.5 - 2097.5) 
mm <- (pk2017[2] - pk2097[2]) / (2017.5 - 2097.5) 
ms1 <- (pk2017[3] - pk2097[3]) / (2017.5 - 2097.5) 
ms2 <- (pk2017[4] - pk2097[4]) / (2017.5 - 2097.5) 

#extract single values for year 2016
c <- pk2017[1] - mc*1.5
m <- pk2017[2] - mm*1.5
s1 <- pk2017[3] - ms1*1.5
s2 <- pk2017[4] - ms2*1.5

#put parameters in list
fert.parameters <- list(c,m,s1,s2,mc,mm,ms1,ms2)

################################################################################
#3. Definition of function
################################################################################
#function
fert.fct <- function(age,calTime,duration){
  
  c <-  fert.parameters[[1]] + fert.parameters[[5]]*(calTime-2016)
  m <-  fert.parameters[[2]] + fert.parameters[[6]]*(calTime-2016)
  s1 <- fert.parameters[[3]] + fert.parameters[[7]]*(calTime-2016)
  s2 <- fert.parameters[[4]] + fert.parameters[[8]]*(calTime-2016)
  
  rate <- c * exp(-1*((age - m) / (ifelse(age<=m, s1, s2)*age))^2) 
  
  return(rate)
  
}

#check fit 
age <- seq(17.5,47.5,5)
year <- seq(2017.5,2097.5,5)

model <- unlist(lapply(year,function(x) fert.fct(age,x)))
un <- unproj$value/2000

checkfit <- data.frame(age=age,year=rep(year,each=length(age)),model=model,un=un)

ggplot(checkfit,aes(age,model))+
  geom_line()+
  geom_point(aes(age,un))+
  facet_wrap(~year)

sum(fert.fct(10:60,2016))*2
sum(fert.fct(10:60,2099))*2

################################################################################
#4. SAVE PARAMETERS AND FUNCTION
################################################################################
#list with parameters (both populations) 
saveRDS(fert.parameters,"fertilityparameters")

#list with mortality functions (both populations)
saveRDS(fert.fct,"fertilityfunction")

################################################################################
#5. SIMULATION
################################################################################
#Simulation parameters##########################################################
#Defining simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/2000", endDate="31/12/2070")

#Definition of maximal age
maxAge <- 70

#non-absorbing and absorbing states
sex <- "f"
fert <- c("0","1+")
stateSpace <- expand.grid(sex=sex,fert=fert)
absStates <- c("dead")

#Birth range
birth.dates.cat <- chron(dates=c("12/12/1999"),format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))

#Initial state for new borns
initStates <- matrix(c("f","0"),nrow=1)

#Probability to be in state x at birth
initStatesProb <- 1

#Transition pattern and assignment of functions specifying transition rates
fertTrMatrix <- cbind(c("0->1+","1+->1+"),c("fert.fct","fert.fct"))
allTransitions <- fertTrMatrix
absTransitions <- rbind(c("dead","mort.fct"))
transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
                                          absTransitions=absTransitions,
                                          stateSpace=stateSpace)

#transitions triggering birth event
fertTr <- fertTrMatrix[,1]

#population size
n <- 10^2

#generate birth dates
birth.dates <- rep(dates(birth.dates.cat),n)

#data frame with initial population
initPop <- data.frame(ID=1:n,birthDate=birth.dates,initState="f/0")

#mortality function (equals zero)
mort.fct <- function(age,calTime,duration){
  
  rate <- 0

  return(rate)
  
}

#run simulation ################################################################
tfr <- list()

for (i in 1:17){

  #functions that determines fertility rates 
  fert.fct <- function(age,calTime,duration){
    
    c <-  fert.parameters[[1]] + fert.parameters[[5]]*(year[i]-2016)
    m <-  fert.parameters[[2]] + fert.parameters[[6]]*(year[i]-2016)
    s1 <- fert.parameters[[3]] + fert.parameters[[7]]*(year[i]-2016)
    s2 <- fert.parameters[[4]] + fert.parameters[[8]]*(year[i]-2016)
    
    rate <- c * exp(-1*((age - m) / (ifelse(age<=m, s1, s2)*age))^2) 
    
    return(rate)
    
    }

  #set number of cores
  cores <- 2
  
  #Run simulation#################################################################
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
  
  #ID as numeric
  simpop$ID <- as.numeric(simpop$ID)
  
  #initial population
  ip <- filter(simpop,ID<=n)
  
  #first births
  fb <- length(filter(ip,From=="f/0",To=="f/1+")$ID)
  
  #second births
  sb <- length(filter(ip,From=="f/1+",To=="f/1+")$ID)
  
  #Total
  tb <- fb + sb
  
  #women only
  tfr[[i]] <- round( tb / n , 2) * 2
  
  }

#comparison#####################################################################
un <- (unproj %>% group_by(X) %>% summarise(tfr=sum(value)/1000*5))$tfr

results <- data.frame(year=year,tfr=c(unlist(tfr),un),label=rep(c("Model","UN"),each=length(year)))

ggplot(results,aes(year,tfr,group=label,linetype=label))+
  geom_line()+
  theme_bw()

#END OF DO FILE#################################################################