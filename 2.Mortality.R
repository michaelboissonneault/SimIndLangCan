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
  #2.PARAMETER ESTIMATION
  #3.DEFINITION OF FUNCTIONS
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

#set directory
setwd("C:/Users/micha/Dropbox/Work/Projects/LinguisticDiversity/Canada/")

#un population projection 2019 
unproj <- read.csv("Data/WPP2019_Life_Table_Medium.csv")

################################################################################
#2.PARAMETER ESTIMATION
################################################################################
#2.1 Inuit population: change by age############################################ 
#select relevant information 
inuitmort <- unproj %>% 
  filter(Location=="Lower-middle-income countries",Sex=="Total",MidPeriod>=2017) %>%
  select(Location,MidPeriod,AgeGrpStart,AgeGrpSpan,qx,ex)

#make year vector
year <- seq(2018,2098,5)
  
#children: exponential model
child.a <- list()
child.b <- list()
    
for (i in 1:17){
      
  y <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart<10)$qx
  x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart<10)$AgeGrpStart
    
  model <- lm(log(y) ~ x)
  
  child.a[[i]] <- coef(model)[[1]]
  child.b[[i]] <- coef(model)[[2]]
      
  }
    
#check fit
i <- 1
x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart<15)$AgeGrpStart
plot(filter(inuitmort,MidPeriod==year[i],AgeGrpStart<15)$qx)
lines(exp(child.a[[i]] + child.b[[i]]*x))
    
#young ages: linear model
young.a <- list()
young.b <- list()
    
for (i in 1:17){
      
  y <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>5,AgeGrpStart<35)$qx
  x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>5,AgeGrpStart<35)$AgeGrpStart
    
  model <- lm(y ~ x)
      
  young.a[[i]] <- coef(model)[[1]]
  young.b[[i]] <- coef(model)[[2]]
      
  }
    
#check fit
i <- 1
x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>0,AgeGrpStart<40)$AgeGrpStart
plot(x,filter(inuitmort,MidPeriod==year[i],AgeGrpStart>0,AgeGrpStart<40)$qx)
lines(x,young.a[[i]] + young.b[[i]]*x)
    
#middle ages: Gompertz model
middle.a <- list()
middle.b <- list()
    
for (i in 1:17){
      
  y <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<85)$qx
  x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<85)$AgeGrpStart
      
  model <- nls(y ~ a * exp(b*x),start=list(a=0.00001,b=0.07))
      
  middle.a[[i]] <- coef(model)[[1]]
  middle.b[[i]] <- coef(model)[[2]]
      
  }
    
#check fit
i <- 1
x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<90)$AgeGrpStart
plot(x,filter(inuitmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<90)$qx)
lines(x,middle.a[[i]]*exp(middle.b[[i]]*x))
    
#old ages: logarithmic function
old.a <- list()
old.b <- list()

for (i in 1:17){
      
  y <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>75)$qx
  x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>75)$AgeGrpStart
      
  model <- lm(y ~ log(x))
      
  old.a[[i]] <- coef(model)[[1]]
  old.b[[i]] <- coef(model)[[2]]
      
  }
    
#check fit
i <- 1
x <- filter(inuitmort,MidPeriod==year[i],AgeGrpStart>=75)$AgeGrpStart
plot(x,filter(inuitmort,MidPeriod==year[i],AgeGrpStart>=75)$qx)
lines(x,old.a[[i]] + old.b[[i]]*log(x))

#2.2 Inuit population: change over time########################################
#children
  #a parameter
  y <- unlist(child.a)
  plot(y)
  inuit.child.a <- coef(lm(exp(y*-1) ~ year))
  lines(log(inuit.child.a[[1]] + inuit.child.a[[2]]*year)*-1)
  
  #b parameter
  y <- unlist(child.b)
  plot(y)
  inuit.child.b <- coef(lm(y ~ year + I(year^2)))
  lines(inuit.child.b[[1]] + inuit.child.b[[2]]*year + inuit.child.b[[3]]*year^2)

#young ages
  #a parameter
  y <- unlist(young.a)
  plot(y)
  inuit.young.a <- coef(lm(y ~ year + I(year^2)))
  lines(inuit.young.a[[1]] + inuit.young.a[[2]]*year + inuit.young.a[[3]]*year^2)
  
  #b parameter
  y <- unlist(young.b)
  plot(y)
  inuit.young.b <- coef(lm(log(y) ~ year))
  lines(exp(inuit.young.b[[1]] + inuit.young.b[[2]]*year))

#middle ages (Gompertz)
  #a parameter
  y <- unlist(middle.a)
  plot(y)
  inuit.middle.a <- coef(lm(log(y) ~ year))
  lines(exp(inuit.middle.a[[1]] + inuit.middle.a[[2]]*year))
  
  #b parameter
  y <- unlist(middle.b)
  plot(y)
  inuit.middle.b <- coef(lm(y ~ year))
  lines(inuit.middle.b[[1]] + inuit.middle.b[[2]]*year)

#older ages (logarithmic)
  #a parameter
  y <- unlist(old.a)
  plot(y)
  inuit.old.a <- coef(lm(y ~ year + I(year^2)))
  lines(inuit.old.a[[1]] + inuit.old.a[[2]]*year + inuit.old.a[[3]]*year^2)
  
  #b parameter
  y <- unlist(old.b)
  plot(y)
  inuit.old.b <- coef(lm(y ~ year + I(year^2)))
  lines(inuit.old.b[[1]] + inuit.old.b[[2]]*year + inuit.old.b[[3]]*year^2)

#2.3 Indian population: change by age########################################### 
#select relevant information
indianmort <- unproj %>% 
    filter(Location=="Upper-middle-income countries",Sex=="Total",MidPeriod>=2017) %>%
    select(Location,MidPeriod,AgeGrpStart,AgeGrpSpan,qx,ex)
  
#children: exponential model
child.a <- list()
child.b <- list()
  
for (i in 1:17){
    
  y <- filter(indianmort,MidPeriod==year[i],AgeGrpStart<10)$qx
  x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart<10)$AgeGrpStart
    
  model <- lm(log(y) ~ x)
    
  child.a[[i]] <- coef(model)[[1]]
  child.b[[i]] <- coef(model)[[2]]
    
  }
  
#check fit
i <- 1
x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart<15)$AgeGrpStart
plot(filter(indianmort,MidPeriod==year[i],AgeGrpStart<15)$qx)
lines(exp(child.a[[i]] + child.b[[i]]*x))
  
#young ages: linear model
young.a <- list()
young.b <- list()
  
for (i in 1:17){
    
  y <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>5,AgeGrpStart<35)$qx
  x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>5,AgeGrpStart<35)$AgeGrpStart
    
  model <- lm(y ~ x)
    
  young.a[[i]] <- coef(model)[[1]]
  young.b[[i]] <- coef(model)[[2]]
    
  }
  
#check fit
i <- 1
x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>0,AgeGrpStart<40)$AgeGrpStart
plot(x,filter(indianmort,MidPeriod==year[i],AgeGrpStart>0,AgeGrpStart<40)$qx)
lines(x,young.a[[i]] + young.b[[i]]*x)
  
#middle ages: Gompertz model
middle.a <- list()
middle.b <- list()
  
for (i in 1:17){
    
  y <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<85)$qx
  x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<85)$AgeGrpStart
    
  model <- nls(y ~ a * exp(b*x),start=list(a=0.00001,b=0.07))
    
  middle.a[[i]] <- coef(model)[[1]]
  middle.b[[i]] <- coef(model)[[2]]
    
  }
  
#check fit
i <- 1
x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<90)$AgeGrpStart
plot(x,filter(indianmort,MidPeriod==year[i],AgeGrpStart>35,AgeGrpStart<90)$qx)
lines(x,middle.a[[i]]*exp(middle.b[[i]]*x))
  
#old ages: logarithmic function
old.a <- list()
old.b <- list()
  
for (i in 1:17){
    
  y <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>75)$qx
  x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>75)$AgeGrpStart
    
  model <- lm(y ~ log(x))
    
  old.a[[i]] <- coef(model)[[1]]
  old.b[[i]] <- coef(model)[[2]]
    
  }
  
#check fit
i <- 1
x <- filter(indianmort,MidPeriod==year[i],AgeGrpStart>=75)$AgeGrpStart
plot(x,filter(indianmort,MidPeriod==year[i],AgeGrpStart>=75)$qx)
lines(x,old.a[[i]] + old.b[[i]]*log(x))
  
#2.4 Indian population: change over time##########################################
#children 
  #a parameter
  y <- unlist(child.a)
  plot(y)
  indian.child.a <- coef(lm(exp(y*-1) ~ year))
  lines(log(indian.child.a[[1]] + indian.child.a[[2]]*year)*-1)
  
  #b parameter
  y <- unlist(child.b)
  plot(y)
  indian.child.b <- coef(lm(y ~ year + I(year^2)))
  lines(indian.child.b[[1]] + indian.child.b[[2]]*year + indian.child.b[[3]]*year^2)
  
#young ages
  #a parameter
  y <- unlist(young.a)
  plot(y)
  indian.young.a <- coef(lm(y ~ year + I(year^2)))
  lines(indian.young.a[[1]] + indian.young.a[[2]]*year + indian.young.a[[3]]*year^2)
  
  #b parameter
  y <- unlist(young.b)
  plot(y)
  indian.young.b <- coef(lm(log(y) ~ year))
  lines(exp(indian.young.b[[1]] + indian.young.b[[2]]*year))
  
#middle ages (Gompertz)
  #a parameter
  y <- unlist(middle.a)
  plot(y)
  indian.middle.a <- coef(lm(log(y) ~ year))
  lines(exp(indian.middle.a[[1]] + indian.middle.a[[2]]*year))
  
  #b parameter
  y <- unlist(middle.b)
  plot(y)
  indian.middle.b <- coef(lm(y ~ year))
  lines(indian.middle.b[[1]] + indian.middle.b[[2]]*year)
  
#older ages (logarithmic)
  #a parameter
  y <- unlist(old.a)
  plot(y)
  indian.old.a <- coef(lm(y ~ year + I(year^2)))
  lines(indian.old.a[[1]] + indian.old.a[[2]]*year + indian.old.a[[3]]*year^2)
  
  #b parameter
  y <- unlist(old.b)
  plot(y)
  indian.old.b <- coef(lm(y ~ year + I(year^2)))
  lines(indian.old.b[[1]] + indian.old.b[[2]]*year + indian.old.b[[3]]*year^2)
  
################################################################################
#3. DEFINITION OF FUNCTIONS
################################################################################
#3.1 Inuit population###########################################################
#put parameters in lists
inuit.list <- list(inuit.child.a,inuit.child.b,
                   inuit.young.a,inuit.young.b,
                   inuit.middle.a,inuit.middle.b,
                   inuit.old.a,inuit.old.b)
                   
  
#function
mort.inuit <- function(age,calTime,duration){
  
  m1a <- log(inuit.list[[1]][1] + inuit.list[[1]][2]*calTime)*-1
  m1b <-     inuit.list[[2]][1] + inuit.list[[2]][2]*calTime + inuit.list[[2]][3]*calTime^2
  
  m2a <-     inuit.list[[3]][1] + inuit.list[[3]][2]*calTime + inuit.list[[3]][3]*calTime^2
  m2b <- exp(inuit.list[[4]][1] + inuit.list[[4]][2]*calTime)
  
  m3a <- exp(inuit.list[[5]][1] + inuit.list[[5]][2]*calTime)
  m3b <-     inuit.list[[6]][1] + inuit.list[[6]][2]*calTime
  
  m4a <-     inuit.list[[7]][1] + inuit.list[[7]][2]*calTime + inuit.list[[7]][3]*calTime^2
  m4b <-     inuit.list[[8]][1] + inuit.list[[8]][2]*calTime + inuit.list[[8]][3]*calTime^2
  
  rate <- ifelse(age<7.5, exp(m1a + m1b*age),
                 ifelse(age>=7.5 & age<37.5, m2a + m2b*age,
                        ifelse(age>=37.5 & age<80, m3a*exp(m3b*age), m4a + m4b*log(age))))
  
  rate <- rate/5
  
  return(rate)
  
}

#check all ages and years
age <- rep(unique(inuitmort$AgeGrpStart),17)
year <- rep(unique(inuitmort$MidPeriod),each=length(unique(age)))
value <-  c(inuitmort$qx,unlist(lapply(unique(year),function(x) lapply(unique(age),function(y) mort.inuit(y,x))))*5)
label <- rep(c("UN","Own"),each=length(inuitmort$qx))

df <- data.frame(year=year,age=age,value=value,label=label)

#plot 
ggplot(df,aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

ggplot(filter(df,age<50),aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

ggplot(filter(df,age>=50),aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

#3.2 Indian population##########################################################
#put parameters in lists
indian.list <- list(indian.child.a,indian.child.b,
                   indian.young.a,indian.young.b,
                   indian.middle.a,indian.middle.b,
                   indian.old.a,indian.old.b)


#function
mort.indian <- function(age,calTime,duration){
  
  m1a <- log(indian.list[[1]][1] + indian.list[[1]][2]*calTime)*-1
  m1b <-     indian.list[[2]][1] + indian.list[[2]][2]*calTime + indian.list[[2]][3]*calTime^2
  
  m2a <-     indian.list[[3]][1] + indian.list[[3]][2]*calTime + indian.list[[3]][3]*calTime^2
  m2b <- exp(indian.list[[4]][1] + indian.list[[4]][2]*calTime)
  
  m3a <- exp(indian.list[[5]][1] + indian.list[[5]][2]*calTime)
  m3b <-     indian.list[[6]][1] + indian.list[[6]][2]*calTime
  
  m4a <-     indian.list[[7]][1] + indian.list[[7]][2]*calTime + indian.list[[7]][3]*calTime^2
  m4b <-     indian.list[[8]][1] + indian.list[[8]][2]*calTime + indian.list[[8]][3]*calTime^2
  
  rate <- ifelse(age<7.5, exp(m1a + m1b*age),
                 ifelse(age>=7.5 & age<37.5, m2a + m2b*age,
                        ifelse(age>=37.5 & age<80, m3a*exp(m3b*age), m4a + m4b*log(age))))
  
  rate <- rate/5
  
  return(rate)
  
}

#check all ages and years
age <- rep(unique(indianmort$AgeGrpStart),17)
year <- rep(unique(indianmort$MidPeriod),each=length(unique(age)))
value <-  c(indianmort$qx,unlist(lapply(unique(year),function(x) lapply(unique(age),function(y) mort.indian(y,x))))*5)
label <- rep(c("UN","Own"),each=length(indianmort$qx))

df <- data.frame(year=year,age=age,value=value,label=label)

#plot 
ggplot(df,aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

ggplot(filter(df,age<50),aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

ggplot(filter(df,age>=50),aes(age,value,group=label,linetype=label))+
  geom_line()+
  facet_wrap(~year)

#comparison
age <- seq(0,80,5)
year <- rep(seq(2018,2098,5),each=length(age))
inuit <- unlist(lapply(unique(year),function(x) mort.inuit(age,x)))
indian <- unlist(lapply(unique(year),function(x) mort.indian(age,x)))
df <- data.frame(year=year,age=age,value=c(inuit,indian),label=rep(c("Inuit","Indian"),each=length(year)))

ggplot(df,aes(age,value,group=label,color=label))+
  geom_line()+
  facet_wrap(~year)

#############################################################################
#4.SAVE PARAMETERS AND FUNCTIONS
#############################################################################
#list with mortality parameters (both populations) 
saveRDS(list(inuit.list,indian.list),"Code/mortalityparameters")

#list with mortality functions (both populations)
saveRDS(list(mort.inuit,mort.indian),"Code/mortalityfunctions")

################################################################################
#5. SIMULATIONS
################################################################################
#Define parameters##############################################################
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
n <- 10^2
initPop <- data.frame(ID=1:n,birthDate=rep(birthDates,n),initState=rep(initStates,n))

#year vector
yr <- seq(2018,2098,5)

#Run simulation: Inuit population###############################################
le <- list()
  
for (i in 1:17){
    
  #year in function set to remain the same throughout a cohort's life
  mort.fct <- function(age,calTime,duration){
    
    m1a <- log(inuit.list[[1]][1] + inuit.list[[1]][2]*yr[i])*-1
    m1b <-     inuit.list[[2]][1] + inuit.list[[2]][2]*yr[i] + inuit.list[[2]][3]*yr[i]^2
    
    m2a <-     inuit.list[[3]][1] + inuit.list[[3]][2]*yr[i] + inuit.list[[3]][3]*yr[i]^2
    m2b <- exp(inuit.list[[4]][1] + inuit.list[[4]][2]*yr[i])
    
    m3a <- exp(inuit.list[[5]][1] + inuit.list[[5]][2]*yr[i])
    m3b <-     inuit.list[[6]][1] + inuit.list[[6]][2]*yr[i]
    
    m4a <-     inuit.list[[7]][1] + inuit.list[[7]][2]*yr[i] + inuit.list[[7]][3]*yr[i]^2
    m4b <-     inuit.list[[8]][1] + inuit.list[[8]][2]*yr[i] + inuit.list[[8]][3]*yr[i]^2
    
    rate <- ifelse(age<7.5, exp(m1a + m1b*age),
                   ifelse(age>=7.5 & age<37.5, m2a + m2b*age,
                          ifelse(age>=37.5 & age<80, m3a*exp(m3b*age), m4a + m4b*log(age))))
    
    rate <- rate/5
    
    return(rate)
    
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
value <- c(filter(inuitmort,AgeGrpStart==0)$ex,unlist(le))
label <- rep(c("UN model","Own model"),each=length(yr))
comp <- data.frame(year=yr,label=label,value=value)
ggplot(comp,aes(year,value,group=label,linetype=label))+
  geom_line()+
  theme_bw()

#Run simulation: Indian population##############################################
le <- list()

for (i in 1:17){
  
  #year in function set to remain the same throughout a cohort's life
  mort.fct <- function(age,calTime,duration){
    
    m1a <- log(indian.list[[1]][1] + indian.list[[1]][2]*yr[i])*-1
    m1b <-     indian.list[[2]][1] + indian.list[[2]][2]*yr[i] + indian.list[[2]][3]*yr[i]^2
    
    m2a <-     indian.list[[3]][1] + indian.list[[3]][2]*yr[i] + indian.list[[3]][3]*yr[i]^2
    m2b <- exp(indian.list[[4]][1] + indian.list[[4]][2]*yr[i])
    
    m3a <- exp(indian.list[[5]][1] + indian.list[[5]][2]*yr[i])
    m3b <-     indian.list[[6]][1] + indian.list[[6]][2]*yr[i]
    
    m4a <-     indian.list[[7]][1] + indian.list[[7]][2]*yr[i] + indian.list[[7]][3]*yr[i]^2
    m4b <-     indian.list[[8]][1] + indian.list[[8]][2]*yr[i] + indian.list[[8]][3]*yr[i]^2
    
    rate <- ifelse(age<7.5, exp(m1a + m1b*age),
                   ifelse(age>=7.5 & age<37.5, m2a + m2b*age,
                          ifelse(age>=37.5 & age<80, m3a*exp(m3b*age), m4a + m4b*log(age))))
    
    rate <- rate/5
    
    return(rate)
    
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

#comparison#####################################################################
year <- unique(indianmort$MidPeriod)
value <- c(filter(indianmort,AgeGrpStart==0)$ex,unlist(le))
label <- rep(c("UN model","Own model"),each=length(year))

comp <- data.frame(year=year,label=label,value=value)

ggplot(comp,aes(year,value,group=label,linetype=label))+
  geom_line()+
  theme_bw()

#END OF DO FILE#################################################################