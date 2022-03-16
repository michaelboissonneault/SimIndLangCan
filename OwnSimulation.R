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

#year birth in starting population
sp$birthgroup <- 2011-sp$age

#intergenerational transmission
sp <- left_join(sp,select(int.nb,language,slope))
sp$it <- sp$slope*sp$speaker

################################################################################
#2.DEFINE FERTILITY AND MORTALITY FUNCTIONS
################################################################################
#fertility######################################################################
fert.fct <- function(age,calTime){
  
  c <-  fert.pm[[1]] + fert.pm[[5]]*(calTime-2016)
  m <-  fert.pm[[2]] + fert.pm[[6]]*(calTime-2016)
  s1 <- fert.pm[[3]] + fert.pm[[7]]*(calTime-2016)
  s2 <- fert.pm[[4]] + fert.pm[[8]]*(calTime-2016)
  
  rate <- c * exp(-1*((age - m) / (ifelse(age<=m, s1, s2)*age))^2) 
  
  rate[age>=50] <- 0
  
  return(rate)
  
}

#Inuit mortality################################################################
mort.fct <- function(age,calTime){
  
  A <- exp(15.3363 + -0.01218625*calTime)
  B <- 0.02152155 + 0.000044244*calTime
  C <- exp(20.998 + -.01265521*calTime)
  
  return((A*exp(B*age) / (1 + A*exp(B*age)) + C) / 5)
  
}

################################################################################
#3.PREPARATION
################################################################################
prep.fct <- function(x,z){
  language <- languages[z]
  birthyr <- floor(runif(round(filter(sp,language==languages[z],birthgroup==x)$speaker),x,x+5))
  return(data.frame(language=rep(language,length(birthyr)),
                    birthyr=birthyr))
  }

df1 <- bind_rows(lapply(seq(1916,2011,5), function(a) 
  lapply(1:length(languages), function(b) prep.fct(a,b))))

################################################################################
#4.SIMULATION
################################################################################
      ##################################################################################
      agebirth <- list()
      agedeath <- list()
      ###################################################################################
      
      yr <- 2016
      
      df <- filter(df1,language==languages[1])
      
      for (i in 1:length(df$language)){

      inityear <- yr 
      currentyear <- yr-1
      birthyear <- df[i,]$birthyr
      it <- filter(sp,language==languages[1],birthgroup>=birthyear,birthgroup<birthyear+5)$it
      
      #repeat until person dies
      repeat{
        currentyear <- currentyear+1
        if(runif(1)<fert.fct(currentyear-birthyear,currentyear)*it){agebirth[[i]] <- currentyear}
        if(runif(1)<mort.fct(currentyear-birthyear,currentyear)){agedeath[[i]] <- currentyear;break}
        }
      }
      
      data.frame(language=languages[1],birthyr=unlist(agebirth))
      df$agedeath <- unlist(agedeath)
      
      
      yr <- 2016
      
      for (i in 125500:125550){
        
        initage <- yr-df1[i,]$birthyr
        it <- df1[i,]$it
        
        #repeat until person dies
        repeat{
          initage <- initage+1
          if(runif(1)<fert.fct(initage,yr+initage)*it){agebirth[[i]] <- initage; lang[[i]] <- df1[i,]$language}
          if(runif(1)<mort.fct(initage,yr+initage)){agedeath[[i]] <- initage;break}
        }
        
      }
      
      data.frame(language=unlist(lang),birthyr=unlist(agebirth))
      
      #put information into data frame
      agebirth <- unlist(agebirth)
      agebirth <- if(length(agebirth)==0){NA}else{agebirth}
      l <- ifelse(length(agebirth)==0,1,length(agebirth))
      dfl[[i]] <- data.frame(id=rep(paste(p[i],i,sep=""),l),
                             language=rep(languages[z],l),
                             yearbirth=rep(p[i],l),
                             agebirth=agebirth,
                             agedeath=rep(agedeath,l))
      df <- bind_rows(dfl)
      
      }#end of for loop
    
    }else{  }#end of condition
  
  sp <- bind_rows(sp,df %>% group_by(birthyr=floor(yearbirth+agebirth)) %>% summarise(language=languages[z],age=0,speaker=n()) %>% filter(!is.na(birthyr)))
  
  sp

    
#execute function
simpop <- bind_rows(lapply(seq(1916,2096,5), function(x) lapply(1:1, function(y) sim.fct(x,y))))

result <- simpop %>% group_by(birth.years=floor(yearbirth+agebirth)) %>% summarise(births=n()) %>% filter(!is.na(birth.years))

birth.years <- c(birth.years,rep(result$birth.years,result$births))






