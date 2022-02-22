################################################################################
#IN THIS DO FILE: SUMMARY DATA ON LANGUAGUES. WE WILL SUBMIT THIS DATA TO EXPERTS
#TO CHECK WHETHER WE HAVE RELIABLE INPUT TO OUR MODELS 

################################################################################
#1.PACKAGES 
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(openxlsx)

#load data
df <- readRDS("rawpopulations")

################################################################################
#Preparation
################################################################################
df <- bind_rows(
  #data frame without attributed speakers
  df %>% group_by(language,year,age) %>% filter(origin!="attributed") %>% select(-origin) %>% mutate(set="partial")
  ,
  #data frame with attributed and original speakers as total
  df %>% group_by(language,year,age) %>% summarise(speaker=sum(speaker)) %>% mutate(set="complete")
  )

#attribute number to each unique combination of language, year and set
df <- df %>% group_by(language,year,set) %>% mutate(id=cur_group_id()) %>% arrange(language,year,set) %>% ungroup()

#find total of unique combinations
maxid <- max(df$id)

###############################################################################
#Intergenerational transmission
###############################################################################
#estimate loess model
df$smooth <- unlist(lapply(1:maxid, function(x)  predict(loess(speaker ~ age, span=0.5, data=filter(df,id==x)))))

#replace negative values with zero
df$smooth <- ifelse(df$smooth<0,0,df$smooth)

#function (the formula comes from Hauer & Schmertmann 2020 Demography)
xITR.fct <- function(x){
  
  #number of women ages 15-49
  W <- sum(filter(df,id==x,age>=15,age<50)$smooth)/2 
  
  #proportion of women ages 25-34 to those ages 15-49 (denoted pi)
  pi <- sum(filter(df,id==x,age>=25,age<35)$smooth)/2 / W 
  
  #number of children ages 0-4
  C <- filter(df,id==x,age==0)$smooth
  
  #xITR formula
  xITR <- ifelse(W==0,0,(10.65 - 12.55 * pi) * (C/W))
  
  return(xITR)
  
}  

#execute function and put result in data frame
xITR <- data.frame(id=1:maxid,
                   xITR=round(unlist(lapply(1:maxid, function(x) xITR.fct(x))),2))

#obtain percentage
xITR$xITR <- round(xITR$xITR/2.7*100/10)*10

#top up at 100%
xITR$xITR <- ifelse(xITR$xITR>100,100,xITR$xITR)

#add language name, year and set
xITR <- left_join(xITR,unique(select(df,-age,-smooth,-speaker)))

################################################################################
#Total speakers
################################################################################
#total speakers
total <- df %>% group_by(language,year,set) %>% summarise(total=round(sum(speaker)))

#speaker number trends: pivot wider
total <- total %>% pivot_wider(names_from=year, values_from=total) 

#speaker number trends: assess difference
total <- mutate(total, trend=case_when(
  `2016`/`2011` >= 2 | `2016`/`2011` < .5 ~ "Problem",
  `2016`/`2011` < 2 & `2016`/`2011` >= 1.2 ~ "Increasing",
  `2016`/`2011` < 1.2 & `2016`/`2011` >= 0.83 ~ "Stable",
  `2016`/`2011` < 0.83 & `2016`/`2011` >= 0.5 ~ "Decreasing"
))

total <- total %>% pivot_longer(c(`2011`,`2016`),names_to="year",values_to="total")

total$year <- as.numeric(total$year)

################################################################################
#broad age groups
################################################################################
#make groups of three years
df <- df %>% mutate(agegroup=case_when(
  age<15 ~ "ages0_14",
  age>=15 & age <45 ~ "ages15_44",
  age>=45 ~ "ages45plus"
))

agegroups <- df %>% group_by(language,year,set,agegroup) %>% summarise(speaker=round(sum(speaker))) %>% 
  pivot_wider(names_from=agegroup,values_from=speaker)

#add totals 
agegroups <- left_join(agegroups,total)

#unique variable
agegroups$agegroups <- paste(round(agegroups$ages0_14/agegroups$total*100),"-",
                             round(agegroups$ages15_44/agegroups$total*100),"-",
                             100 - round(agegroups$ages15_44/agegroups$total*100) - round(agegroups$ages0_14/agegroups$total*100),
                             sep="")

################################################################################
#individual assessments (I don't use the age groups in the end because estimates are very sensitive to the different numbers by year and sets )
################################################################################
#put together
assessment <- select(left_join(xITR,agegroups),language,year,set,xITR,trend,total,agegroups)

#remove the year 2011 for the cree languages
assessment <- filter(assessment,language!="Plains Cree"|year!=2011,language!="Swampy Cree"|year!=2011,language!="Woods Cree"|year!=2011)

#function
assessment.fct <- function(x){
  
  lang <- unique(df$language)[x]
  
  a <- filter(assessment,language==lang)
  
  itr <- ifelse(length(unique(a$xITR))==1,
                paste(unique(a$xITR),"%"),
                paste(min(a$xITR),"/",max(a$xITR),"%"))
  
  trend <- ifelse(anyNA(unique(a$trend))==T,NA,
                  ifelse(length(unique(a$trend))==1,unique(a$trend),
                         paste(unique(a$trend)[1],"/",unique(a$trend)[2])))

  total <- ifelse(min(a$total)/max(a$total)<0.5,
                  paste(min(a$total),"/",max(a$total)),
                  as.character(round(mean(a$total))))
                    
  return(data.frame(language=lang,int.trans=itr,trend=trend,total=total))
  
  }

#run function
assessment.results <- bind_rows(lapply(1:length(unique(df$language)), function(x) assessment.fct(x)))

#save
write.xlsx(assessment.results,"assessments.xlsx")
