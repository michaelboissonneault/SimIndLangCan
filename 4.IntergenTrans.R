################################################################################
#IN THIS DO FILE: CALCULATION OF INTERGENERATIONAL TRANSMISSION.
#IT IS FOUND BY DIVIDING THE NUMBER OF CHILDREN AGES 0-4 IN THE DATA 
#BY THE NUMBER THAT WOULD BE EXPECTED IF TRANSMISSION WAS FULL. 
#THIS RATIO IS MULTIPLIED BY THE FERTILITY RATES IN THE SIMULATIONS.
#NOTE: THIS APPROACH MIGHT NOT BE OPTIMAL AND SHOULD PROBABLY BE IMPROVED. 

#CONTENT:
#1. PACKAGES, DIRECTORY, DATA
#2. DATA ON SPEAKER NUMBERS
#3. SAVE
################################################################################
#1. PACKAGES, DIRECTORY, DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(lubridate)
library(openxlsx)

#Set directory
setwd("C:/Users/micha/Documents/Git-RStudio/SimIndLangCan")

#load starting populations
sp <- readRDS("startingpopulations")

#load fertility parameters & function
fert.parameters <- readRDS("fertilityparameters")
fert.fct <- readRDS("fertilityfunction")

################################################################################
#2. CALCULATION OF INTERGENERATIONAL TRANSMISSION
################################################################################
#languages
languages <- unique(sp$language)

#number of languages
k <- length(languages)

#age
age <- unique(sp$age)

#ratio of children 0-4 to number if full transmission
int <- unlist(lapply(1:k,function(x) filter(sp,language==languages[x])$speaker[1] / 
                       sum(filter(sp,language==languages[x])$speaker[4:10] * fert.fct(unique(sp$age)[4:10],2016)*5)))

#put with rest of data
#sp$int <- rep(int,each=length(age))

#intergenerational transmission set to zero for Sekani and So. Tutchone (based on ELP and pyramids above)
int <- ifelse(languages=="Sekani" | languages=="Southern Tutchone",0,int)

#data frame with speaker number
int.nb <- sp %>% group_by(language) %>% summarise(speaker=sum(speaker))
int.nb$int <- int

#add log of speaker number
int.nb$logspeaker <- log10(int.nb$speaker)

#examine relation between log of speakers and int. trans.
summary(lm(int ~ logspeaker, data=int.nb))

#visualize relation
ggplot(int.nb, aes(int,logspeaker)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Log of number of speakers")+
  xlab("Intergenerational transmission")+
  annotate("text",x=.9,y=2.1,label="R-squared=0.29")

#estimates for languages with fewer than 100 speakers are probably less reliable
lt100speakers <- unique(filter(int.nb,logspeaker<2))[['language']]

#calculate intergenerational transmission based on group average to improve robustness
lb100 <- sp %>% group_by(age) %>%
  filter(language %in% lt100speakers) %>%
  summarise(speaker=sum(speaker))

int.nb$int <- ifelse(int.nb$language=="Kutenai"|int.nb$language=="Sekani"|int.nb$language=="Southern Tutchone"|int.nb$language=="Squamish"|int.nb$language=="Tlingit",
                 lb100$speaker[1] / sum(lb100$speaker[4:10] * fert.fct(age[4:10],2016)*5),int.nb$int)

#simulation assumes that intergenerational transmission depends on population size and specific language
#find for each language the slope that connects the origin (0,0)
#and the intersection between nb of speakers ages 25-29 * intergen. trans.
#(change in speaker number on log scale)
int.nb$slope <- unlist(lapply(1:length(languages),function(x)
  summary(lm(c(int[x],0) ~ c(log10(filter(sp,age==25,language==languages[[x]])$speaker),0)))[[4]][[2]]))

#####################################################################
#3. SAVE EVERYTHING
#####################################################################
saveRDS(int.nb,"inttrans")
