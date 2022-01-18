################################################################################
#IN THIS DO FILE: TRANSFORMATION OF STATISTICS CANADA'S RAW DATA ON SPEAKERS
#OF INDIGENOUS LANGUAGES INTO DATA THAT WILL ALLOW TO DETERMINE EACH LANGUAGE'S 
#STARTING POPULATION IN THE SIMULATIONS AND ESTIMATE THEIR INTERGENERATIONAL 
#TRANSMISSION RATES (CALCULATIONS OF INTERGEN. TRANS. ITSELF IN OTHER DO FILE)
#CONTENT:
  #1. PACKAGES & DIRECTORY
  #2. DATA ON SPEAKER NUMBERS
    #2.1 YEAR 2016: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?LANG=E&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=0&GID=0&GK=0&GRP=1&PID=112132&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=122&VID=0&VNAMEE=&VNAMEF=
    #2.2 YEAR 2011: https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/tbt-tt/Lp-eng.cfm?LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=0&GC=0&GID=0&GK=0&GRP=1&PID=0&PRID=0&PTYPE=101955&S=0&SHOWALL=0&SUB=0&Temporal=2011&THEME=90&VID=0&VNAMEE=&VNAMEF=
    #2.3 MERGE OF 2016 & 2011
  #3. INTERPOLATION & SMOOTHING OF AGE STRUCTURE 
  #4. SAVE
################################################################################
#1.PACKAGES & DIRECTORY
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(lubridate)
library(openxlsx)

#Set directory
setwd("C:/Users/micha/Documents/Git-RStudio/SimIndLangCan")

#set ggplot2 theme for consistency
theme_set(theme_bw())  

################################################################################
#2. DATA ON SPEAKER NUMBERS
################################################################################
#2.1 Year 2016##################################################################
#vector to scroll through when reading the data
dataname <- c(4853,4908,4928,5012,5032,5056,5114,5131)

#download by age group and put into vector (one column for all info, including language names)
ms16 <- unlist(lapply(1:8,function(x) read.csv(paste("1121322021092602",dataname[x],".CSV",sep=""))[-c(1:34,367:380),]))

#put in data frame
ms16 <- data.frame(label=c("language","total","single","multiple"),speaker=ms16)

#rearrange into two columns: language, single speaker number
ms16language <- filter(ms16,label=="language")
ms16speakers <- filter(ms16,label!="language")
ms16 <- data.frame(language=rep(ms16language$speaker,each=3),label=ms16speakers$label,speaker=ms16speakers$speaker)
ms16 <- ms16 %>% filter(label=="single") %>% select(language,speaker)

#remove trailing spaces at beginning of language names
ms16$language <- trimws(ms16$language)

#take information between parentheses and create new variable for alternate name
ms16$language2 <- stringr::str_extract(string=ms16$language,pattern = "(?<=\\().*(?=\\))")

#remove information between parentheses
ms16$language <- gsub("\\s*\\([^\\)]+\\)","",as.character(ms16$language))

#transform speaker numbers to numeric variable
ms16$speaker <- as.numeric(ms16$speaker)

#add information on age
ms16$agelo <- rep(c(0,seq(15,75,10)),each=length(unique(ms16$language)))
ms16$agehi <- rep(c(14,seq(24,84,10)),each=length(unique(ms16$language)))

#add year
ms16$year <- 2016

#2.1 Year 2011######################################################################
#vectors to scroll through when reading the data
dataname <- c(618,734,753,812,834,855)

#download by age group and merge
ms11 <- bind_rows(lapply(1:6,function(x) read.csv(paste("10325120210909034",dataname[x],".CSV",sep=""),header=F)[9:73,c(1,3)]))

#rename variables
ms11 <- rename(ms11,language=V1)
ms11 <- rename(ms11,speaker=V3)

#take information between parentheses and create new variable for alternate name
ms11$language2 <- stringr::str_extract(string=ms11$language,pattern = "(?<=\\().*(?=\\))")

#remove information between parentheses
ms11$language <- gsub("\\s*\\([^\\)]+\\)","",as.character(ms11$language))

#Innu Montagnais special case
ms11$language <- ifelse(ms11$language=="Innu/Montagnais","Montagnais",ms11$language)
ms11$language2 <- ifelse(ms11$language=="Montagnais","Innu",ms11$language2)

#transform speaker numbers to numeric variable
ms11$speaker <- as.numeric(ms11$speaker)

#add age variables
ms11$agelo <- rep(c(0,15,25,45,65,75),each=length(unique(ms11$language)))
ms11$agehi <- rep(c(14,24,44,64,74,84),each=length(unique(ms11$language)))

#add year
ms11$year <- 2011

#Compare information in 2011 and 2016
setdiff(unique(ms16$language),unique(ms11$language))
setdiff(unique(ms11$language),unique(ms16$language))

#We take the names from 2016
ms11 <- mutate(ms11,
               language=replace(language,language=="Wetsuweten","Babine"),
               language=replace(language,language=="Tlicho","Dogrib"),
               language=replace(language,language=="Gitksan","Gitxsan"),
               language=replace(language,language=="Nootka","Nuu-chah-nulth"),
               language=replace(language,language=="Sarcee","Sarsi"))

#merge two datasets
ms <- bind_rows(ms11,ms16)

#2.3 Merging 2016 & 2011 ################################################################
#identify the values with n.o.s. or n.i.e., or groups
ms$nos <- ifelse(str_detect(ms$language,"n.o.s.")==TRUE,1,0)
ms$nie <- ifelse(str_detect(ms$language,"n.i.e.")==TRUE,1,0)
ms$group <- ifelse(str_detect(ms$language,"languages")==TRUE & ms$nos==0 & ms$nie==0,1,0)

#identify lines referring to groups of languages (year 2016)
ms$total <- ifelse(str_detect(ms$language,"languages")==TRUE & ms$nie==0,1,0)

#add information about language family (to be used to ventilate non-attributed speakers)
ms <- mutate(ms,family=case_when(
  language=="Blackfoot" | language=="Atikamekw" | language=="Montagnais" |
    language=="Moose Cree" |language=="Naskapi" |language=="Northern East Cree" |
    language=="Plains Cree" | language=="Southern East Cree" | language=="Swampy Cree" |
    language=="Woods Cree" | language=="Malecite"|language=="Mi'kmaq"|
    language=="Algonquin" | language=="Ojibway" | language=="Oji-Cree" |
    language=="Ottawa" |language=="Algonquian languages, n.i.e." ~ "Algonquian",
  
  language=="Babine"|language=="Beaver"|language=="Carrier"|language=="Chilcotin"|
    language=="Dene"|language=="Dogrib"|language=="Gwich'in"|language=="Sarsi"|
    language=="Sekani"|language=="North Slavey"|language=="South Slavey"|
    language=="Kaska" | language=="Tahltan" | language=="Northern Tutchone" |
    language=="Southern Tutchone"|language=="Athabaskan languages, n.i.e." |
    language=="Athapaskan languages, n.i.e." ~ "Athabaskan",
  
  language=="Inuinnaqtun" | language=="Inuktitut" | language=="Inuvialuktun" |
    language=="Inuit languages, n.i.e."~ "Inuit",
  
  language=="Cayuga" | language=="Mohawk" | language=="Oneida" |
    language=="Iroquoian languages, n.i.e." ~ "Iroquoian",
  
  language=="Comox" | language=="Halkomelem" | language=="Lillooet" | language=="Okanagan" |
    language=="Shuswap" | language=="Squamish" | language=="Straits" |
    language=="Thompson" | language=="Salish languages, n.i.e." ~ "Salish",
  
  language=="Dakota" | language=="Stoney" | language=="Siouan languages, n.i.e." ~ "Siouan",
  
  language=="Gitxsan" | language=="Nisga'a" | language=="Tsimshian" |
    language=="Tsimsian languages, n.i.e." ~ "Tsimshian",
  
  language=="Haisla" | language=="Heiltsuk" | language=="Kwakiutl" |
    language=="Nuu-chah-nulth" | language=="Wakashan languages, n.i.e." ~ "Wakashan"
  
))

ms <- mutate(ms,subfamily=case_when(
  language=="Northern East Cree" | language=="Plains Cree" | language=="Southern East Cree" |
    language=="Swampy Cree" |language=="Woods Cree" |language=="Cree, n.o.s."|language=="Cree, n.i.e." ~ "Cree",
  
  language=="Northern Tutchone"|language=="Southern Tutchone"|language=="Tutchone, n.o.s." ~ "Tutchone",
  
  language=="North Slavey" | language=="South Slavey" | language=="Slavey, n.o.s." ~ "Slavey"
))

#total speakers by family
totbyfam <- ms %>% group_by(family,agelo,year) %>% filter(nos==0,nie==0) %>% summarise(speakerfam=sum(speaker))

#total speakers not included in a specific language
totnie <- ms %>% filter(nie==1) %>% select(speaker,family,agelo,year)

#remove na's
totnie <- totnie[complete.cases(totnie),]

#change speaker variable name
totnie <- rename(totnie,speakernie=speaker)

#merge to main dataset
ms <- left_join(ms,totbyfam)
ms <- left_join(ms,totnie)

#speaker proportion in family
ms$propinfam <- ms$speaker / ms$speakerfam

#new speaker number with non-assigned speakers
ms$speakernew <- ms$speaker + ms$propinfam * ms$speakernie

#total speakers by subfamily
totbysubfam <- ms %>% group_by(subfamily,agelo,year) %>% filter(nos==0,nie==0) %>% summarise(speakersubfam=sum(speaker))

#total speakers not otherwise specified
totnos <- ms %>% filter(nos==1) %>% select(speaker,subfamily,agelo,year)

#remove na's
totnos <- totnos[complete.cases(totnos),]

#change speaker variable name
totnos <- rename(totnos,speakernos=speaker)

#merge to main dataset
ms <- left_join(ms,totbysubfam)
ms <- left_join(ms,totnos)

#speaker proportion in subfamily
ms$propinsubfam <- ms$speaker / ms$speakersubfam

#new speaker number with non-assigned speakers
ms$speakernew <- ifelse(!is.na(ms$speakernos), ms$speakernew + ms$propinsubfam * ms$speakernos,ms$speakernew)

#round new speaker number
ms$speakernew <- round(ms$speakernew)

#replace na cells
ms$speakernew <- ifelse(is.na(ms$speakernew),ms$speaker,ms$speakernew)

#remove total categories
ms <- ms %>% filter(group==0)

#keep only necessary information
ms <- ms %>% filter(nos==0,nie==0) %>% select(language,speaker,speakernew,agelo,agehi,year,family)

#Select languages that appear in both years
languages <- sort(intersect(unique(filter(ms,year==2011)$language),
                            unique(filter(ms,year==2016)$language)))

#number of different languages
k <- length(languages)

#garbage collection
gc() 

################################################################################
#3. INTERPOLATION & SMOOTHING OF AGE STRUCTURE
################################################################################
#put languages in separate lists according to year
li16 <- lapply(1:k,function(x) ms %>% filter(language==languages[x],year==2016))
li11 <- lapply(1:k,function(x) ms %>% filter(language==languages[x],year==2011))

#create population vectors according to five year age categories
n16 <- lapply(1:k,function(x) c(rep(li16[[x]]$speakernew[1]/3,3),
                                rep(li16[[x]]$speakernew[2]/2,2),
                                rep(li16[[x]]$speakernew[3]/2,2),
                                rep(li16[[x]]$speakernew[4]/2,2),
                                rep(li16[[x]]$speakernew[5]/2,2),
                                rep(li16[[x]]$speakernew[6]/2,2),
                                rep(li16[[x]]$speakernew[7]/2,2),
                                rep(li16[[x]]$speakernew[8]/3,3),rep(0,5)))

n11 <- lapply(1:k,function(x) c(rep(li11[[x]]$speakernew[1]/3,3),
                                rep(li11[[x]]$speakernew[2]/2,2),
                                rep(li11[[x]]$speakernew[3]/4,4),
                                rep(li11[[x]]$speakernew[4]/4,4),
                                rep(li11[[x]]$speakernew[5]/2,2),
                                rep(li11[[x]]$speakernew[6]/3,3),rep(0,5)))


#create age vector
age <- seq(0,110,5)

#estimate loess model
smooth16 <- lapply(1:k, function(x) unlist(predict(loess(n16[[x]] ~ age,span=0.5))))
smooth11 <- lapply(1:k, function(x) unlist(predict(loess(n11[[x]] ~ age,span=0.5))))

#replace negative values with 0
smooth16 <- lapply(1:k, function(x) ifelse(smooth16[[x]]<0,0,smooth16[[x]]))
smooth11 <- lapply(1:k, function(x) ifelse(smooth11[[x]]<0,0,smooth11[[x]]))

#replace smoothed values above 0 when observed values are at 0 and age below 20 or above 95
smooth16 <- lapply(1:k, function(x) ifelse(smooth16[[x]]>0 & n16[[x]]==0 & (age<20 | age>=95),0,smooth16[[x]]))
smooth11 <- lapply(1:k, function(x) ifelse(smooth11[[x]]>0 & n11[[x]]==0 & (age<20 | age>=95),0,smooth11[[x]]))

#put everything in data frames
agedist <- bind_rows(
  data.frame(language=rep(languages,each=length(age)),
             age=age,
             model=unlist(smooth11),
             data=unlist(n11),
             year=2011),
  data.frame(language=rep(languages,each=length(age)),
             age=age,
             model=unlist(smooth16),
             data=unlist(n16),
             year=2016)
)

agedist$lang_nb <- rep(1:length(languages),each=length(age))

#check model against data
ggplot(filter(agedist, lang_nb<14),aes(age, model, group=1)) +
  geom_line() +
  geom_col(aes(age, data, group=2)) +
  facet_grid(year ~ language, scales = "free") +
  coord_flip()

ggplot(filter(agedist, lang_nb>=14, lang_nb<27),aes(age, model, group=1)) +
  geom_line() +
  geom_col(aes(age, data, group=2)) +
  facet_grid(year~language,scales = "free") +
  coord_flip()

ggplot(filter(agedist, lang_nb>=27, lang_nb<41),aes(age, model, group=1)) +
  geom_line() +
  geom_col(aes(age,data,group=2)) +
  facet_grid(year~language,scales = "free") +
  coord_flip()

ggplot(filter(agedist, lang_nb>=41),aes(age, model, group=1)) +
  geom_line() +
  geom_col(aes(age, data, group=2)) +
  facet_grid(year ~ language, scales = "free") +
  coord_flip()

#compare smoothed distributions 2011 and 2016
ggplot(agedist,aes(age, model, group=as.character(year), color=as.character(year))) +
  geom_line() +
  facet_wrap(~language,scales="free") +
  coord_flip()

#based on visual assessment the following languages shouldn't be included (or only after further scrutiny)
languages <- languages[languages %in% c("Cayuga","Oneida") == FALSE]
agedist <- filter(agedist,language!="Cayuga",language!="Oneida")

#starting populations that will enter simulations
sp <- agedist %>% group_by(language, age) %>% summarise(speaker=mean(model))

#############################################################################
#4.SAVE
#############################################################################
saveRDS(sp,"startingpopulations")

#END OF DO FILE#################################################################