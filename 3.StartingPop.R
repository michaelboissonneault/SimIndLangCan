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
dataname <- c(10926024853,20210083911,10926024928,10926025012,10926025032,10926025056,10926025114,10926025131)

#download by age group and put into vector (one column for all info, including language names)
ms16 <- unlist(lapply(1:8,function(x) read.csv(paste("112132202",dataname[x],".CSV",sep=""))[-c(1:34,367:380),]))

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

#function to attribute speakers to 5 year age categories 
fvyr.fct <- function(x,y,z){
  
  #select information in data frame based on age, language and year
  df <- ms %>% filter(agelo==x,year==y,language==z)
  
  #calculate width of age category
  w <- (df$agehi-df$agelo+1)/5
  
  #make data frame with 5-year age categories
  df <- data.frame(language=rep(z,w),
                   speaker=rep(sum(df$speaker)/w,w),
                   age=c(seq(df$agelo,df$agehi,5)),
                   year=y)
  } 

#Year 2016: age and language vectors to scroll through
agev <- unique(filter(ms,year==2016)$agelo)
langv <- unique(filter(ms,year==2016)$language)

#Apply function to data from year 2016
fvyr.list.2016 <- lapply(agev, function(x)
  lapply(langv, function(z) fvyr.fct(x,2016,z))) 

#Year 2011: age and language vectors to scroll through
agev <- unique(filter(ms,year==2011)$agelo)
langv <- unique(filter(ms,year==2011)$language)

#Apply function to data from year 2011
fvyr.list.2011 <- lapply(agev, function(x)
  lapply(langv, function(z) fvyr.fct(x,2011,z))) 

#merge both years
fvyr <- bind_rows(
  bind_rows(fvyr.list.2016),
  bind_rows(fvyr.list.2011)) 

#athapaskan = athabaskan
fvyr$language <- ifelse(fvyr$language=="Athapaskan languages, n.i.e.","Athabaskan languages, n.i.e.",fvyr$language)

#identify broad languages categories in both years
broad <- unique(
  c(sort(unique(filter(fvyr,str_detect(language,", n.o.s.")|str_detect(language,", n.i.e."),year==2016)$language)),
    sort(unique(filter(fvyr,str_detect(language,", n.o.s.")|str_detect(language,", n.i.e."),year==2011)$language))))

#remove the n.i.e. and n.o.s.
broad <- str_remove(broad,", n.i.e.")
broad <- str_remove(broad,", n.o.s.")

#remove duplicates
broad <- unique(broad)

#find total of speaker by age for each language category
broadspeaker <- 
  bind_rows(
  lapply(1:length(broad),function(x) 
  fvyr %>% 
  group_by(age) %>% 
  filter(str_detect(language,paste(broad[x],", n.o.s.",sep=""))|str_detect(language,paste(broad[x],", n.i.e.",sep=""))) %>% 
  summarise(speaker=sum(speaker),name=str_remove(broad[x]," languages"))))

#remove broad categories from main dataset
fvyr <- filter(fvyr,!str_detect(language,"languages"),!str_detect(language,"n.o.s."),!str_detect(language,"n.i.e."))

#five languages are in the 2016 dataset but not in the 2011 one
in16not11 <- setdiff(filter(fvyr,year==2016)$language,filter(fvyr,year==2011)$language)

#Set those five languages apart
fvyr_apart <- fvyr %>% 
  filter(
  language==in16not11[1] | language==in16not11[2] |language==in16not11[3] |language==in16not11[4] |language==in16not11[5] 
)

#make data set with both years
fvyr <- fvyr %>% 
  filter(language!=in16not11[1],language!=in16not11[2],language!=in16not11[3],language!=in16not11[4],language!=in16not11[5] ) %>%
  group_by(language,age) %>% summarise(speaker=sum(speaker)/2)

#merge back with those set apart
fvyr <- bind_rows(fvyr,select(fvyr_apart,-year))

#add information about language family 
fvyr <- mutate(fvyr, family=case_when(
  
  language=="Blackfoot" | language=="Atikamekw" | language=="Montagnais" |
    language=="Moose Cree" | language=="Naskapi" | language=="Northern East Cree" |
    language=="Plains Cree" | language=="Southern East Cree" | language=="Swampy Cree" |
    language=="Woods Cree" | language=="Malecite"| language=="Mi'kmaq" |
    language=="Algonquin" | language=="Ojibway" | language=="Oji-Cree" | language=="Ottawa" ~ "Algonquian",
  
  language=="Babine"|language=="Beaver"|language=="Carrier"|language=="Chilcotin"|
    language=="Dene"|language=="Dogrib"|language=="Gwich'in"|language=="Sarsi"|
    language=="Sekani"|language=="North Slavey"|language=="South Slavey"| language=="Kaska" | 
    language=="Tahltan" | language=="Northern Tutchone" | language=="Southern Tutchone" ~ "Athabaskan",
  
  language=="Inuinnaqtun" | language=="Inuktitut" | language=="Inuvialuktun" ~ "Inuit",
  
  language=="Cayuga" | language=="Mohawk" | language=="Oneida" ~ "Iroquoian",
  
  language=="Comox" | language=="Halkomelem" | language=="Lillooet" | language=="Okanagan" |
    language=="Shuswap" | language=="Squamish" | language=="Straits" | language=="Thompson" ~ "Salish",
  
  language=="Dakota" | language=="Stoney" ~ "Siouan",
  
  language=="Gitxsan" | language=="Nisga'a" | language=="Tsimshian" ~ "Tsimshian",
  
  language=="Haisla" | language=="Heiltsuk" | language=="Kwakiutl" | language=="Nuu-chah-nulth" ~ "Wakashan"
  
))

#information about subfamily
fvyr <- mutate(fvyr,subfamily=case_when(
  
  language=="Atikamekw" | language=="Montagnais" | language=="Moose Cree" | language=="Naskapi" | language=="Northern East Cree" | 
    language=="Plains Cree" | language=="Southern East Cree" | language=="Swampy Cree" | language=="Woods Cree" ~ "Cree",
  
  language=="North Slavey" | language=="South Slavey" ~ "Slavey",
  
  language=="Northern Tutchone"|language=="Southern Tutchone" ~ "Tutchone",
  
  ))

#Total of speakers in all aboriginal languages, by age
fvyr <- left_join(fvyr, fvyr %>% group_by(age) %>% summarise(speaker_aboriginal=sum(speaker)))

#Total of speakers in the family, by age
fvyr <- left_join(fvyr,
                     fvyr %>% group_by(age,family) %>% summarise(speaker_family=sum(speaker)),
                     by=c("age","family"))

#Total of speakers in the subfamily, by age
fvyr <- left_join(fvyr,
                     fvyr %>% group_by(age,subfamily) %>% summarise(speaker_subfamily=sum(speaker)),
                     by=c("age","subfamily"))

#Proportion to total in all aboriginal languages
fvyr$proportion_aboriginal <- fvyr$speaker / fvyr$speaker_aboriginal

#Proportion to total in family
fvyr$proportion_family <- fvyr$speaker / fvyr$speaker_family

#Proportion to total in subfamily
fvyr$proportion_subfamily <- fvyr$speaker / fvyr$speaker_subfamily

#attach non-attributed speakers, all aboriginal languages
fvyr <- left_join(fvyr,
                  broadspeaker %>% filter(name=="Aboriginal") %>% rename("nonatt_aboriginal"=speaker) %>% select(-name),
                  by="age")

#attach non-attributed speakers by family
fvyr <- left_join(fvyr,
                  broadspeaker %>% rename("nonatt_family"=speaker),
                  by=c("age","family"="name"))

#attach non-attributed speakers by subfamily
fvyr <- left_join(fvyr,
                  broadspeaker %>% rename("nonatt_subfamily"=speaker),
                  by=c("age","subfamily"="name"))

#drop totals
fvyr <- select(fvyr,-starts_with("speaker_"))

#total of attributed speakers
fvyr <- fvyr %>% rowwise() %>% 
  mutate(total_attributed = sum(proportion_aboriginal*nonatt_aboriginal,proportion_family*nonatt_family,proportion_subfamily*nonatt_subfamily, na.rm=T)) 

#total attributed + original speaker number
fvyr$speaker <- fvyr$speaker + fvyr$total_attributed

#drop unnecessary columns
fvyr <- fvyr %>% select(language,speaker,age) 

#add zero speaker numbers ages 85, 90, 95
fvyr <- bind_rows(fvyr,data.frame(language=rep(unique(fvyr$language),each=3),speaker=0,age=c(85,90,95)))

#arrange by language
fvyr <- fvyr %>% arrange(language)

################################################################################
#3. INTERPOLATION & SMOOTHING OF AGE STRUCTURE
################################################################################
#loess model
fvyr$speaker_smooth <- unlist(lapply(unique(fvyr$language), function(x) 
  predict(loess(speaker ~ age, span=0.5, data=filter(fvyr,language==x)))))

#replace negative values with zero
fvyr$speaker_smooth <- ifelse(fvyr$speaker_smooth<0,0,fvyr$speaker_smooth)

#check fit
ggplot(fvyr[1:(20*20),])+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker))+
  facet_wrap(.~language,scales="free")+
  coord_flip()

ggplot(fvyr[(20*20+1):(20*20*2),])+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker))+
  facet_wrap(.~language,scales="free")+
  coord_flip()

ggplot(fvyr[(20*20*2+1):(20*20*4),])+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker))+
  facet_wrap(.~language,scales="free")+
  coord_flip()

fvyr$speaker_smooth <- ifelse(fvyr$language==lag(fvyr$language) & fvyr$speaker_smooth>0 & lag(fvyr$speaker_smooth)==0 & fvyr$age!=0,0,fvyr$speaker_smooth)

#############################################################################
#INTERGENERATIONAL TRANSMISSION RATE
#############################################################################
#function
xITR.fct <- function(z){
  
  #number of women ages 15-49
  W <- sum(filter(fvyr,language==z,age>=15,age<50)$speaker_smooth)/2 

  #proportion of women ages 25-34 to those ages 15-49 (denoted pi)
  pi <- sum(filter(fvyr,language==z,age>=25,age<35)$speaker_smooth)/2 / W 
  
  #number of children ages 0-4
  C <- filter(fvyr,language==z,age==0)$speaker_smooth
    
  #xITR
  xITR <- ifelse(W==0,0,(10.65 - 12.55 * pi) * (C/W))
  
  return(xITR)
  
  }  

#execute function and put result in data frame
xITR <- data.frame(language=unique(fvyr$language),
                   xITR=unlist(lapply(unique(fvyr$language), function(z) xITR.fct(z))))

ggplot(xITR,aes(xITR))+
  geom_histogram()

#############################################################################
#4.SAVE
#############################################################################
saveRDS(fvyr,"startingpopulations")
saveRDS(xITR,"xitr")

#END OF DO FILE#################################################################