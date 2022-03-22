################################################################################
#IN THIS DO FILE: TRANSFORMATION OF STATISTICS CANADA'S RAW DATA ON SPEAKERS
#OF INDIGENOUS LANGUAGES INTO DATA THAT WILL ALLOW TO DETERMINE EACH LANGUAGE'S 
#STARTING POPULATION IN THE SIMULATIONS AND ESTIMATE THEIR INTERGENERATIONAL 
#TRANSMISSION RATES 
#CONTENT:
  #1. PACKAGES 
  #2. LOAD AND CLEAN DATA ON SPEAKER NUMBERS 
    #2.1 YEAR 2016: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?LANG=E&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=0&GID=0&GK=0&GRP=1&PID=112132&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=122&VID=0&VNAMEE=&VNAMEF=
    #2.2 YEAR 2011: https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/tbt-tt/Lp-eng.cfm?LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=0&GC=0&GID=0&GK=0&GRP=1&PID=0&PRID=0&PTYPE=101955&S=0&SHOWALL=0&SUB=0&Temporal=2011&THEME=90&VID=0&VNAMEE=&VNAMEF=
  #3. DIVIDE SPEAKER NUMBERS INTO 5-YEAR AGE CATEGORIES
  #4. ATTRIBUTE NON-ATTRIBUTED SPEAKERS
  #5. INTERPOLATION & SMOOTHING OF THE AGE STRUCTURES
  #6. CALCULATION OF INTERGENERATIONAL TRANSMISSION RATES
  #7. SAVE
################################################################################
#1.PACKAGES 
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(lubridate)
library(openxlsx)

#set ggplot2 theme for consistency
theme_set(theme_bw())  

################################################################################
#2. LOAD AND CLEAN DATA ON SPEAKER NUMBERS 
################################################################################
#2.1 Year 2016##################################################################
#vector to scroll through when reading the data
dataname <- c(10926024853,20210083911,10926024928,10926025012,10926025032,10926025056,10926025114,10926025131)

#download by age group and put into vector (one column for all info, including language names)
ms16 <- unlist(lapply(1:8,function(x) read.csv(paste("112132202",dataname[x],".CSV",sep=""))[-c(1:34,367:380),]))

#put in data frame
ms16 <- data.frame(label=c("language","total","single","multiple"),speaker=ms16)

#rearrange into two columns: language, single speaker number
ms16 <- data.frame(language=rep(filter(ms16,label=="language")$speaker,each=3),
                   label=filter(ms16,label!="language")$label,
                   speaker=filter(ms16,label!="language")$speaker) %>% 
  filter(label=="single") %>% 
  select(language,speaker)

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

#2.2 Year 2011######################################################################
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

#Use the 2016 names for the 2011 dataset
ms11 <- mutate(ms11,
               language=replace(language,language=="Wetsuweten","Babine"),
               language=replace(language,language=="Tlicho","Dogrib"),
               language=replace(language,language=="Gitksan","Gitxsan"),
               language=replace(language,language=="Nootka","Nuu-chah-nulth"),
               language=replace(language,language=="Sarcee","Sarsi"))

#########################################################################################
#3. DIVIDE SPEAKER NUMBERS INTO 5-YEAR AGE CATEGORIES
#########################################################################################
#function to attribute speakers to 5 year age categories 
fvyr.fct <- function(x,y,dta){
  
  #select information in data frame based on age, language and year
  df <- dta %>% filter(agelo==x,language==y)
  
  #calculate width of age category
  w <- (df$agehi-df$agelo+1)/5
  
  #make data frame with 5-year age categories
  df <- data.frame(language=rep(y,w),
                   speaker=rep(sum(df$speaker)/w,w),
                   age=c(seq(df$agelo,df$agehi,5)),
                   year=df$year)
  } 

#Apply function to data from year 2016
fvyr.list.2016 <- lapply(unique(ms16$agelo), function(x)
  lapply(unique(ms16$language), function(y) fvyr.fct(x,y,ms16))) 

#Apply function to data from year 2011
fvyr.list.2011 <- lapply(unique(ms11$agelo), function(x)
  lapply(unique(ms11$language), function(y) fvyr.fct(x,y,ms11))) 

#merge both years in single data frame
fvyr <- bind_rows(
  bind_rows(fvyr.list.2016),
  bind_rows(fvyr.list.2011)) 

#athapaskan = athabaskan
fvyr$language <- ifelse(fvyr$language=="Athapaskan languages, n.i.e.","Athabaskan languages, n.i.e.",fvyr$language)

#########################################################################################
#4. ATTRIBUTE NON-ATTRIBUTED SPEAKERS
#########################################################################################
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
  group_by(age,year) %>% 
  filter(str_detect(language,paste(broad[x],", n.o.s.",sep=""))|str_detect(language,paste(broad[x],", n.i.e.",sep=""))) %>% 
  summarise(speaker=sum(speaker),name=str_remove(broad[x]," languages")))) 

#remove broad categories from main dataset
fvyr <- filter(fvyr,!str_detect(language,"languages"),!str_detect(language,"n.o.s."),!str_detect(language,"n.i.e."))

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

#information about subfamily (Atikamekw, Montagnais, Naskapi not included in Cree. Although linguists consider these as Cree languages, speakers who identify themselves as Cree likely refer to those that bear the name "Cree". )
fvyr <- mutate(fvyr,subfamily=case_when(
  
  language=="Moose Cree" | language=="Northern East Cree" | language=="Naskapi" |
  language=="Plains Cree" | language=="Southern East Cree" | language=="Swampy Cree" | language=="Woods Cree" ~ "Cree",
  
  language=="North Slavey" | language=="South Slavey" ~ "Slavey",
  
  language=="Northern Tutchone"|language=="Southern Tutchone" ~ "Tutchone",
  
  ))

#Total of speakers in all aboriginal languages, by age and year
fvyr <- left_join(fvyr, fvyr %>% group_by(age,year) %>% summarise(speaker_aboriginal=sum(speaker)))

#Total of speakers in the family, by age and year
fvyr <- left_join(fvyr, fvyr %>% group_by(age,year,family) %>% summarise(speaker_family=sum(speaker)),by=c("age","year","family"))

#Total of speakers in the subfamily, by age and year
fvyr <- left_join(fvyr, fvyr %>% group_by(age,year,subfamily) %>% summarise(speaker_subfamily=sum(speaker)), by=c("age","year","subfamily"))

#Proportion to total in all aboriginal languages
fvyr$proportion_aboriginal <- fvyr$speaker / fvyr$speaker_aboriginal

#Proportion to total in family
fvyr$proportion_family <- fvyr$speaker / fvyr$speaker_family

#Proportion to total in subfamily
fvyr$proportion_subfamily <- fvyr$speaker / fvyr$speaker_subfamily

#Attach non-attributed speakers, all aboriginal languages
fvyr <- left_join(fvyr,
                  broadspeaker %>% filter(name=="Aboriginal") %>% rename("nonatt_aboriginal"=speaker) %>% select(-name),
                  by=c("age","year"))

#attach non-attributed speakers by family
fvyr <- left_join(fvyr,
                  broadspeaker %>% rename("nonatt_family"=speaker),
                  by=c("age","year","family"="name"))

#attach non-attributed speakers by subfamily
fvyr <- left_join(fvyr,
                  broadspeaker %>% rename("nonatt_subfamily"=speaker),
                  by=c("age","year","subfamily"="name"))

#find total of speakers to be attributed
fvyr <- fvyr %>% rowwise() %>% 
  mutate(attributed=sum(c(proportion_aboriginal*nonatt_aboriginal,proportion_family*nonatt_family,proportion_subfamily*nonatt_subfamily),na.rm=T))

#drop unnecessary columns
fvyr <- fvyr %>% select(language,age,year,speaker,attributed) 

#pivot longer
fvyr <- fvyr %>% pivot_longer(c(speaker,attributed),names_to = "origin", values_to = "speaker") %>% 
  mutate(origin = case_when(origin=="speaker" ~ "in category", origin=="attributed" ~ "attributed"))

#add zero speaker numbers ages 85, 90, 95 (to help smoothing below)
fvyr <- bind_rows(fvyr,
                  data.frame(language=rep(unique(filter(fvyr,year==2011)$language),each=3),
                             age=c(85,90,95),
                             year=2011,
                             origin="in category",
                             speaker=0),
                  data.frame(language=rep(unique(filter(fvyr,year==2016)$language),each=3),
                             age=c(85,90,95),
                             year=2016,
                             origin="in category",
                             speaker=0))
                  
#Northern and Southern East Cree are usually considered the same
fvyr <- bind_rows(fvyr, 
                  (fvyr %>% filter(language=="Northern East Cree"|language=="Southern East Cree") %>% 
                     group_by(age,year,origin) %>% summarise(language="East Cree",speaker=sum(speaker)))) 

fvyr <-  fvyr %>% filter(language!="Northern East Cree",language!="Southern East Cree")

#Analyses for the Cree languages should rest on the 2016 data only because very few people identified themselves as speakers of one of them in 2011
fvyr <- fvyr %>% filter((language!="Moose Cree" | year!=2011),
                            (language!="Naskapi" | year!=2011),
                            (language!="East Cree" | year!=2011),
                            (language!="Woods Cree" | year!=2011),
                            (language!="Swampy Cree" | year!=2011),
                            (language!="Plains Cree" | year!=2011))

#new data frame total attributed + original speaker number, mean of both years
total <- fvyr %>% group_by(language,age,year) %>% summarise(speaker=sum(speaker)) %>%
  group_by(language,age) %>% summarise(speaker=mean(speaker))

#arrange by language
total <- total %>% arrange(language)
fvyr <- fvyr %>% arrange(language)

#save for the assessments
saveRDS(fvyr,"rawpopulations")

################################################################################
#5. INTERPOLATION & SMOOTHING OF THE AGE STRUCTURES
################################################################################
#loess model
total$speaker_smooth <- unlist(lapply(unique(total$language), function(x) 
  predict(loess(speaker ~ age, span=0.5, data=filter(total,language==x)))))

#replace negative values with zero
total$speaker_smooth <- ifelse(total$speaker_smooth<0,0,total$speaker_smooth)

#join smoothed data to original data
fvyr <- left_join(fvyr,select(total,language,age,speaker_smooth))

#attribute numbers to languages
fvyr <- left_join(fvyr,data.frame(language=unique(fvyr$language),nb=1:length(unique(fvyr$language))))

#check fit
ggplot(filter(fvyr,nb<=15))+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker,fill=origin),position="stack")+
  facet_grid(year~language,scales="free")+
  coord_flip()

ggplot(filter(fvyr,nb>15,nb<=30))+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker,fill=origin),position="stack")+
  facet_grid(year~language,scales="free")+
  coord_flip()

ggplot(filter(fvyr,nb>30,nb<=44))+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker,fill=origin),position="stack")+
  facet_grid(year~language,scales="free")+
  coord_flip()

ggplot(filter(fvyr,nb>44,nb<=58))+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker,fill=origin),position="stack")+
  facet_grid(year~language,scales="free")+
  coord_flip()

ggplot(filter(fvyr,language=="Moose Cree" | language=="Northern East Cree" | 
                language=="Naskapi" | language=="Plains Cree" | language=="Southern East Cree" | 
                language=="Swampy Cree" | language=="Woods Cree"))+
  geom_line(aes(age,speaker_smooth))+
  geom_col(aes(age,speaker,fill=origin),position="stack")+
  facet_grid(year~language,scales="free")+
  coord_flip()

#find the absolute error between the smoothed and actual values in relation to a language's speaker number
total$abs.err <- abs(total$speaker - total$speaker_smooth)

#new data frame
lang.summary <- total %>% group_by(language) %>% summarise(speaker=sum(speaker),abs.err=sum(abs.err)) %>% mutate(rel.err=abs.err/speaker)

#display languages with worst/best fits 
arrange(lang.summary,-rel.err)
arrange(lang.summary,rel.err)

#############################################################################
#6. CALCULATION OF INTERGENERATIONAL TRANSMISSION RATES
#############################################################################
#function (the formula comes from Hauer & Schmertmann 2020 Demography)
xITR.fct <- function(z){
  
  #number of women ages 15-49
  W <- sum(filter(fvyr,language==z,age>=15,age<50)$speaker_smooth)/2 

  #proportion of women ages 25-34 to those ages 15-49 (denoted pi)
  pi <- sum(filter(fvyr,language==z,age>=25,age<35)$speaker_smooth)/2 / W 
  
  #number of children ages 0-4
  C <- filter(fvyr,language==z,age==0)$speaker_smooth
    
  #xITR formula
  xITR <- ifelse(W==0,0,(10.65 - 12.55 * pi) * (C/W))
  
  return(xITR)
  
  }  

#execute function and put result in data frame
xITR <- data.frame(language=unique(fvyr$language),
                   xITR=unlist(lapply(unique(fvyr$language), function(z) xITR.fct(z))))

#explore distribution of xITR
ggplot(xITR,aes(xITR))+
  geom_histogram()

#relationship with population size
xITR <- left_join(xITR, total %>% group_by(language) %>% summarise(speaker=sum(speaker_smooth)))

#statistical relationship whole set
summary(lm(xITR ~ log(speaker), data=xITR)) #adjusted R-squared = 0.09

#statistical relationship languages with 150 speakers or more
summary(lm(filter(xITR,log(speaker)>=5)$xITR ~ log(filter(xITR,log(speaker)>=5)$speaker))) #adjusted R-squared = 0.20

#statistical relationship languages with 500 speakers or more
summary(lm(filter(xITR,log(speaker)>=6.22)$xITR ~ log(filter(xITR,log(speaker)>=6.22)$speaker))) #adjusted R-squared = 0.47

#slope of change: y1=xITR, y2=0, x1=speakers ages 30-34, x2=0 
xITR$slope <- unlist(lapply(1:length(xITR$language), function(x) 
  summary(lm(c(xITR$xITR[x],0) ~ c(filter(total,language==xITR$language[x],age==30)$speaker_smooth,0)))[[4]][[2]]))

#############################################################################
#7.SAVE
#############################################################################
saveRDS(total %>% select(language,age,speaker_smooth) %>% rename(speaker=speaker_smooth),"startingpopulations")
saveRDS(xITR,"xitr")

#END OF DO FILE#################################################################