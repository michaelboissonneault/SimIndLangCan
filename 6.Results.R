################################################################################
#PACKAGES
################################################################################
library(tidyverse)
library(MicSim)
library(lubridate)
library(openxlsx)
################################################################################
#DIRECTORY, DATA FROM PREVIOUS CALCULATIONS FOR MORTALITY AND FERTILITY
################################################################################
rm(list=ls())
setwd("//ia/NIDI$/home/MichaelBo/Documents/work/LinguisticDiversity/Canada/")

################################################################################
#RESULTS
################################################################################
#load results
finalpop <- bind_rows(lapply(1:5, function(a) lapply(17:19,function(b)
  readRDS(paste("Results/finalpop",languages[b],a,sep=""))
)))

#Data frame with speaker number and number of births                     
results <- finalpop %>% group_by(period,language,run) %>% summarise(alive=sum(alive),births=sum(births))
results$alive <- ifelse(results$alive==0,NA,results$alive)
results <- arrange(results,language)

#Plot
ggplot(results,aes(period,alive,group=run))+
  geom_line()+
  facet_wrap(~language,scales = "free")+
  theme_minimal()

#distribution in 2050
ggplot(filter(results,period==2050),aes(alive))+
  geom_histogram(binwidth=1)+
  facet_wrap(~language,scales="free")

#speakers in 2100 
results %>% group_by(language) %>% filter(period==2100) %>% 
  summarise(mean2100=mean(alive),
            lower= mean(alive)-1.96*sd(alive)/sqrt(max(results$run)),
            upper= mean(alive)+1.96*sd(alive)/sqrt(max(results$run)))

#year of death
deathyear <- results %>% group_by(language,run) %>% filter(is.na(alive)) %>% summarise(deathyear=min(period)) 
table(deathyear$language)

#only babine becomes extinct in each run
deathyear %>% filter(language=="Babine") %>% 
  summarise(meandeath=mean(deathyear),lower=mean(deathyear)-1.96*sd(deathyear)/sqrt(max(results$run)),
            upper=mean(deathyear)+1.96*sd(deathyear)/sqrt(max(results$run)))

#how to estimate uncertainty concerning the other languages? 

################################################################################
#RESULTS BASED ON LEI SCALE
################################################################################
#based on 1 run for now
res1 <- filter(results,run==1)

#speaker number 
res1 <- mutate(res1,number = case_when(
  alive >= 10^5 ~ "0.Safe",
  alive < 10^5 & alive>= 10^4 ~ "1.Vulnerable",
  alive <  10^4 & alive>=10^3 ~ "2.Threatened",
  alive < 10^3 & alive>=10^2 ~ "3.Endangered",
  alive <10^2 & alive > 10 ~ "4.Severely Endangered",
  alive < 10 & alive >=1 ~ "5.Critically Endangered",
  is.na(alive) ~ "6.Dead"
))

#transmission
res1 <- res1 %>% group_by(language) %>%
  mutate(birthslag5 = lag(births,5),
         birthslag25 = lag(births,25),
         birthslag50 = lag(births,50),
         birthslag75 = lag(births,75))

res1$birthrate <- res1$birthslag5/res1$alive

res1 <- mutate(res1,transmission = case_when(
  birthrate>0.05 ~ "0.Safe",
  birthrate<0.05 & birthslag5>0 ~ "1.Vulnerable",
  birthslag5==0 & birthslag25>0 ~ "2.Threatened",
  birthslag25==0 & birthslag50>0 ~ "3.Endangered",
  birthslag50==0 & birthslag75>0 ~ "4.Severely Endangered",
  birthslag75==0 & alive>=1 ~ "5.Critically Endangered",
  is.na(alive) ~ "6.Dead"
))


#lagged speaker numbers
res1 <- res1 %>% group_by(language) %>% mutate(alivelag = lag(alive,20))
res1$trendvar <- res1$alive / res1$alivelag

#trend
res1 <- mutate(res1,trend = case_when(
  trendvar >=.95 ~ "0.Safe",
  trendvar <.95 & trendvar>=0.8 ~ "1.vulnerable",
  trendvar <0.8 & trendvar >=0.65 ~ "2.Threatened",
  trendvar <0.65 & trendvar >=0.5 ~ "3.Endangered",
  trendvar <0.5 & trendvar >=0.35 ~ "4.Severely Endangered",
  trendvar <0.35 ~ "5.Critically Endangered",
  is.na(alive) ~ "6.Dead"
  
))

#overall
res1 <- mutate(res1,overall1 = case_when(
  number=="0.Safe" ~ 0,
  number=="1.Vulnerable" ~ 1,
  number=="2.Threatened" ~ 2,
  number=="3.Endangered" ~ 3,
  number=="4.Severely Endangered" ~ 4,
  number=="5.Critically Endangered" ~ 5
))

res1 <- mutate(res1,overall2 = case_when(
  transmission=="0.Safe" ~ 0,
  transmission=="1.Vulnerable" ~ 1,
  transmission=="2.Threatened" ~ 2,
  transmission=="3.Endangered" ~ 3,
  transmission=="4.Severely Endangered" ~ 4,
  transmission=="5.Critically Endangered" ~ 5
))

res1 <- mutate(res1,overall3 = case_when(
  trend=="0.Safe" ~ 0,
  trend=="1.Vulnerable" ~ 1,
  trend=="2.Threatened" ~ 2,
  trend=="3.Endangered" ~ 3,
  trend=="4.Severely Endangered" ~ 4,
  trend=="5.Critically Endangered" ~ 5
))

res1 <- left_join(res1,res1 %>% group_by(language,period) %>% summarise(score=(overall1 + overall2*2 + overall3)/20),by=c("language","period"))

res1 <- mutate(res1,overall = case_when(
  score==0 ~ "0.Safe",
  score>0 & score <=.2 ~ "1.Vulnerable",
  score>.2 & score <=.4 ~ "2.Threatened",
  score>.4 & score <=.6 ~"3.Endangered",
  score>.6 & score <=.8 ~"4.Severely Endangered",
  score>.8 ~"5.Critically Endangered",
  is.na(score) ~ "6.Dead"
))

table <- res1 %>% filter(period==2100) %>% select(language,number,transmission,trend,overall)

table(table$overall)
