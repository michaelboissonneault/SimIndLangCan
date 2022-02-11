#population aggregated
population <- filter(sp,language=="Heiltsuk"|language=="Michif"|language=="Algonquin")

#population size
N <- sum(round(population$speaker))

#vectors with 1 position by person in population
language <- unlist(lapply(1:length(population$language), function(x) rep(population$language[x],round(population$speaker[x]))))
age <- unlist(lapply(1:length(population$language), function(x) rep(population$age[x],round(population$speaker[x]))))

#data frame with 1 row per person (population individual)
population <- data.frame(id=1:N,language=language,age=age)

xITR.fct <- function(lang){
  
  #take 20% sample. 
  s <- population[sample(nrow(population),.2*N), ]
  
  #number of women ages 15-49
  W <- length(filter(s,language==lang,age>=15,age<50)$id)/2 
  
  #proportion of women ages 25-34 to those ages 15-49 (denoted pi)
  pi <- length(filter(s,language==lang,age>=25,age<35)$id)/2 / W 
  
  #number of children ages 0-4
  C <- length(filter(s,language==lang,age<5)$id)
  
  #xITR
  xITR <- (10.65 - 12.55 * pi) * (C/W)
  
  return(xITR)
  
  }  

xITRs <- unlist(lapply(1:100, function(x) xITR.fct("Algonquin")))

xITRs <- data.frame(run=1:100,xITR=xITRs)

ggplot(xITRs)+
  geom_histogram(aes(xITR))

# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~.)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

mtcars

# Bootstrap 95% CI for regression coefficients
library(boot)
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}

# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)

mean(mtcars$mpg)

# view results
results

35 + 3*-3.35 + 250*-.0177

plot(results, index=1) # intercept
plot(results, index=2) # wt
plot(results, index=3) # disp

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp

#Heiltsuk
heiltsukboot <- boot(data=population,statistic=bs,R=1000,formula=age~language)

heiltsukboot

plot(heiltsukboot, index=1) # intercept
plot(heiltsukboot, index=2) # wt

ggplot(sp)+
  geom_col(aes(age,speaker))+
  facet_wrap(~language,scales="free_y")
