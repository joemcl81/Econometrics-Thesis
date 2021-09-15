getwd()

source("http://www.sthda.com/upload/rquery_cormat.r")
library(lmtest)
install.packages("lme4", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
install.packages("nlme", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
install.packages(packageurl, repos=NULL, type="source")
library(dplyr)
library(ggplot2)
library(plm)
library(car)
library(orcutt)
library(Metrics)
library(MASS)
library(robustbase)
library(vars)
library(kableExtra)
library(tidyverse)
library(tidyr)
library(zoo)
library(ggcorrplot)
library("lmtest")
library("sandwich")

data1 <- read.csv('C:/Users/Joe/Documents/WorkingDataset.csv')
Portfoliodata <- data1 %>% filter(Year > 2006 & Year < 2019)
names(Portfoliodata) <- c("country", "year", "gini", "giniChange", "s80s20", "s80s20Change", "gdp", "gdpGrowth", "eduEXP", "healthEXP", "soprotEXP", "FDIinc", "netTrade", "unemployment", "eduENROLL", "eduATTAIN", "eduATTAINchange")
Portfoliodata <- Portfoliodata %>% filter(country != "Croatia")
Portfoliodata <- na.omit(Portfoliodata)
#-361000000000 - lowest FDIinflow value
Portfoliodata$FDIinc <- Portfoliodata$FDIinc + 500000000000
#-85471111720 - lowest NetTrade value
Portfoliodata$netTrade <- Portfoliodata$netTrade + 100000000000
#Adding constants to remove negative numbers
Portfoliodata$giniChange <- Portfoliodata$giniChange + 1
Portfoliodata$s80s20Change <- Portfoliodata$s80s20Change + 1
Portfoliodata$eduATTAINchange <- Portfoliodata$eduATTAINchange + 1
#Sum of various government expenditure variables
Portfoliodata$govEXP <- Portfoliodata$eduEXP + Portfoliodata$soprotEXP + Portfoliodata$healthEXP

Portfoliodata$gini100 <- Portfoliodata$gini / 100

#Time series Each Variable
ggplot(data = Portfoliodata, aes(x = year, y = gini, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Gini coefficient") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = s80s20, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of S80/S20 ratio") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = gdpGrowth, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of GDP Growth") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = eduEXP, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Education Expenditure") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = healthEXP, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Health Expenditure") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = soprotEXP, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Social Protection Expenditure") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = govEXP, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Government Expenditure on Health, Education and Social Protection") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = FDIinc, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Net FDI inflow") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = netTrade, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Net Trade") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = unemployment, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Unemployment Rate") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

ggplot(data = Portfoliodata, aes(x = year, y = eduATTAIN, color = country)) + geom_line() + geom_point() +
  labs(title = "Time Series of Tertiary Education Attainment") + 
  stat_smooth(color = "#FC4E07", method = "lm", formula = y ~ x) +
  theme(plot.title=element_text(margin=margin(t=40,b=-30)))

#Summary stats of dataset
summary(Portfoliodata %>% select(-country, -year))
#Removed Kosovo, Montenegro, North Macedonia, Serbia and Turkey as they are developing countries
#No Greece because of inadequate data

#Correlation matrix
corIncome <- cor(Portfoliodata %>% select(-country, -year))
write.csv(corIncome, "correlationTABLE.csv")
ggcorrplot(corIncome, hc.order = TRUE, type = "lower", lab = TRUE)

#Modelling Gini coefficient

# ORIGINAL GINI MODEL
Gimodel1 <- lm(gini ~ gdpGrowth + soprotEXP + healthEXP + eduEXP + eduATTAIN + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
summary(Gimodel1)
bptest(Gimodel1)

#Multicollinearity is not present as demonstrated my correlation plot and VIF
car::vif(Gimodel1)

if(bptest(Gimodel1)$statistic > qchisq(.95, df=8)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

#FIXED HETEROSECDASCITY HERE
Gimodel2 <- lm(log(gini) ~ gdpGrowth + govEXP + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
summary(Gimodel2)
bptest(Gimodel2)
plot(Gimodel2)

if(bptest(Gimodel2)$statistic > qchisq(.95, df=5)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

#TESTING FOR AUTOCORRELATION
durbinWatsonTest(Gimodel2)
summary(Gimodel2)

testStat <- 0.3207713
dl <- 1.796
du <- 1.854

if(testStat > 0 && testStat < dl){
  print("Reject the null hypothesis i.e. positive autocorrelation")
} else if(testStat > dl && testStat < du) {
  print("Zone of indecision")
} else if(testStat > 4-dl && testStat < 4) {
  print("Reject the null hypothesis i.e. negative autocorrelation")
} else if(testStat > 4-du && testStat < 4-dl) {
  print("Zone of indecision")
} else{
  print("Do not reject null hypothesis")
}

#Fixing autocorrelation
cochGimodel2 <- cochrane.orcutt(Gimodel2) #Final Model 
summary(cochGimodel2)

#Modelling S80S20 ratio

s80s20model1 <- lm(log(s80s20) ~ gdpGrowth + soprotEXP + healthEXP + eduEXP + eduATTAIN + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
bptest(s80s20model1)
summary(s80s20model1)

#Multicollinearity is not present as demonstrated my correlation plot and VIF
car::vif(s80s20model1)

#Formal Test for Heterscedascity
if(bptest(s80s20model1)$statistic > qchisq(.95, df=8)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

s80s20model2 <- lm(log(s80s20) ~ gdpGrowth + eduATTAINchange + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
summary(s80s20model2)
bptest(s80s20model2)

if(bptest(s80s20model2)$statistic > qchisq(.95, df=5)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

#TESTING FOR AUTOCORRELATION
durbinWatsonTest(s80s20model2)
summary(s80s20model2)

S80S20testStat <- 0.3070873
S80S20dl <- 1.796
S80S20du <- 1.854

if(testStat > 0 && testStat < dl){
  print("Reject the null hypothesis i.e. positive autocorrelation")
} else if(testStat > dl && testStat < du) {
  print("Zone of indecision")
} else if(testStat > 4-dl && testStat < 4) {
  print("Reject the null hypothesis i.e. negative autocorrelation")
} else if(testStat > 4-du && testStat < 4-dl) {
  print("Zone of indecision")
} else{
  print("Do not reject null hypothesis")
}

#Fixing autocorrelation
cochs80s20model2 <- cochrane.orcutt(s80s20model2) #Final Model 
summary(cochs80s20model2)


### EXTRA CODE USED FOR EXPERIMENTATION ###
#NO HETERSCEDASCITY HERE EITHER
Gimodel3 <- lm(gini ~ gdpGrowth + eduEXP + healthEXP + unemployment + log(netTrade), Portfoliodata)
bptest(Gimodel3)
summary(Gimodel3)

if(bptest(Gimodel3)$statistic > qchisq(.95, df=5)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

#Gini Model using Robust Linear Regression
#ROBUSTGimodel2 = lmrob(gini ~ gdpGrowth + soprotEXP + eduEXP + eduATTAIN + unemployment + log(FDIinc) + log(netTrade), Portfoliodata, control = lmrob.control(max.it = 100))
#summary(ROBUSTGimodel2)
#plot(ROBUSTGimodel2)

#Using Gini Change to solve heterscedascity - Ruins model
Gimodel4 <- lm(giniChange ~ gdpGrowth + soprotEXP + eduEXP + eduATTAIN + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
summary(Gimodel4)
bptest(Gimodel4)

exp(-0.01060)
exp(0.0098576)

s80s20model3 <- lm(s80s20 ~ gdpGrowth + govEXP + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)

bptest(s80s20model3)

summary(s80s20model3)

if(bptest(s80s20model1)$statistic > qchisq(.95, df=7)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}

#USING SS80S20 Change to resolve heterscedascity - Ruins model
s80s20model2 <- lm(s80s20Change ~ gdpGrowth + soprotEXP + eduEXP + education + unemployment + log(FDIinc) + log(netTrade), Portfoliodata)
summary(s80s20model2)
bptest(s80s20model2)
if(bptest(s80s20model2)$statistic > qchisq(.95, df=7)){
  print("Reject the null hypothesis i.e. heteroscedascity is present")
} else {
  print("Fail to reject the null hypothesis i.e. no heteroscedascity detected")
}
