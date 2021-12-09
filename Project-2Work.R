library(tidyverse)
library(olsrr)
library(GGally)
library(ggfortify)
library(mdsr)



data <- read.csv("Project3Sample4000.csv")

############### Matt's code
data <- data %>% select(!c(name, dob, c_jail_in, c_jail_out, RiskRecidScreeningDate, c_charge_desc))

modelAll <- glm(isRecid~.,data,family="binomial")
summary(modelAll)

# Plots
ggplot(data,aes(x=log(age),fill=as.factor(isRecid),alpha=.5)) + geom_density()

model <- glm(isRecid~.-c_charge_degree
             -RiskViolenceScoreLevel
             -ageCat
             -RiskRecidScoreLevel
             -RiskViolenceDecileScore
             -juvMisdemeanerCount
             -juvOtherCount
             -c_days_from_compas
             -race
             -juvFelonyCount
             -days_b_screening_arrest,data,family="binomial")
summary(model)

dataReduced <- data %>% select(sex,age,priorsCount,RiskRecidDecileScore,isRecid)

dataReduced$age <- log(dataReduced$age)
model2 <- glm(isRecid~.,dataReduced,family="binomial")

summary(model2)

ggplot(data=data,aes(x=sex, fill=as.factor(isRecid))) + 
  geom_bar(position="dodge") +
  labs(fill="Reoffended",x="Gender",y="Count")


data <- read.csv("Project3Sample4000.csv")

pairs <- data %>% select(!c(name,dob, c_jail_in, c_jail_out, RiskRecidScreeningDate,c_charge_desc))

data$ageCat <- as.factor(data$ageCat)
data$c_charge_degree <- as.factor(data$c_charge_degree)

ggpairs(pairs)

recidModel <- glm(isRecid ~., pairs, family = "binomial")
model <- glm(isRecid ~.-ageCat, pairs, family = "binomial")

ggplot(pairs, aes(x=race, fill=as.factor(isRecid), color = race, alpha = 0.5)) +  geom_density(aes(color = race))

ggplot(pairs, aes(x=sex, fill = as.factor(isRecid), color = sex, alpha = 0.2)) + geom_density()

ggplot(pairs, aes(x=priorsCount, y = isRecid, color = race)) + geom_point() + geom_jitter()

priorsModel <- glm(isRecid ~ priorsCount, pairs, family = "binomial")
summary(priorsModel)

pairs$RiskRecidDecileScore <- log(pairs$RiskRecidDecileScore)
ggpairs(pairs)



############# references
# https://rpubs.com/sweeneys/STATS504-COMPAS       