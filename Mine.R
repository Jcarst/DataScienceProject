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
  labs(fill="Reoffended",x="Gender",y="Count",title="Gender vs. Reoffended")

ggplot(data=data,aes(fill=as.factor(isRecid), x=RiskRecidDecileScore)) +
  geom_density(alpha=.2) + 
  facet_wrap(~race) +
  labs(title="Risk of Recidivism Score vs. Reoffended",x="Risk of Recidivism Score",y="Count",fill="Reoffended")
