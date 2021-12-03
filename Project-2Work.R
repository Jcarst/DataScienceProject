library(tidyverse)
library(olsrr)
library(GGally)
library(ggfortify)
library(mdsr)

data <- read.csv("Project3Sample4000.csv")
data <- data %>% select(!c(name, dob, c_jail_in, c_jail_out, RiskRecidScreeningDate, c_charge_desc))

model <- glm(isRecid~.-c_charge_degree
             -RiskViolenceScoreLevel
             -ageCat
             -RiskRecidScoreLevel
             -RiskViolenceDecileScore
             -juvMisdemeanerCount
             -juvOtherCount
             -c_days_from_compas
             -race
             -juvFelonyCount,data,family="binomial")
summary(model)

dataReduced <- data %>% select(sex,age,priorsCount,days_b_screening_arrest,RiskRecidDecileScore,isRecid)

dataReduced$age <- log(dataReduced$age)
model2 <- glm(isRecid~.,dataReduced,family="binomial")

summary(model2)

ggplot(data)