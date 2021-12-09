library(tidyverse)
library(olsrr)
library(GGally)
library(ggfortify)
library(mdsr)

Originaldata <- read.csv("Project3Sample4000.csv")

############### Matt's code
data <- Originaldata %>%
  separate(c_jail_in, c("inDay","inMonth","inYear")) %>%
  separate(c_jail_out,c("outDay","outMonth","outYear")) %>%
  mutate(
    daysInJail = ((as.integer(outYear)*365)+(as.integer(outMonth)*30)+(as.integer(outDay)))-((as.integer(inYear)*365)+(as.integer(inMonth)*30)+(as.integer(inDay)))
  ) %>%
  select(!c(name, dob, inDay, inMonth, inYear, outDay, outMonth, outYear, RiskRecidScreeningDate, c_charge_desc)) %>%
  mutate(
    crimeType = str_extract(c_charge_degree,"[MF]")
  ) %>%
  filter(
    !is.na(crimeType)
  ) %>%
  select(!c(RiskViolenceDecileScore,crimeType,c_charge_degree,race,juvFelonyCount,juvMisdemeanerCount,juvOtherCount,ageCat,days_b_screening_arrest,c_days_from_compas,RiskRecidScoreLevel,RiskViolenceScoreLevel))


ggplot(data=data,aes(x=crimeType,fill=as.factor(isRecid)))+
  geom_bar(position="dodge")

# model with no scores
modelNS <- glm(data=data,isRecid~.-RiskRecidDecileScore+log(priorsCount+.1)-priorsCount,family="binomial")
summary(modelNS)


modelNSPred <- predict.glm(modelNS,newdata=data,type="response")


dataWNSPred  <- cbind(data,modelNSPred)

dataWNSPred <- dataWNSPred %>%
  mutate(
    recidPred = ifelse(modelNSPred > .5, 1, 0)
  )

WNSmatrix <- table(dataWNSPred$recidPred,dataWNSPred$isRecid)
WNSmatrix

((WNSmatrix[1,2]+WNSmatrix[2,1])/count(data))

table(data$isRecid)[2]/count(data)


# Model with scores
modelWS <- glm(data=data,isRecid~.+log(priorsCount+.1)-priorsCount,family="binomial")
summary(modelWS)

modelWSPred <- predict.glm(modelWS,newdata=data,type="response")


dataWSPred  <- cbind(data,modelWSPred)

dataWSPred <- dataWSPred %>%
  mutate(
    recidPred = ifelse(modelWSPred > .5, 1, 0)
  )

WSmatrix <- table(dataWSPred$recidPred,dataWSPred$isRecid)
WSmatrix

((WSmatrix[1,2]+WSmatrix[2,1])/count(data))






#### Multiple regression











#  Risk of recidivism model.

recidData <- Originaldata %>%
  separate(c_jail_in, c("inDay","inMonth","inYear")) %>%
  separate(c_jail_out,c("outDay","outMonth","outYear")) %>%
  mutate(
    daysInJail = ((as.integer(outYear)*365)+(as.integer(outMonth)*30)+(as.integer(outDay)))-((as.integer(inYear)*365)+(as.integer(inMonth)*30)+(as.integer(inDay)))
  ) %>%
  select(!c(name, dob, inDay, inMonth, inYear, outDay, outMonth, outYear, RiskRecidScreeningDate, c_charge_desc)) %>%
  mutate(
    totalPriors = priorsCount+juvFelonyCount+juvMisdemeanerCount+juvOtherCount
  ) %>%
  select(!c(priorsCount,juvFelonyCount,juvMisdemeanerCount,juvOtherCount)) %>%
  select(!c(isRecid))

modelRecidAll <- lm(data=recidData,RiskRecidDecileScore~.)
summary(modelRecidAll)




# Plots








ggplot(data=data,aes(x=sex, fill=as.factor(isRecid))) + 
  geom_bar(position="dodge") +
  labs(fill="Reoffended",x="Gender",y="Count",title="Gender vs. Reoffended")

ggplot(data=data,aes(fill=as.factor(isRecid), x=RiskRecidDecileScore)) +
  geom_density(alpha=.2) + 
  facet_wrap(~race) +
  labs(title="Risk of Recidivism Density and Reoffended",x="Risk of Recidivism Score",y="Density",fill="Reoffended")

ggplot(data=data,aes(x=totalPriors,fill=as.factor(isRecid))) +
  geom_density(alpha=.3) +
  labs(title="Priors Density and Reoffended",x="Priors",y="Density",fill="Reoffended")

ggplot(data=data,aes(x=c_charge_degree,fill=as.factor(isRecid))) +
  geom_bar(position="dodge") +
  labs(title="Charge Degree and Reoffended",x="Charge Degree",y="Count",fill="Reoffended")

ggplot(data=data,aes(x=daysInJail,fill=as.factor(isRecid))) +
  geom_density(alpha=.3)

