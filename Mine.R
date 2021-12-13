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
modelNSdata <- data %>% select(!RiskRecidDecileScore)
modelNS <- glm(data=modelNSdata,isRecid~sex+age+daysInJail+log(priorsCount+.1),family="binomial")
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
modelWS <- glm(data=data,isRecid~+sex+age+RiskRecidDecileScore+daysInJail+log(priorsCount+.1),family="binomial")
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












##### Predicting scores

mystery <- read.csv("Project3Mystery100.csv")

predictions <- as.data.frame(mystery$personID) %>%
  mutate(
    personID = `mystery$personID`
  ) %>%
  select(personID)

mdf <- mystery %>%
  separate(c_jail_in, c("inDay","inMonth","inYear")) %>%
  separate(c_jail_out,c("outDay","outMonth","outYear")) %>%
  mutate(
    daysInJail = ((as.integer(outYear)*365)+(as.integer(outMonth)*30)+(as.integer(outDay)))-((as.integer(inYear)*365)+(as.integer(inMonth)*30)+(as.integer(inDay)))
  ) %>%
  select(!c(dob, inDay, inMonth, inYear, outDay, outMonth, outYear, RiskRecidScreeningDate, c_charge_desc)) %>%
  mutate(
    crimeType = str_extract(c_charge_degree,"[MF]")
  ) %>%
  filter(
    !is.na(crimeType)
  ) %>%
  select(!c(crimeType,c_charge_degree,race,juvFelonyCount,juvMisdemeanerCount,juvOtherCount,ageCat,days_b_screening_arrest,c_days_from_compas))

willRecid <- predict.glm(modelNS,newdata=mdf,type="response")

willRecidPred <- cbind(mdf,willRecid)

willRecidPred <- willRecidPred %>%
  mutate(
    willRecid = ifelse(willRecid > .5, 1, 0)
  )

predictions <- predictions %>%
  mutate(
    willRecidivate = willRecidPred$willRecid
  )

bestModel <- lm(RiskRecidDecileScore ~ age + race + juvFelonyCount + juvOtherCount + priorsCount, data=Originaldata)

bmPredictions <- as.data.frame(predict.lm(bestModel,newdata=Originaldata))

summary(bestModel)

RMSE(bestModel$fitted.values,Originaldata$RiskRecidDecileScore)

recidDecile <- as.data.frame(predict.lm(bestModel,newdata=mystery))



RMSE <- function(predict, obs) {
  RMSE <-sqrt(mean((predict-obs)^2, na.rm=T))
  return(RMSE)
}

predictions <- predictions %>%
  mutate(
    predictedRecidScore = recidDecile$`predict.lm(bestModel, newdata = mystery)`
  )


############################
pleaseGodHelp <- Originaldata %>% 
  select(!c(c_charge_degree,c_charge_desc,name,dob,RiskRecidScreeningDate,c_jail_in,c_jail_out,RiskViolenceScoreLevel, isRecid, ageCat, RiskRecidScoreLevel, RiskRecidDecileScore, RiskViolenceScoreLevel, c_days_from_compas, days_b_screening_arrest))

byron <- lm(RiskViolenceDecileScore ~ ., pleaseGodHelp)

RMSE(byron$fitted.values,Originaldata$RiskViolenceDecileScore)

summary(byron)

chocolateChipIceCream <- ols_step_forward_p(byron)
chocolateChipIceCream

anime <- ols_step_backward_p(byron)
anime 

naruto <- ols_step_best_subset(byron)
naruto

initialD <- ols_step_both_p(byron) # Choose this one 
initialD

initialDPred <- as.data.frame(predict.lm(byron,newdata=mystery))

predictions <- predictions %>%
  mutate(
    predictedViolenceScore = initialDPred$`predict.lm(byron, newdata = mystery)`
  )

write.csv(predictions,"Bushnell_Carstens_Predictions.csv")

#



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


# Gender studies


males <- read.csv("Project3Males1500.csv")
females <- read.csv("Project3Females1500.csv")

fdf <- females %>%
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
  select(!c(daysInJail,juvMisdemeanerCount,juvFelonyCount,juvOtherCount,c_charge_degree,race,sex,RiskViolenceDecileScore,crimeType,ageCat,days_b_screening_arrest,c_days_from_compas,RiskRecidScoreLevel,RiskViolenceScoreLevel))

mdf <- males %>%
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
  select(!c(juvMisdemeanerCount,juvFelonyCount,juvOtherCount,c_charge_degree,race,sex,RiskViolenceDecileScore,crimeType,ageCat,days_b_screening_arrest,c_days_from_compas,RiskRecidScoreLevel,RiskViolenceScoreLevel))


fmodel <- glm(data=fdf,isRecid~.,family="binomial")
summary(fmodel)


mmodel <- glm(data=mdf,isRecid~.,family="binomial")
summary(mmodel)


fmodelPred <- predict.glm(fmodel,newdata=fdf,type="response")


fDataPred  <- cbind(fdf,fmodelPred)

fDataPred <- fDataPred %>%
  mutate(
    recidPred = ifelse(fmodelPred > .5, 1, 0)
  )

fmatrix <- table(fDataPred$recidPred,fDataPred$isRecid)
fmatrix

((fmatrix[1,2]+fmatrix[2,1])/count(fdf))

table(fdf$isRecid)[2]/count(fdf)





mmodelPred <- predict.glm(mmodel,newdata=mdf,type="response")


mDataPred  <- cbind(mdf,mmodelPred)

mDataPred <- mDataPred %>%
  mutate(
    recidPred = ifelse(mmodelPred > .5, 1, 0)
  )

mmatrix <- table(mDataPred$recidPred,mDataPred$isRecid)
mmatrix

((mmatrix[1,2]+mmatrix[2,1])/count(mdf))

table(mdf$isRecid)[2]/count(mdf)




########### Task 4

library(arules)


extraData <- Originaldata %>%
  separate(c_jail_in, c("inDay","inMonth","inYear")) %>%
  separate(c_jail_out,c("outDay","outMonth","outYear")) %>%
  mutate(
    daysInJail = ((as.integer(outYear)*365)+(as.integer(outMonth)*30)+(as.integer(outDay)))-((as.integer(inYear)*365)+(as.integer(inMonth)*30)+(as.integer(inDay)))
  ) %>%
  select(!c(name, inDay, inMonth, inYear, outDay, outMonth, outYear, RiskRecidScreeningDate, c_charge_desc)) %>%
  mutate(
    crimeType = str_extract(c_charge_degree,"[MF]")
  ) %>%
  filter(
    !is.na(crimeType)
  ) %>%
  separate(dob, c("birthDay","birthMonth")) %>%
  mutate(
    birthMonth = as.integer(str_remove(birthMonth,"^0+")),
    birthDay = as.integer(birthDay)
  ) %>%
  mutate(
    birthSign = case_when(
      (birthMonth == 1 & birthDay > 19) | (birthMonth == 2 & birthDay < 19) ~ "Aquarius",
      (birthMonth == 2 & birthDay > 18) | (birthMonth == 3 & birthDay < 21) ~ "Pisces",
      (birthMonth == 3 & birthDay > 20) | (birthMonth == 4 & birthDay < 20) ~ "Aries",
      (birthMonth == 4 & birthDay > 19) | (birthMonth == 5 & birthDay < 21) ~ "Taurus",
      (birthMonth == 5 & birthDay > 20) | (birthMonth == 6 & birthDay < 21) ~ "Gemini",
      (birthMonth == 6 & birthDay > 21) | (birthMonth == 7 & birthDay < 23) ~ "Cancer",
      (birthMonth == 7 & birthDay > 22) | (birthMonth == 8 & birthDay < 23) ~ "Leo",
      (birthMonth == 8 & birthDay > 22) | (birthMonth == 9 & birthDay < 23) ~ "Virgo",
      (birthMonth == 9 & birthDay > 22) | (birthMonth == 10 & birthDay < 23) ~ "Libra",
      (birthMonth == 10 & birthDay > 22) | (birthMonth == 11 & birthDay < 22) ~ "Scorpius",
      (birthMonth == 11 & birthDay > 21) | (birthMonth == 12 & birthDay < 22) ~ "Sagittarius",
      (birthMonth == 12 & birthDay > 21) | (birthMonth == 1 & birthDay < 20) ~ "Capricorn"
    )) %>%
  select(!c(birthDay, birthMonth, crimeType,c_charge_degree,ageCat,days_b_screening_arrest,c_days_from_compas,RiskRecidScoreLevel,RiskViolenceScoreLevel)) %>%
  filter(!is.na(birthSign))


ggplot(data=extraData,aes(x=birthSign, fill=as.factor(isRecid))) +
  geom_bar(position="dodge") +
  labs(title="Count of Birthsign and Reoffended",x="Birth Sign",y="Count",fill="Reoffended") +
  theme(axis.text.x=element_text(angle =- 20,hjust=-.1))

ggplot(data=extraData,aes(x=birthSign,y=juvFelonyCount+juvMisdemeanerCount,fill=as.factor(isRecid)))+
  geom_col() +
  labs(fill="Reoffended", title="Juvenile Felony+Misdemeaner Count vs. Birth Sign", x="Birth Sign", y="Juvenile Felony+Misdemeaner Count") +
  theme(axis.text.x=element_text(angle =- 20,hjust=-.1))



modelWhat <- lm(data=extraData,juvFelonyCount~.)
summary(modelWhat)


modelSign <- glm(data=extraData,isRecid~.,family="binomial")
summary(modelSign)













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
  geom_density(alpha=.3) +
  xlim(0,25)

