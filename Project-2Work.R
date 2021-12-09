library(tidyverse)
library(olsrr)
library(GGally)
library(ggfortify)
library(mdsr)


data <- read.csv("Project3Sample4000.csv")

pairs <- data %>% select(!c(name,dob, c_jail_in, c_jail_out, RiskRecidScreeningDate,c_charge_desc))

data$ageCat <- as.factor(data$ageCat)
data$c_charge_degree <- as.factor(data$c_charge_degree)

ggpairs(pairs)

recidModel <- glm(isRecid ~., pairs, family = "binomial")
model <- glm(isRecid ~.-ageCat, pairs, family = "binomial")



ggplot(pairs, aes(x=sex, y = isRecid, color = sex, alpha = 0.2)) + geom_density()

ggplot(pairs, aes(x=priorsCount, fill = as.factor(isRecid), alpha = 0.1)) + geom_density()

priorsModel <- glm(isRecid ~ priorsCount, pairs, family = "binomial")
summary(priorsModel)

pairs$RiskRecidDecileScore <- log(pairs$RiskRecidDecileScore)
ggpairs(pairs)

regressionModel <- glm(isRecid ~ sex + priorsCount + age, pairs, family = "binomial")
summary(regressionModel)

ageModel <- glm(isRecid ~ log(age), pairs, family = "binomial")
summary(ageModel)


### Multiple Regression ### 

mrData <- pairs %>% 
  select(!c(RiskViolenceScoreLevel, isRecid, ageCat, RiskRecidScoreLevel, RiskViolenceDecileScore, RiskViolenceScoreLevel, c_days_from_compas, days_b_screening_arrest)) %>% 
  mutate(logAge = log10(age))

RiskRecidDecileModel <- lm(RiskRecidDecileScore ~., mrData)

forwardSelect <- ols_step_forward_p(RiskRecidDecileModel)
forwardSelect

backwardsSelect <- ols_step_backward_p(RiskRecidDecileModel)
backwardsSelect

bestSubset <- ols_step_best_subset(RiskRecidDecileModel)
bestSubset # Pick 8 

mixedSelection <- ols_step_both_p(RiskRecidDecileModel)
mixedSelection

bestModel <- lm(RiskRecidDecileScore ~ age + race + juvFelonyCount + juvOtherCount + priorsCount, mrData)
summary(bestModel)
trial <- ols_step_best_subset(bestModel)

### Riskof Violence Decile Score ### 

pleaseGodHelp <- pairs %>% 
  select(!c(RiskViolenceScoreLevel, isRecid, ageCat, RiskRecidScoreLevel, RiskRecidDecileScore, RiskViolenceScoreLevel, c_days_from_compas, days_b_screening_arrest))

byron <- lm(RiskViolenceDecileScore ~ ., pleaseGodHelp)

chocolateChipIceCream <- ols_step_forward_p(byron)
chocolateChipIceCream

anime <- ols_step_backward_p(byron)
anime 

naruto <- ols_step_best_subset(byron)
naruto

initialD <- ols_step_both_p(byron) # Choose this one 
initialD
