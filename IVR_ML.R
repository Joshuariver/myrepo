# Intrumental variable regression and machine learning
# https://www.r-bloggers.com/intrumental-variable-regression-and-machine-learning/



library(tidyverse)
library(randomForest)
library(wooldridge)
library(AER)
library(Metrics)

data("mroz")

working_w <- mroz %>% 
  filter(inlf == 1)

wage_lm <- lm(lwage ~ exper + expersq + educ, 
              data = working_w)

summary(wage_lm)

wage_lm2 <- lm(lwage ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage + city + educ, 
               data = working_w)

summary(wage_lm2)

first_stage <- lm(educ ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage 
                  + city + motheduc +  fatheduc + huseduc, data = working_w)

working_w$predictions_first_stage <- predict(first_stage)

wage_lm2 <- lm(lwage ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage + city + educ, 
               data = working_w)

second_stage <- lm(lwage ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage 
                   + city + predictions_first_stage,
                   data = working_w)

summary(second_stage)

inst_reg <- ivreg(lwage ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage + city + educ 
                  | .-educ + motheduc + fatheduc + huseduc,
                  data = working_w)

summary(inst_reg)

set.seed(42)
sample <- sample.int(n = nrow(working_w), size = floor(.90*nrow(working_w)), replace = F)
train <- working_w[sample, ]
test  <- working_w[-sample, ]

second_stage <- lm(lwage ~ exper + expersq + kidslt6 + kidsge6 + 
                     husage + huswage + city + predictions_first_stage,
                   data = train)

summary(second_stage)

library(randomForest)

first_stage_rf <- randomForest(educ ~ exper + expersq + kidslt6 + kidsge6 + husage + huswage 
                               + city + motheduc +  fatheduc + huseduc, 
                               data = train)

test$predictions_first_stage_rf <- predict(first_stage_rf, newdata = test)

rf_rmse <- rmse(predicted = test$predictions_first_stage_rf, actual = test$educ)

train$predictions_first_stage_rf <- predict(first_stage_rf)

second_stage_rf_lm <- lm(lwage ~ exper + expersq + kidslt6 + kidsge6 + 
                           husage + huswage + city + predictions_first_stage_rf,
                         data = train)

summary(second_stage_rf_lm)

second_stage_rf_rf <- randomForest(lwage ~ exper + expersq + kidslt6 + kidsge6 + 
                                     husage + huswage + city + predictions_first_stage_rf,
                                   data = train)

library("iml")

predictor <- Predictor$new(
  model = second_stage_rf_rf, 
  data = select(test, exper, expersq,
                kidslt6, kidsge6,
                husage, huswage, city,
                predictions_first_stage_rf), 
  y = test$lwage, 
  predict.fun = predict,
  class = "regression"
)

imp_rf <- FeatureImp$new(predictor, loss = "rmse")

plot(imp_rf)

interactions <- Interaction$new(predictor, feature = "predictions_first_stage_rf")

plot(interactions)

corr_vars <- cor(select(test, exper, expersq,
                        kidslt6, kidsge6,
                        husage, huswage, city,
                        predictions_first_stage_rf)) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname, names_to = "vars2") %>% 
  rename(vars1 = rowname)

head(corr_vars)

inst_effect <- FeatureEffect$new(predictor, "predictions_first_stage_rf", method = "pdp+ice")

plot(inst_effect)                  

(new_obs <- data.frame(
  exper = rep(10, 2),
  expersq = rep(100, 2),
  kidslt6 = rep(1, 2),
  kidsge6 = c(0, 2),
  husage = rep(35, 2),
  huswage = rep(6, 2),
  city = rep(1, 2),
  predictions_first_stage_rf = rep(10, 2)
))

predict(second_stage_rf_rf, newdata = new_obs)

shapley_1 <-  Shapley$new(predictor, x.interest = new_obs[1, ], sample.size = 100)
shapley_2 <-  Shapley$new(predictor, x.interest = new_obs[2, ], sample.size = 100)

plot(shapley_1)

plot(shapley_2)

