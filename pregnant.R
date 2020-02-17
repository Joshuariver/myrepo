# Learning Data Science: The Supermarket knows you are pregnant before your Dad does
# https://www.r-bloggers.com/learning-data-science-the-supermarket-knows-you-are-pregnant-before-your-dad-does/


rm(list=ls())

RetailMart <- read.csv("RetailMart.csv") # load data

head(RetailMart)
tail(RetailMart)

table(RetailMart$PREGNANT)

str(RetailMart)


# The metadata for each feature are the following:
  
# Account holder is Male/Female/Unknown by matching surname to census data.
# Account holder address is a home, apartment, or PO box.
# Recently purchased a pregnancy test
# Recently purchased birth control
# Recently purchased feminine hygiene products
# Recently purchased folic acid supplements
# Recently purchased prenatal vitamins
# Recently purchased prenatal yoga DVD
# Recently purchased body pillow
# Recently purchased ginger ale
# Recently purchased Sea-Bands
# Bought cigarettes regularly until recently, then stopped
# Recently purchased cigarettes
# Recently purchased smoking cessation products (gum, patch, etc.)
# Bought wine regularly until recently, then stopped
# Recently purchased wine
# Recently purchased maternity clothing

# For building the actual model we use glm (for generalized linear model):
  
logreg <- glm(PREGNANT ~ ., data = RetailMart, family = binomial) # logistic regression - glm stands for generalized linear model
summary(logreg)


pred <- ifelse(predict(logreg,RetailMart[ , -ncol(RetailMart)], "response") < 0.5, 0, 1) # naive approach to predict whether pregnant
results <- data.frame(actual = RetailMart$PREGNANT, prediction = pred)
results[460:520, ]

(conf <- table(pred, RetailMart$PREGNANT)) # create confusion matrix

sum(diag(conf)) / sum(conf) # calculate accuracy

