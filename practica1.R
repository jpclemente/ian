knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(caret)
library(Hmisc)
library(ggplot2)
library(VIM)
library(summarytools)
library(glmnet)

source('transformations.R')

set.seed(12345)

kc_house_data <- read_csv("data/kc_house_data.csv")
inTraining <- createDataPartition(pull(kc_house_data), p = .7, list = FALSE,times = 1)

house_training <- slice(kc_house_data, inTraining)
house_testing <- slice(kc_house_data, -inTraining)

house_training <- transform(house_training)

house_training <- na.omit(house_training)
house_testing <- na.omit(house_testing)

glm.fit = glm(price ~ ., data=house_training)




x_train <- model.matrix(price~.,house_training)[,-1]
y_train <- house_training$price

cv.out <- cv.glmnet(x_train, y_train, alpha=1, nfolds=10, type.measure="mse")

house_testing <- transform(house_testing)
x_test <- data.matrix(house_testing %>% select(-price))

y_predicted_test=predict(cv.out,newx = x_test, type = "response")
y_real_test = house_testing$price

y = (y_real_test - y_predicted_test)^2
mean(y)


#plot(cv.out)

freq(house_training$bedrooms, plain.ascii = FALSE, style = "rmarkdown")
