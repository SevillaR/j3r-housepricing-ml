library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
library(caret)
library(xgboost)
library(readr)

# Load test data 
test <- read_csv("./data/test.csv")
train <- read_csv("./data/train.csv")

str(test)
skim(test)

mode(test[1, ])
