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

which(str(test) == 'character' )

numericCols <- sapply(test, is.numeric)
categoricalCols <- sapply(test, is.character)

colnames(test[numericCols])

# [1] "Id"            "MSSubClass"    "LotFrontage"   "LotArea"       "OverallQual"   "OverallCond"  
# [7] "YearBuilt"     "YearRemodAdd"  "MasVnrArea"    "BsmtFinSF1"    "BsmtFinSF2"    "BsmtUnfSF"    
# [13] "TotalBsmtSF"   "1stFlrSF"      "2ndFlrSF"      "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
# [19] "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  "TotRmsAbvGrd" 
# [25] "Fireplaces"    "GarageYrBlt"   "GarageCars"    "GarageArea"    "WoodDeckSF"    "OpenPorchSF"  
# [31] "EnclosedPorch" "3SsnPorch"     "ScreenPorch"   "PoolArea"      "MiscVal"       "MoSold"       
# [37] "YrSold"    

colnames(test[categoricalCols])

# [1] "MSZoning"      "Street"        "Alley"         "LotShape"      "LandContour"   "Utilities"    
# [7] "LotConfig"     "LandSlope"     "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
# [13] "HouseStyle"    "RoofStyle"     "RoofMatl"      "Exterior1st"   "Exterior2nd"   "MasVnrType"   
# [19] "ExterQual"     "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"      "BsmtExposure" 
# [25] "BsmtFinType1"  "BsmtFinType2"  "Heating"       "HeatingQC"     "CentralAir"    "Electrical"   
# [31] "KitchenQual"   "Functional"    "FireplaceQu"   "GarageType"    "GarageFinish"  "GarageQual"   
# [37] "GarageCond"    "PavedDrive"    "PoolQC"        "Fence"         "MiscFeature"   "SaleType"     
# [43] "SaleCondition"

