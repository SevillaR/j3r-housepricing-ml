load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)

install.packages("data.table")
library("data.table")


install.packages("gridExtra")
library("gridExtra")


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

# 2. Explore data graphically looking for correlation

skim(test[numericCols])


library(data.table)
# setDT(train)

cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]

colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))


plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(train[,colSums(is.na(train)) > 0, with = FALSE])

summary(train[,.SD, .SDcols =numeric_var])


####Convert character to factors 

train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]



train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)

as.data.frame(train) %>% group_by(Neighborhood) %>% summarise(Media=mean(SalePrice)) %>% arrange(desc(Media))
as.data.frame(train) %>% group_by(Neighborhood) %>% tally() %>% arrange(desc(n))

#### xgboost
install.packages("xgboost")
library("xgboost")







