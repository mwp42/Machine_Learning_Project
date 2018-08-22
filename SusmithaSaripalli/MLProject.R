### Machine Learning Project###
###    Confidence Squared   ###
# 
# Load Data
library(dplyr)
library(data.table)

dtest <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/test.csv", stringsAsFactors = FALSE)
dtrain <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/train.csv", stringsAsFactors = FALSE)

dtrain <- subset(dtrain, select=-c(MiscFeature,Fence,PoolQC,Alley,Street,Utilities,Condition2,RoofMatl,ID,PoolArea))
dtest <- subset(dtest, select=-c(MiscFeature,Fence,PoolQC,Alley,Street,Utilities,Condition2,RoofMatl,ID,PoolArea))

#missing GarageYrBlt should equal to YearBuilt
dtrain$GarageYrBlt[is.na(dtrain$GarageYrBlt)] = dtrain$YearBuilt[is.na(dtrain$GarageYrBlt)]
dtest$GarageYrBlt[is.na(dtest$GarageYrBlt)] = dtrain$YearBuilt[is.na(dtest$GarageYrBlt)]

#For training set in several columns, missing value should assign "NA" value as a category, using myFun1
Traingroup1 <-dtrain[,c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond")]
Traingroup2 <-dtrain[,!(colnames(dtrain) %in% c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond"))]

Testgroup1 <-dtest[,c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond")]
Testgroup1 <-dtest[,!(colnames(dtrain) %in% c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond"))]

myFun1 <- function(x) {
  
  x[is.na(x)] <- "None"
  
}

myFun2 <- function(x) {
  
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}



Traingroup1=data.table(Traingroup1)
Traingroup2=data.table(Traingroup2)

Traingroup1[, lapply(.SD, myFun1)]
Traingroup2[, lapply(.SD, myFun2)]

Testgroup1=data.table(Testgroup1)
Testgroup2=data.table(Testgroup2)

Testgroup1[, lapply(.SD, myFun1)]
Testgroup2[, lapply(.SD, myFun2)]
#get the new training group without NA
toTrain = cbind(Traingroup1,Traingroup2)
toTest = cbind(Testgroup1,Testgroup2)

image(is.na(dtest), main = "Missing Values", xlab = "Observation", ylab = "Variable", 
      xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(dtest)), 1:nrow(dtest), col = "white")
axis(2, c(0, 0.5, 1), names(dtest), col = "white", las = 2)