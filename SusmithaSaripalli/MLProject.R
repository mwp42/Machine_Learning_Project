### Machine Learning Project###
###    Confidence Squared   ###
# 

library(dplyr)
library(data.table)
library(randomForest)

# Load Data

dtest <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/test.csv", stringsAsFactors = TRUE)
dtrain <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/train.csv", stringsAsFactors = TRUE)

# # Quick look at data
# summary(dtest)
# describe(dtest)
# summary(dtrain)
# describe(dtrain)
# # Plot missingness
# image(is.na(dtest), main = "Missing Values", xlab = "Observation", ylab = "Variable", 
#       xaxt = "n", yaxt = "n", bty = "n")
# axis(1, seq(0, 1, length.out = nrow(dtest)), 1:nrow(dtest), col = "white")
# axis(2, c(0, 0.5, 1), names(dtest), col = "white", las = 2)
# 
# image(is.na(dtrain), main = "Missing Values", xlab = "Observation", ylab = "Variable", 
#       xaxt = "n", yaxt = "n", bty = "n")
# axis(1, seq(0, 1, length.out = nrow(dtrain)), 1:nrow(dtrain), col = "white")
# axis(2, c(0, 0.5, 1), names(dtrain), col = "white", las = 2)


# Defining cleaning functions:

# add "None" as a level for group1
addLevel <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
  return(x)
}

# For the rest columns, qualitative assign the most frequest, quantitative assign 0
missFun1 <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- 0
    #mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

# Transform: fill missing and remove some columns
transFun <- function(t) {
  
  t <- subset(t, select=-c(MiscFeature,Fence,PoolQC,Alley,Street,Utilities,Condition2,RoofMatl,Id,PoolArea,LotFrontage))
  
  # missing GarageYrBlt should equal YearBuilt
  t$GarageYrBlt[is.na(t$GarageYrBlt)] = t$YearBuilt[is.na(t$GarageYrBlt)]
  
  # Separating groups for different functions
  cols = c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond")
  group1 <-t[,cols]
  group2 <-t[,!(colnames(t) %in% cols)]
  
  # adding "None" level
  group1 <- as.data.frame(lapply(group1, addLevel))
  group1[is.na(group1)] <- "None"
  
  # Assigning 0 or most regularly occuring value to variable
  group2=data.table(group2)
  group2[, lapply(.SD, missFun1)]
    
  # combine the two groups
  newt=cbind(group1,group2)
  
  
  return(newt)
  
}

# Process Test set and Training set in the same way
nTrain = transFun(dtrain)
nTest = transFun(dtest)

# Quick fix for errors
nTrain1 <- nTrain[complete.cases(nTrain), ]

# Implementing Random Forest
model1 <- randomForest(SalePrice ~ ., data = nTrain1, ntree = 1000, mtry =10, importance = TRUE)
model1
varImpPlot(model1)
