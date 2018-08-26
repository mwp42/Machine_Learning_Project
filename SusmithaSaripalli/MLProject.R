### Machine Learning Project###
###    Confidence Squared   ###
# Susmitha Saripalli
# Data cleaning

# clear Workspace
rm(list = ls())

library(dplyr)
library(data.table)
library(plyr)
library(Metrics)

# Load Data

dtrain <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/train.csv", stringsAsFactors = FALSE) #train
dtest <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/test.csv", stringsAsFactors = FALSE) #test
# Defining levels (from data description)
Qual_lev <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
# for ExterQual, ExterCond, BsmtQual, BsmtCond, HeatingQC, KitchenQual,FireplaceQu, GarageQual, GarageCond, PoolQC
BsmtFin_lev <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
# for BsmtFinType1, BsmtFinType2
BsmtExp_lev <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
# for BsmtExposure
Func_lev <- c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)
# for Functional

transFun <- function(t) {
  # Remove ID
  t$Id <- NULL #train
  # Fix GarageYrBlt
  t$GarageYrBlt[is.na(t$GarageYrBlt)] = t$YearBuilt[is.na(t$GarageYrBlt)] #train
  # Select group for Qual_lev
  col1 <- c('ExterQual','ExterCond','BsmtQual','BsmtCond','HeatingQC','KitchenQual','FireplaceQu','GarageQual','GarageCond', 'PoolQC', 'Exterior1st','Exterior2nd')
  col2 <- c('BsmtFinType1','BsmtFinType2','BsmtExposure','MiscFeature','Alley','Fence', 'GarageType','GarageFinish')
  colT <- append(col1, col2)
  
  group1 <- subset(t, select=col1) #train
  group2 <- subset(t, select=col2) #train
  group3 <- t[,!(colnames(t) %in% colT)] #train
  
  group1[is.na(group1)] <- "None" #train
  group1 <- as.data.frame(
    lapply(group1, function(x) {
      revalue(x,Qual_lev)
    })) #train
  
  group2[is.na(group2)] <- "None" #train
  group2$BsmtFinType1 <- as.integer(revalue(group2$BsmtFinType1, BsmtFin_lev)) #train
  group2$BsmtFinType2 <- as.integer(revalue(group2$BsmtFinType2, BsmtFin_lev)) #train
  group2$BsmtExposure <- as.integer(revalue(group2$BsmtExposure, BsmtExp_lev)) #train
  group2$MiscFeature <- as.factor(group2$MiscFeature) #train
  group2$Alley <- as.factor(group2$Alley) #train
  group2$Fence <- as.factor(group2$Fence) #train
  group2$GarageType <- as.factor(group2$GarageType) #train
  group2$GarageFinish <- as.factor(group2$GarageFinish) #train
  
  
  # if the houses with veneer area NA are also NA in the veneer type,
  # find the one that should have a MasVnrType,
  # assign the veneer as the most popular (that is not none)
  if(length(which(is.na(group3$MasVnrType) & !is.na(group3$MasVnrArea)))>0){
    group3[is.na(group3$MasVnrType) & !is.na(group3$MasVnrArea),]$MasVnrType <- names(sort(-table(group3$MasVnrType)))[2]
  } #train
  
  #group3$MasVnrArea[is.na(group3$MasVnrArea)] <- 0 #train
  group3$MasVnrType[is.na(group3$MasVnrType)] <- 'None' #train
  group3$MasVnrType <- as.factor(group3$MasVnrType) #train
  group3$MSZoning[is.na(group3$MSZoning)] <- names(which.max(table(group3$MSZoning))) #train
  group3$Utilities[is.na(group3$Utilities)] <- names(which.max(table(group3$Utilities))) #train
  group3$SaleType[is.na(group3$SaleType)] <- names(which.max(table(group3$SaleType))) #train
  group3[,c('MasVnrArea','LotFrontage','BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','GarageCars','GarageArea','BsmtFullBath','BsmtHalfBath','TotalBsmtSF')] <- 0 #train
  group3$Electrical[is.na(group3$Electrical)] <- names(which.max(table(group3$Electrical))) #train
  group3$Functional[is.na(group3$Functional)] <- names(which.max(table(group3$Functional))) #train
  group3$Functional <- as.integer(revalue(group3$Functional, Func_lev)) #train
  newt <- cbind(group1,group2,group3) #train
  newt[is.character(newt)] <- as.factor( newt[is.character(newt)])
  return(newt)
}

# Process Test set and Training set in the same way
nTrain = transFun(dtrain)
nTest = transFun(dtest)

#make log of SalePrice as the predict variable in nlTrain dataset
nTrain$logSalePrice <- log(nTrain$SalePrice)
train<-nTrain[,-which(names(nTrain) == "SalePrice")]   #take out SalePrice column as it is the response variable

#y_train and train have separate y and all x
y_train<-train$logSalePrice


#separate training and testing dataset from the available data
library(caret)
set.seed(123)
partition <- createDataPartition(y=y_train,    #createDataPartition(< y (a.k.a. the response variable)>,p=<split point (50/50 here)>, list=F)
                                 p=.5,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]


#run Linear regression
lm_model_1 <- lm(logSalePrice ~ ., data=training)
summary(lm_model_1)
#Ajusted R squared is not bad (0.9383)


#use random forest
library(tree)
library(randomForest)
set.seed(0)
model_1 <- randomForest(logSalePrice ~ ., data=training)

# Predict using the test set
prediction <- predict(model_1, testing)   

model_output <- cbind(testing, prediction) 

#Test with RMSE
rmse(model_output$logSalePrice,model_output$prediction) #RMSE = 0.1554594
#remember, prediction was built off of "testing" set
#and model_output$logSalePrice comes from the "training" set


# Ridge Regression and Lasso
library(glmnet)

x_rr <- as.matrix(nTrain[,-1]) # Removes class
y_rr <- as.double(as.matrix(nTrain$logSalePrice)) # Only class

lambda <- 10^seq(10,02,length = 100)

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
RR_1 <- glmnet(x_rr, y_rr, alpha = 1)

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)





