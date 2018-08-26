### Machine Learning Project###
###    Confidence Squared   ###
# Susmitha Saripalli
# Data cleaning

library(dplyr)
library(data.table)
library(plyr)

# Load Data

t <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/train.csv", stringsAsFactors = FALSE)

# Defining levels (from data description)
Qual_lev <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
# for ExterQual, ExterCond, BsmtQual, BsmtCond, HeatingQC, KitchenQual,FireplaceQu, GarageQual, GarageCond, PoolQC
BsmtFin_lev <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
# for BsmtFinType1, BsmtFinType2
BsmtExp_lev <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
# for BsmtExposure
Func_lev <- c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)
# for Functional

# Remove ID
t$Id <- NULL

# Fix GarageYrBlt
t$GarageYrBlt[is.na(t$GarageYrBlt)] = t$YearBuilt[is.na(t$GarageYrBlt)]
# Select group for Qual_lev
col1 <- c('ExterQual','ExterCond','BsmtQual','BsmtCond','HeatingQC','KitchenQual','FireplaceQu','GarageQual','GarageCond', 'PoolQC')
col2 <- c('BsmtFinType1','BsmtFinType2','BsmtExposure','Functional','MiscFeature','Alley','Fence', 'GarageType','GarageFinish')
colT <- append(col1, col2)

group1 <- subset(t, select=col1)
group2 <- subset(t, select=col2)
group3 <- t[,!(colnames(t) %in% colT)]

group1[is.na(group1)] <- "None"
group1 <- as.data.frame(
  lapply(group1, function(x) {
    revalue(x,Qual_lev)
    }))

group2[is.na(group2)] <- "None"
group2$BsmtFinType1 <- as.integer(revalue(group2$BsmtFinType1, BsmtFin_lev))
group2$BsmtFinType2 <- as.integer(revalue(group2$BsmtFinType2, BsmtFin_lev))
group2$BsmtExposure <- as.integer(revalue(group2$BsmtExposure, BsmtExp_lev))
group2$Functional <- as.integer(revalue(group2$Functional, Func_lev))
group2$MiscFeature <- as.factor(group2$MiscFeature)
group2$Alley <- as.factor(group2$Alley)
group2$Fence <- as.factor(group2$Fence)
group2$GarageType <- as.factor(group2$GarageType)
group2$GarageFinish <- as.factor(group2$GarageFinish)


# if the houses with veneer area NA are also NA in the veneer type,
# find the one that should have a MasVnrType,
# assign the veneer as the most popular (that is not none)
if(length(which(is.na(group3$MasVnrType) & !is.na(group3$MasVnrArea)))>0){
  group3[is.na(group3$MasVnrType) & !is.na(group3$MasVnrArea),]$MasVnrType <- names(sort(-table(group3$MasVnrType)))[2]
}
group3$MasVnrArea[is.na(group3$MasVnrArea)] <- 0
group3$MasVnrType[is.na(group3$MasVnrType)] <- 'None'
group3$MasVnrType <- as.factor(group3$MasVnrType)
# Assumed NA LotFrontage means no LotFrontage as there are no zeros in the column
group3$LotFrontage[is.na(group3$LotFrontage)] <- 0
group3$Electrical[is.na(group3$Electrical)] <- names(which.max(table(group3$Electrical)))
t <- cbind(group1,group2,group3)
# Check missing values
NAcol <- which(colSums(is.na(t)) > 0)
sort(colSums(sapply(t[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')




