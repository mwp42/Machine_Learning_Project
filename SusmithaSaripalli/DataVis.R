### Machine Learning Project
### Team Confidence Squared

library(shiny)
library(VIM)
library(mice)
library(caret)
library(Hmisc)
library(dplyr)
library(deldir)
library(shinythemes)

# Load data
dtest <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/test.csv", stringsAsFactors = FALSE)
dtrain <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/machinelearningproj/all/train.csv", stringsAsFactors = FALSE)

# Quick look at data
summary(dtest)
describe(dtest)
summary(dtrain)
describe(dtrain)
# Plot missingness
image(is.na(dtest), main = "Missing Values", xlab = "Observation", ylab = "Variable",
      xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(dtest)), 1:nrow(dtest), col = "white")
axis(2, c(0, 0.5, 1), names(dtest), col = "white", las = 2)

image(is.na(dtrain), main = "Missing Values", xlab = "Observation", ylab = "Variable",
      xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(dtrain)), 1:nrow(dtrain), col = "white")
axis(2, c(0, 0.5, 1), names(dtrain), col = "white", las = 2)


# creating a color vector for plotting purposes
colorvec = c(rep("red", 50),
             rep("green", 50),
             rep("blue", 50))
colorvec[idx_missing] = "black"
shapevec = c(rep(0, 50),
             rep(1, 50),
             rep(2, 50))

# Plotting function
PlotIt = function(x, mainTitle = "Imputed Data") {
  plot(
    x,
    iris$Sepal.Width,
    col = colorvec,
    pch = shapevec,
    main = mainTitle,
    xlab = 'Sepal Length',
    ylab = 'Sepal Width'
  )
  legend(
    "topleft",
    c("Setosa", "Versicolor", "Virginica", "NA"),
    pch = 16,
    col = c("red", "green", "blue", "black"),
    cex = 1
  )
}



