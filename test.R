# Import libraries
library(jsonlite)         # Import JSON-file
library(scatterplot3d)    # three-dimensional scatterplots
require(tikzDevice)       # Export plot as tex-file
library(epiR)             # Analyse confusion matrix
library(randomForest)     # Random forest algorithm
library(caret)            # Confusion matrix
library(class)            # Knn algorithm

brakePads = fromJSON("C:\Users\Sören\Documents\ClassificationOfProductData\data.json", flatten=TRUE)
df = data.frame(brakePads) # Convert to data frame 









df = df[complete.cases(df), ] # Remove null entries
  df[,3:5] = sapply(df[,3:5], as.numeric) # Convert columns to right values
  df[,2] = sapply(df[,2], as.factor) # Convert columns to right values
  completeData = df
  
  # Make classes appearing symmetric
  dHinterachse = df[df$classOfPart=="Hinterachse",]
  dVorderachse = df[df$classOfPart=="Vorderachse",]
  dVorderachse["random"]=runif(length(dVorderachse$partNumber))
  dVorderachse = dVorderachse[order(dVorderachse$random),]
  dVorderachse = dVorderachse[1:length(dHinterachse$partNumber),1:5]
  df = rbind(dHinterachse, dVorderachse)
  
  # Normalize data
  # dfNormZ = as.data.frame( scale(df[3:5] ))
  # df = cbind(df[,1:2], dfNormZ)
  
  # Separate train and test data
  set.seed(seed)
  ind = sample(2, nrow(df), replace=TRUE, prob=c(0.8, 0.2))
  train = df[ind==1,]
  test = df[ind==2,]
  
