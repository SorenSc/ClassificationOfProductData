# Import libraries
library(jsonlite)         # Import JSON-file
library(scatterplot3d)    # three-dimensional scatterplots
require(tikzDevice)       # Export plot as tex-file
library(epiR)             # Analyse confusion matrix
library(randomForest)     # Random forest algorithm
library(caret)            # Confusion matrix
library(class)            # Knn algorithm

t = prepareData()
head(t$completeData)
plotBrakePadData(t$completeData)

data = t$completeData

# Design of plot
colors = c("firebrick4", "dodgerblue4")
xlab = "Breite in mm"
ylab = "Länge in mm"
zlab = "Dicke in mm"

# Assign color to data
data["colors"] = sapply(data$classOfPart, as.character)
data$colors[data$colors=="Vorderachse"]=colors[1]
data$colors[data$colors=="Hinterachse"]=colors[2]

head(data$colors)

pairs(data[,3:5], col=data$colors)

tikz('scatterplotBrakePads.tex', width = 5.75, height = 4)
pairs(data[,3:5],
      col=data$colors)
dev.off()

# Creates a Bravis-Pearson-Correlation Value
# x
# y
# digits
# prefix
panel.cor = function(x, y, digits=2, prefix='', cex.cor, ...)
{
  usr <- par('usr'); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep='')
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * 0.75 * r)
  # text(0.5, 0.5, txt)
}

# Creates a histogram for a given value
# x
panel.hist <- function(x, ...)
{
  usr <- par('usr'); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = 'grey', ...)
}




# Apply logistic regression
t = prepareData(123)
log.r = applyLogisticRegression(t$train, t$test)
epi.tests(log.r$confTable) # Analyse result

# Apply random forest
rf.r = applyRandomForest(t$train, t$test)
rfcT = rf.r$confTable

# Apply knn
knn.r = applyKnn(t$train, t$test, 11)
knn.r$confTable
knn.r$mean




# Create plot
data = prepareData()
length(data$train[,1])

a = data$train[data$train$classOfPart=="Vorderachse",]
length(a[,1])

length(data$completeData[,1])
plotBrakePadData(data$completeData)
data

################################################################################
# Get results
################################################################################
t = prepareData()
results = getMultipleResults(0.48, 35, 1)
results = analyzeResults(results)
################################################################################
# Analyse results
################################################################################
getConfusionBoxPlot(results, 0.48, 35, 1)

head(results)

results.lr = results[results$method=="lr",]
results.rf = results[results$method=="rf",]
results.kn = results[results$method=="kn",]

summary(results.kn[,6:8])
summary(results.lr[,6:8])
summary(results.rf[,6:8])


# Get boxplot of the confusion matrix
getConfusionBoxPlot = function(results, lr.threshold, rf.trees, rf.mtry){
  results.lr = results[results$method=="lr",]
  results.rf = results[results$method=="rf",]
  results.kn = results[results$method=="kn",]
  colors = c("gray90", "gray90", "gray90", "gray90", "gray50", "gray50", "gray50", "gray50", "gray30", "gray30", "gray30", "gray30")
  
  test = cbind(results.kn[,2:5], results.lr[,2:5], results.rf[,2:5])
  head(test)
  # tikz('boxplotsBrakePads.tex', width = 5.75, height = 6)
  boxplot(test, 
          at = c(1,2,3,4, 6,7,8,9, 11,12,13,14), 
          par(mar = c(12, 5, 4, 2) + 0.1),
          col = colors,
          names = c("tp", "fp", "fn", "tn", "tp", "fp", "fn", "tn", "tp", "fp", "fn", "tn"),
          ylim = c(0,23))
  
  legend(2,23.5,
         inset=.02,
         legend=c("kNN", "logistische Regression","Random Forest"),
         fill=c("gray90", "gray50","gray30"), 
         cex=0.8,
         box.lty=0)
  #dev.off()
}

# Analyze the resuts
analyzeResults = function(results){
  accuracy = rep(0, length(results[,1]))
  sensitivity = accuracy
  specifity = accuracy
  
  for(i in 1:length(results[,1])){
    obs = results[i,]
    accuracy[i] = (obs$tp + obs$tn)/(obs$tp + obs$fp + obs$fn + obs$tn)
    sensitivity[i] = (obs$tp)/(obs$tp + obs$fn)
    specifity[i] = obs$tn/(obs$tn + obs$fp)
  }
  
  results["accuracy"] = accuracy
  results["sensitivity"] = sensitivity
  results["specifity"] = specifity
  
  return(results)
}

################################################################################
# Get multiple resutls from different methods
################################################################################
getMultipleResults = function(lr.threshold, rf.trees, rf.mtry){
  df = data.frame()
  for(seed in 1:100){
    df = rbind(df, compareResultOfDifferentMethods(seed, lr.threshold, rf.trees, rf.mtry))
  }
  colnames(df) = c("method", "tp", "fp", "fn", "tn")
  return(df)
}

################################################################################
# Get result from different methods
################################################################################
compareResultOfDifferentMethods = function(seed=123, lr.threshold, rf.trees, rf.mtry){
  
  # Prepare data
  dataSet = prepareData(seed)
  
  # Logistic regression
  lr = getPerfectResult(dataSet$train, dataSet$test, "lr", lr.threshold, rf.trees, rf.mtry)
  
  # kNN
  kn = getPerfectResult(dataSet$train, dataSet$test, "kn", lr.threshold, rf.trees, rf.mtry)
  
  # Random Forest
  rf = getPerfectResult(dataSet$train, dataSet$test, "rf", lr.threshold, rf.trees, rf.mtry)
  
  return(rbind(lr, kn, rf))
}

################################################################################
# Get a converted confusion table from a given method for the given data.
################################################################################
getPerfectResult = function(train, test, method, threshold, rf.trees, rf.mtry){
  if(method=="lr"){
    r = applyLogisticRegression(train, test, threshold)
  }else if(method=="rf"){
    r = applyRandomForest(train, test, rf.trees, rf.mtry)
  }else if(method=="kn"){
    r = applyKnn(train, test, 4)
  }
  ct = r$confTable
  
  return(data.frame(method, ct[1,1], ct[1,2], ct[2,1], ct[2,2]))
}

################################################################################
# Prepare the dataset and return it
################################################################################
prepareData = function(seed=123){
  brakePads = fromJSON("~/data.json", flatten=TRUE)
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
  
  return(list("train"=train, "test"=test, "completeData"=completeData))
}

################################################################################
# Plot the given brake pad dataset
################################################################################
plotBrakePadData = function(data){
  
  # Design of plot
  colors = c("firebrick4", "dodgerblue4")
  xlab = "Breite in mm"
  ylab = "Länge in mm"
  zlab = "Dicke in mm"
  
  # Assign color to data
  data["colors"] = sapply(data$classOfPart, as.character)
  data$colors[data$colors=="Vorderachse"]=colors[1]
  data$colors[data$colors=="Hinterachse"]=colors[2]
  
  # Uncomment tikz and dev.off() to create graphic for latex
  # tikz('dataBrakePads.tex', width = 5.75, height = 3)
  scatterplot3d(data$width, 
                data$length, 
                data$thickness, 
                pch=16, 
                color=data$colors,
                type="h", 
                lty.hplot=3, 
                xlab = xlab,
                ylab = ylab,
                zlab = zlab) 
  # dev.off()
  
  # add a legend
  # legend("topleft", 
  #        inset=.05,      # location and inset
  #        bty="n", 
  #        cex=,              # suppress legend box, shrink text 50%
  #        legend = c("Vorderachse", "Hinterachse"), 
  #        text.col = c(colors[1], colors[2]))
}

################################################################################
# Apply logistic regression to the given data
################################################################################
applyLogisticRegression = function(train, test, threshold){
  
  # Apply logistic regression
  logit.model = glm(classOfPart ~ length + width + thickness, data=train, family=binomial(link="logit"))
  logit.summary = summary(logit.model)
  
  # Test result on testdata
  pred.probs.logit <- predict(logit.model, newdata=test[,3:5], type="response")
  class.pred.logit <- rep("Hinterachse", length(pred.probs.logit))
  class.pred.logit[pred.probs.logit > threshold] <- "Vorderachse"
  logit.table = table(class.pred.logit, test$classOfPart)
  logit.mean = mean(class.pred.logit != test$classOfPart)
  
  return(list("model"=logit.model, "summary"=logit.summary, "confTable"=logit.table, "mean"=logit.mean))
}

################################################################################
# Apply random forest to the given data
################################################################################
applyRandomForest = function(train, test, ntree=10, mtry=1){
  rf.model <- randomForest(classOfPart ~ ., 
                           data=train[,2:5], 
                           ntree=ntree, 
                           mtry=mtry)
  rf.prediction = predict(rf.model, test)
  rf.confTable = confusionMatrix(rf.prediction, test$classOfPart)
  
  return(list("model"=rf.model, "prediction"=rf.prediction, "confTable"=rf.confTable$table))
}

################################################################################
# Apply kNN to the given data
################################################################################
applyKnn = function(train, test, k=4){
  
  knn.prediction = knn(train[,3:5], test[,3:5], train$classOfPart, k=k, prob=TRUE)
  knn.confTable = table(knn.prediction, test$classOfPart)
  knn.mean = mean(knn.prediction == test$classOfPart)
  
  
  return(list("model"=knn.prediction, "confTable"=knn.confTable, "mean"=knn.mean))
}

################################################################################

# Baysian network classifier
tan <- tree.bayes(train[,2:5], training = c('classOfPart'), explanatory = c('length', 'width', 'thickness'))

?tree.bayes


# Cross-validation for random forest
# Random Forest
rf.class <- randomForest(classOfPart ~ ., data=train[,2:5], ntree=500)
p1 = predict(rf.class, train)
confusionMatrix(p1, train$classOfPart)
p2 = predict(rf.class, test)
confusionMatrix(p2, test$classOfPart)
plot(rf.class)
# --> 500 or more trees
t <- tuneRF(train[,3:5], train[,2], stepFactor = 0.25, plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.05)
# --> Varying results, but 4 or 16 might be feasible
hist(treesize(rf.class), main = "No. of Nodes for the Trees", col = "green")

testFunction = function(train, test, ntree, mtry=2){
  rf.class2 <- randomForest(classOfPart ~ ., data=train[,2:5], ntree=ntree, mtry=mtry)
  p3 = predict(rf.class2, test)
  cf = confusionMatrix(p3, test$classOfPart)
  return(cf$overall)
}

for (ntree in c(10,100,250, 500, 750, 1000,2500,5000)){
  for (mtry in c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 16)){
    print(paste(ntree, mtry))
    print(testFunction(train, test, ntree, mtry))
  }
}

# Cross-validation for kNN
for(k in 1:100){
  knn.r = applyKnn(t$train, t$test, k)
  print(knn.r$mean)
}

# Find the best threshold value for logistic regression
for(i in seq(1:20)){
  i = 0.4 + i/100
  t = prepareData()
  results = getMultipleResults(i)
  getConfusionBoxPlot(results,i)
}

# Find the best number of trees/for mtry for random forest
for(i in seq(1, 5, 1)){
  t = prepareData()
  results = getMultipleResults(0.48, 35, i)
  getConfusionBoxPlot(results, 0.48, 35, i)
}
