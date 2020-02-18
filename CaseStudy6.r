#Imports section
library(tm)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(caret)

#This code assumes the jupyter notebook has been executed in accordance with what was provided
#and that the emailDFrp has been stored in the data.Rda file and is in the working directory. 

load("data.rda")

#Note that the emailDFrp has class values (isSpam) in T/F form and not numeric. This is fine for 
#our rpart focused analysis


#####Create a Train / Test Split##########
#Code provided# 

#Seed for reproducibility of split
set.seed(418910)

#Calculate numspam / numham
spam = emailDFrp$isSpam == "T"
numSpam = sum(spam)
numHam = sum(!spam)

#Ensure percentages of in-class and out-of-class remains proportionate
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

#Create Test Dataframe
testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
#Create train dataframe
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])

#Check the numbers in each
sum(trainDF$isSpam == "T") 
sum(trainDF$isSpam == "F")
length(trainDF$isSpam)

sum(testDF$isSpam == "T") 
sum(testDF$isSpam == "F")
length(testDF$isSpam)

#####Baseline (Provided) Model and Parameters######

#Fit the default / baseline model with all features
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class")

#plot the tree created
rpart.plot(rpartFit, extra = 1, cex = .4)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix


######Provided modification of the control parameter cp with a search############

#Possible values to pass to the cp parameter
complexityVals = c(seq(0.00001, 0.0001, length=19),
                   seq(0.0001, 0.001, length=19), 
                   seq(0.001, 0.005, length=9),
                   seq(0.005, 0.01, length=9))

#create multiple fits with these possible values
fits = lapply(complexityVals, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

#Given code creates a plot, but we can just skip to the minimum error value
#Note that this modifies numSpam and numHam
spam = testDF$isSpam == "T"
numSpam = sum(spam)
numHam = sum(!spam)
errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})


#Plot the Type 1 and Type 2 Errors
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ complexityVals, type="l", col=cols[2], 
     lwd = 2,   ylim = c(0,0.2), xlim = c(0,0.01), 
     ylab="Error", xlab="complexity parameter (cp) values", main = "Error Plot over multiple CP Values")
points(errs[2,] ~ complexityVals, type="l", col=cols[1], lwd = 2)

text(x =c(0.003, 0.0035), y = c(0.12, 0.05), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = complexityVals[minI], col ="grey", lty =3, lwd=2)

text(0.0007, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(0.0007, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))

#Find the value of the best cp from these errors
complexityVals[minI]

#Looks like it is 0.0015 (which we could estimate off the plot)

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015))

#plot the tree created
rpart.plot(rpartFit, extra = 1)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix

##########################Begin to search for other parameters###############

##########Start with the minimum split parameter ############################

##########Leverage the same search to find the best minimum split############

##########Although likely to be the smallest possible value##################

#Create a search range of 1 to 30. 
minSplitVals = c(seq(1,30))



fits = lapply(minSplitVals, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=0.0015, minsplit = x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})

#Plot the Type 1 and Type 2 Errors
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ minSplitVals, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,.21), xlim = c(0,30), 
     ylab="Error", xlab="Minimum split (minsplit) values", main = "Error Plot over Multiple Values of minsplit (1:30)")
points(errs[2,] ~ minSplitVals, type="l", col=cols[1], lwd = 2)

text(x =c(1, 1), y = c(0.12, 0.05), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = minSplitVals[minI], col ="grey", lty =3, lwd=2)

text(minSplitVals[minI]+1, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(minSplitVals[minI]+1, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))


#And get the best value for reducing the type 1 error

#Find the value of the best cp from these errors
minSplitVals[minI]

#Looks like it is 23 

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015, minsplit = 23))

#plot the tree created
rpart.plot(rpartFit, extra = 1)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix


##########################Begin to search for other parameters###############

##########Now try adding cross validation of the training data ############################

##########Leverage the same search to find the best number of folds############

##########In theory the more folds the better, but performance may be an issue##################

#Create a search range of 1 to 50. 
numFolds = c(seq(1,50))



fits = lapply(numFolds, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=0.0015, minsplit = 23, xval = x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})

#Plot the Type 1 and Type 2 Errors
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ numFolds, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,.21), xlim = c(0,50), 
     ylab="Error", xlab="Number of Cross Validation Folds", main = "Error Plot using multiple values for xval folding")
points(errs[2,] ~ numFolds, type="l", col=cols[1], lwd = 2)

text(x =c(1, 1), y = c(0.12, 0.05), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = numFolds[minI], col ="grey", lty =3, lwd=2)

text(numFolds[minI]+1, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(numFolds[minI]+1, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))


#And get the best value for reducing the type 1 error

#Find the value of the best cp from these errors
numFolds[minI]

#Huh....no difference. After checking the code above, it appears that the model simply performs the same across all folds. We'll just leave it at the 
#default value of 10. 

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015, minsplit = 23, xval = 10))

#plot the tree created
rpart.plot(rpartFit, extra = 1)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix

###Running Cross Validation did not seem to impact the parameters of the model. This is likely because the model is already well generalized. 


###Change to Information form Gini Index

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", parms = list(split = 'information'), control = rpart.control(cp = 0.0015, minsplit = 23, xval = 20))

#plot the tree created
rpart.plot(rpartFit, extra = 1)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix

####Did not significantly impact the model



############Let's try messing with Max Depth###################


#Default is 30, so let's try something smaller

#Create a search range of 1 to 30. 
depths = c(seq(1,30))



fits = lapply(depths, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=0.0015, minsplit = 23, xval = 10, maxdepth = x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})

#Plot the Type 1 and Type 2 Errors
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ depths, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,.6), xlim = c(0,30), 
     ylab="Error", xlab="maxdepth values", main = "Errors Plotted Across Multiple Values for maxdepth")
points(errs[2,] ~ depths, type="l", col=cols[1], lwd = 2)

text(x =c(1, 1), y = c(0.4, 0.12), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = depths[minI], col ="grey", lty =3, lwd=2)

text(depths[minI]+1, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(depths[minI]+1, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))


#And get the best value for reducing the type 1 error

#Find the value of the best maxdepth from these errors
depths[minI]

#Wow....it looks like 3 is the right value, but that leaves us with high Type 2 error. Maybe that isn't desireable if we can get similar 
#results by using a maxdepth of 10. 

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015, minsplit = 23, xval = 10, maxdepth = 3))

#plot the tree created
rpart.plot(rpartFit, extra = 1, main = "Spam Decision Tree with Max Depth of 3")


#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix

#This really hurt our model performance because of the high type 2 error rate. Let's try picking something that looked more reasonable
#like a max depth of 10 that provided similar Type 1 error, but lower Type 2 error. 

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015, minsplit = 23, xval = 10, maxdepth = 10))

#plot the tree created
rpart.plot(rpartFit, extra = 1)

#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix


############Let's try messing with minbucket###################


#Default is minsplits / 3, so currently 7.6666

#Create a search range of 1 to 30. 
buckets = c(seq(1,30))



fits = lapply(buckets, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=0.0015, minsplit = 23, xval = 10, maxdepth = 10, minbucket = x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})

#Plot the Type 1 and Type 2 Errors
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ depths, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,.6), xlim = c(0,30), 
     ylab="Error", xlab="maxdepth values", main = "Errors Plotted Across Multiple Values for minbucket")
points(errs[2,] ~ depths, type="l", col=cols[1], lwd = 2)

text(x =c(1, 1), y = c(0.4, 0.12), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = depths[minI], col ="grey", lty =3, lwd=2)

text(depths[minI]+1, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(depths[minI]+1, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))


#And get the best value for reducing the type 1 error

#Find the value of the best minbucket from these errors
depths[minI]

#Fit the model with this updated parameter
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class", control = rpart.control(cp = 0.0015, minsplit = 23, xval = 10, maxdepth = 10, minbucket = 9))

#plot the tree created
rpart.plot(rpartFit, extra = 1, main = "Spam Decision Tree with minbucket of 9")


#make predictions on the test set
predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

#Display the confusion matrix for these values
matrix = confusionMatrix(predictions, testDF$isSpam, positive = "T")
matrix
