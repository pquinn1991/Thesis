##### Quick models with data from Model1Data.R #####

### Logistic Regression (no registration data) ###
logTrain <- subset(modelDataAll, year == "2006" | year == "2008" | year == "2010" | year == "2012")
logTest <- subset(modelDataAll, year == "2012" | year == "2016")

logistic <- glm(IncWin ~ PVIeffect + midtermEffect + IncContDiff + SabatoScore + PrevElectionIncumbParty. + Delta.unemployment.Rate + thirdParty + PercentDelta.median.household.income + logIncCont + Open, data = logTrain, family = binomial)
summary(logistic)

logPred <- predict(logistic, newdata = logTest, type = "response")
logFitted <- ifelse(logPred > 0.5,1,0)
logError <- mean(na.omit(logFitted != logTest$IncWin))
print(paste('Accuracy',1-logError))

table(logPred >= 0.5, logTest$IncWin == 1)

### Logistic Regression (including registration data) ###
testFraction = 0.25
set.seed(123)

logTrain2 <- sample(nrow(modelDataReg),size=(1-testFraction)*nrow(modelDataReg), replace=FALSE)
logistic2 <- glm(IncWin ~ PVIeffect + IncReg + IncContDiff, data = modelDataReg, subset = logTrain2, family = binomial)

logPred2 <- predict(logistic2, newdata = modelDataReg[-logTrain2,], type = "response")
logFitted2 <- ifelse(logPred2 > 0.5,1,0)
logError2 <- mean(logFitted2 != modelDataReg[-logTrain2,]$IncWin)
print(paste('Accuracy',1-logError2))

table(logPred2 >= 0.5, modelDataReg[-logTrain2,"IncWin"] == 1)

### Multiple Linear Regression (no registration) ###
lmTrain <- subset(modelDataNoReg, year == "2006" | year == "2008" | year == "2010" | year == "2012")[,-12]
lmTest <- subset(modelDataNoReg, year == "2014" | year == "2016")[,-12]

lm <- lm(IncPct2 ~ . -IncWin -INC, data = lmTrain)
summary(lm)
par(mfrow = c(2,2))
plot(lm)
par(mfrow = c(1,1))
plot(fitted(lm) + residuals(lm), fitted(lm))
abline(0,1)

lmPred <- predict(lm, newdata = lmTest)

table(lmPred >= 0.5, lmTest$IncPct2 >= 0.5)
hist(lmPred - lmTest$IncPct2, breaks = 10)

library(MASS)
stepAIC(lm, direction = "forward")

### Multiple Linear Regression (including registration) ###
testFraction = 0.25
lmTrain2 <- sample(nrow(modelDataReg),size=(1-testFraction)*nrow(modelDataReg), replace=FALSE)
lm2 <- lm(IncPct2 ~ . -IncWin, data = modelDataReg[,-13], subset = lmTrain2)
summary(lm2)
par(mfrow = c(2,2))
plot(lm2)
par(mfrow = c(1,1))
plot(fitted(lm2) + residuals(lm2), fitted(lm2))
abline(0,1)

lmPred2 <- predict(lm2, newdata = modelDataReg[-lmTrain2,])

table(lmPred2 >= 0.5, modelDataReg[-lmTrain2,"IncPct2"] >= 0.5)
hist(lmPred2 - modelDataReg[-lmTrain2,"IncPct2"], breaks = 40)

stepAIC(lm2, direction = "backward")

### K-nearest-neighbors ###

knnData <- modelDataNoReg[,c(2,4:14)]
knnData$midtermEffect <- as.numeric(knnData$midtermEffect)
knnData$thirdParty <- as.numeric(knnData$thirdParty)
knnData$IncWin <- as.numeric(knnData$IncWin) - 1
knnData$Open <- as.numeric(knnData$Open)
knnData <- na.omit(knnData)

knnDataNorm <- cbind(as.data.frame(scale(knnData[,-c(1,10)])), year = knnData[,10], IncWin = knnData[,1])

knnTrain <- subset(knnDataNorm, year == "2006" | year == "2008" | year == "2010" | year == "2012")[,1:10]
knnTest <- subset(knnDataNorm, year == "2014" | year == "2016")[,1:10]
knnTrainLabels <- subset(knnDataNorm, year == "2006" | year == "2008" | year == "2010" | year == "2012")[,12]
knnTestLabels <- subset(knnDataNorm, year == "2014" | year == "2016")[,12]

library(class)
knnPred <- knn(train = knnTrain, test = knnTest, cl = knnTrainLabels, k = 7)
knnTestLabels <- data.frame(knnTestLabels)
knnMerge <- data.frame(knnPred, knnTestLabels)
table(knnMerge$knnPred, knnMerge$knnTestLabels)


### Random Forest ###
library(randomForest)

rfData <- modelData2
rfData <- na.omit(rfData) # take care of missing values in excel doc?
#rfData <- subset(rfData, IncPct2 < 1)
#rfData$PrevIncPctDiff <- rfData$PrevElectionIncumbParty. - .5
rfTrain <- subset(rfData, year == "2006" | year == "2008" | year == "2010" | year == "2012")
rfTest <- subset(rfData, year == "2016" | year == "2014")

#If predicting IncPct
rf <- randomForest(IncPct2 ~ .-IncWin -year, data = rfTrain, mtry = 15, importance = TRUE, ntree = 1000)
yhat.rf <- predict(rf, newdata = rfTest)

par(pty = "s")
plot(yhat.rf, rfTest$IncPct2, main = "Predicted versus actual", xlab = "Predicted Incumbent Vote Share", ylab = "Actual Incumbent Vote Share", xlim = c(.3,1), ylim = c(.3,1))
abline(0,1)
mean((yhat.rf - rfTest$IncPct2)^2)
mean(abs(yhat.rf - rfTest$IncPct2))
importance(rf)
par(pty = "m")
varImpPlot(rf, n.var = 15, main = "Random Forest (VS) Variable Importance")

table(yhat.rf >= 0.5, rfTest$IncPct2 >= 0.5)

hist(yhat.rf - rfTest$IncPct2, breaks = 24, main = "Histogram of errors (predicted - actual)", xlab = "Error", xlim = c(-.15,.15))

rfCheck <- cbind(rfTest, yhat.rf)
rfCheck <- subset(rfCheck, yhat.rf < .5 & rfTest$IncPct2 >= 0.5 | yhat.rf >= .5 & rfTest$IncPct2 < 0.5)

# If predicting incWin
rf2 <- randomForest(IncWin ~ .-IncPct2, data = rfTrain, mtry = 15, importance = TRUE, ntree = 1000)
yhat.rf2 <- predict(rf2, newdata = rfTest)

importance(rf2)
varImpPlot(rf2)

table(yhat.rf2,rfTest$IncWin)

rfCheck2 <- cbind(rfTest, yhat.rf2)
rfCheck2 <- rfCheck2[rfCheck2$IncWin != rfCheck2$yhat.rf2,]

yhat.rf2 <- predict(rf2, newdata = rfTest, type = "prob")
hist(yhat.rf2[,2])
par(pty = "s")
plot(yhat.rf2[,2], rfTest$IncPct2)

rf2Inspect <- cbind(IncProb = yhat.rf2[,2], IncPct2 = rfTest$IncPct2, IncWin = as.numeric(rfTest$IncWin) - 1)
write.csv(rf2Inspect, "rf2Inspect.csv", row.names = FALSE)

### Tree for IncPct ###
library(tree)
treeTrain <- subset(modelDataNoReg, year == "2006" | year == "2008" | year == "2010" | year == "2012")
treeTest <- subset(modelDataNoReg, year == "2014" | year == "2016")

tree <- tree(IncPct2 ~ . -IncWin, data = treeTrain)
summary(tree)
par(mfrow = c(1,1))
plot(tree)
text(tree, pretty = 0)

yhat.tree <- predict(tree, newdata = treeTest)
plot(yhat.tree, treeTest[,"IncPct2"])
abline(0,1)
mean((yhat.tree - treeTest[,"IncPct2"])^2)
mean(abs(yhat.tree - treeTest[,"IncPct2"]))
hist(yhat.tree - treeTest[,"IncPct2"])
table(yhat.tree >= .5, treeTest$IncWin)

#Try cv
cv.tree <- cv.tree(tree)
plot(cv.tree$size, cv.tree$dev, type = 'b')

prune.tree <- prune.tree(tree, best = 6)
plot(prune.tree)
text(prune.tree, pretty = 0)
summary(prune.tree)

yhat.tree2 <- predict(prune.tree, newdata = treeTest)
mean(abs(yhat.tree2 - treeTest[,"IncPct2"]))
hist(yhat.tree2 - treeTest[,"IncPct2"])
table(yhat.tree2 >= .5, treeTest$IncWin)

### Tree for IncWin ###
treeTrain2 <- subset(modelDataNoReg, year == "2006" | year == "2008" | year == "2010" | year == "2012")
treeTest2 <- subset(modelDataNoReg, year == "2014" | year == "2016")

tree2 <- tree(IncWin ~ . -IncPct2, data = treeTrain2)
summary(tree2)
par(mfrow = c(1,1))
plot(tree2)
text(tree2, pretty = 0)

predTree <- predict(tree2, newdata = treeTest2, type = "class")
table(predTree, treeTest2$IncWin)

### Boosting for tree ###
library(gbm)
set.seed(123)

# IncPct #
boost <- gbm(IncPct2 ~ ., data = treeTrain[,-2], distribution = "gaussian", n.trees = 5000, interaction.depth = 3, cv.folds = 5, verbose = FALSE)
summary(boost)

best.iter <- gbm.perf(boost, method = "cv")

yhat.boost <- predict(boost, newdata = treeTest, n.trees = 5000)
mean((yhat.boost - treeTest$IncPct2)^2)
mean(abs(yhat.boost - treeTest$IncPct2))
hist(yhat.boost - treeTest$IncPct2, breaks = 10)
table(yhat.boost >= 0.5, treeTest$IncPct2 >= 0.5)

# IncWin #
treeTrain$IncWin <- as.numeric(treeTrain$IncWin)-1
boost2 <- gbm(IncWin ~ ., data = treeTrain[,-1], distribution = "bernoulli", n.trees = 3000, interaction.depth = 3, cv.folds = 5)
summary(boost2)

best.iter2 <- gbm.perf(boost2, method = "cv")

yhat.boost2 <- predict(boost2, newdata = treeTest, n.trees = 3000, type = "response")
table(round(yhat.boost2), treeTest$IncWin)

### fastAdaboost ###
library(fastAdaboost)

adaBoost <- adaboost(IncWin ~ . -IncPct2, data = treeTrain, nIter = 500)

adaPred <- predict(adaBoost, newdata = treeTest)
print(adaPred$error)
table(adaPred$class, treeTest$IncWin)

### Naive Bayes ###
library(e1071)

# No registration data
nbTrain <- subset(modelDataNoReg, year == "2006" | year == "2008" | year == "2010" | year == "2012")
nbTest <- subset(modelDataNoReg, year == "2014" | year == "2016")

nb <- naiveBayes(IncWin ~ . -IncPct2, data = nbTrain)
nb
summary(nb)

nbPred <- predict(nb, nbTest[,-c(1,2,12)])
table(pred = nbPred, true = nbTest$IncWin)

# Registration data
testFraction <- 0.25
nbTrain2 <- sample(nrow(modelDataReg),size=(1-testFraction)*nrow(modelDataReg), replace=FALSE)

nb2 <- naiveBayes(IncWin ~ . -IncPct2 -year, data = modelDataReg, subset = nbTrain2)
summary(nb2)

nbPred2 <- predict(nb2, modelDataReg[-nbTrain2,-c(1,12)])
table(pred = nbPred2, true = modelDataReg[-nbTrain2, "IncWin"])


### PCR ###
library(pls)

pcrTrain <- na.omit(subset(modelData2, year == "2006" | year == "2008" | year == "2010" | year == "2012"))
pcrTest <- na.omit(subset(modelData2, year == "2014" | year == "2016"))

pcr <- pcr(IncPct2 ~ . -IncWin -year, data = pcrTrain, scale = TRUE, validation = "CV")
summary(pcr)
validationplot(pcr, val.type = "MSEP")
PCRpred <- predict(pcr, pcrTest, ncomp = 11)
PCRerror <- mean((PCRpred - pcrTest$IncPct2)^2)
PCRerror
hist(PCRpred - pcrTest$IncPct2, breaks = 20)
mean(abs(PCRpred - pcrTest$IncPct2))

table(PCRpred >= 0.5, pcrTest$IncPct2 >= 0.5)

### Shrinkage Models (Lasso) ###
library(glmnet)
lassoData <- na.omit(modelData2)
lassoTrain <- subset(lassoData, year == "2006" | year == "2008" | year == "2010" | year == "2012")
lassoTest <- subset(lassoData, year == "2014" | year == "2016")

lassoX <- model.matrix(IncPct2 ~ . -IncWin -year,data=lassoTrain)[,-1]
lassoY <- lassoTrain$IncPct2
lassoXtest <- model.matrix(IncPct2 ~ . -IncWin -year,data=lassoTest)[,-1]
lassoYtest <- lassoTest$IncPct2

set.seed(123)

grid <- 10^seq(10,-2, length = 100)

lasso.mod <- glmnet(lassoX,lassoY,alpha = 1, lambda = grid)
plot(lasso.mod)

cvLasso <- cv.glmnet(lassoX,lassoY,alpha = 1)
plot(cvLasso)

bestlam <- cvLasso$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam ,newx=lassoXtest)

mean((lasso.pred - lassoYtest)^2)
mean(abs(lasso.pred - lassoYtest))

hist(lasso.pred - lassoYtest, breaks = 10)

plot(lasso.pred, lassoYtest)
abline(0,1)

table(lasso.pred >= 0.5, lassoYtest >= 0.5)


### SVM ###
library(e1071)

svmTrain <- subset(modelDataNoReg, year == "2006" | year == "2008" | year == "2010" | year == "2012")
svmTest <- subset(modelDataNoReg, year == "2014" | year == "2016")

svmModel <- svm(IncWin ~ . -IncPct2 -year, data = svmTrain, kernel = "linear", cost = 10, scale = FALSE)
summary(svmModel)

svmPred <- predict(svmModel, svmTest)
table(svmPred, na.omit(svmTest)$IncWin)

svmTune <- tune(svm, IncWin ~ . -IncPct2 -year, data = svmTrain, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(svmTune)

svmBest <- svmTune$best.model
summary(svmBest)

svmPred2 <- predict(svmBest, svmTest)
table(svmPred2, na.omit(svmTest)$IncWin)

#### QUESTIONS #####

# Use registration data for some of the models?
# Try to include ideology? Need to fix equation. Correlated with PVIeffect.

# Still need to fix up tuning parameters?

# MODELING: subset by Sabato score? Predict dPct?
# Reduce/increase the number of variables included in models? RF uses lots of similar variables

### SUPER LEARNER package ###
## Weighted combination of predictions from several algorithms ##
# Data prep
# https://cran.r-project.org/web/packages/SuperLearner/vignettes/SuperLearnerPresent.pdf
# https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html
# Look into printing latex tables (requires Hmisc package) -- latex(summary(...))
# Look into creating a grid of tuning parameters for the algortihms that are included
library(SuperLearner)
SLdata <- na.omit(modelDataNoReg)
SLdata <- subset(SLdata, IncPct2 < 1)
SLdata$thirdParty <- as.numeric(SLdata$thirdParty) - 1
SLdata$midtermEffect <- as.numeric(SLdata$midtermEffect) - 2
SLdata$IncWin <- as.numeric(SLdata$IncWin) - 1
SLdata$Open <- as.numeric(SLdata$Open)
## How does including Sabato Scores affect accuracy versus not including?
SLtrain <- subset(SLdata, year == "2006" | year == "2008" | year == "2010" | year == "2012")[,-c(3,12)]
SLtest <- subset(SLdata, year == "2014" | year == "2016")[,-c(3,12)]

# Which models?
SL.library <- c("SL.nnet", "SL.glm", "SL.randomForest", "SL.lm", "SL.svm", "SL.xgboost")
SL.library <- c("SL.nnet", "SL.glm", "SL.randomForest", "SL.lm")

# Fit models for IncPct
fit.data.SL <- SuperLearner(Y = SLtrain[,1], X = SLtrain[,c(3:12)], SL.library = SL.library, verbose = TRUE, family = gaussian(), newX = SLtest[,c(3:12)])
fit.data.SL

pred <- predict(fit.data.SL, SLtest[,c(3:12)], onlySL = T)
par(pty = "s")
plot(pred$pred, SLtest$IncPct2, xlim = c(.3,1), ylim = c(.3,1), main = "Predicted versus actual", xlab = "Predicted Incumbent Vote Share", ylab = "Actual Incumbent Vote Share", cex.lab = 1.25)
abline(0,1)
table(pred$pred >= .5, SLtest$IncPct2 >= .5)
hist(pred$pred - SLtest$IncPct2, breaks = 16, main = "Histogram of errors (predicted - actual)", xlab = "Error", cex.lab = 1.25)
mean(abs(pred$pred - SLtest$IncPct2))

predAct <- data.frame(pred = pred$pred[,1], act = SLtest$IncPct2)
predAct$diff <- predAct$pred - predAct$act
mean(predAct$diff^2)
predAct[(predAct$pred >= .5 & predAct$act <= 0.5) | (predAct$pred <= .5 & predAct$act >= 0.5),]$diff

# Fit models for IncWin
fit.data.SL2 <- SuperLearner(Y = SLtrain[,2], X = SLtrain[,c(3:12)], SL.library = SL.library, verbose = TRUE, family = binomial())
fit.data.SL2

pred2 <- predict(fit.data.SL2, SLtest[,c(3:12)], onlySL = T)
table(pred2$pred >= .5, SLtest$IncWin)

## How does including Sabato Scores affect accuracy versus not including? ##


#### Try causal forest
library(grf)

cfData <- cbind(modelData2, modelData$ID)
cfData <- na.omit(cfData)
cfData <- subset(cfData, IncPct2 < 1)# take care of missing values in excel doc?
cfData$midtermEffect <- as.numeric(cfData$midtermEffect) - 2
cfData$Midterm. <- as.numeric(cfData$Midterm.) - 1
cfData$Pres_Incumbent_SameParty <- as.numeric(cfData$Pres_Incumbent_SameParty) - 1
#cfData$PrevIncPctDiff <- cfData$PrevElectionIncumbParty. - .5
cfTrain <- subset(cfData, year == "2006" | year == "2008" | year == "2010" | year == "2012")
cfTest <- subset(cfData, year == "2016" | year == "2014")

cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 50, 54, 56)], cfTrain[,54], cfTrain[,50], mtry = 10, num.trees = 1000)
cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 53, 54, 56)], cfTrain[,54], cfTrain[,53], mtry = 10, num.trees = 1000)
cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 42, 54, 56)], cfTrain[,54], (abs(cfTrain[,42])*100), mtry = 10, num.trees = 1000)
cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 38, 54, 56)], cfTrain[,54], cfTrain[,38], mtry = 10, num.trees = 1000)
cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 52, 54, 56)], cfTrain[,54], cfTrain[,52], mtry = 10, num.trees = 1000)

cfPred <- predict(cf, cfData[,-c(1,2,11,22,43,50,54,56)])
plot(cfTest[,50], cfPred$predictions, main = "Treatment effects", xlab = "Challenger expenditures (log)", ylab = "Treatment effect", xlim = c(10,18))
lines(lowess(cfTest[,50], cfPred$predictions))
hist(cfPred$predictions, main = "District PVI effect", xlab = "Treatment effect")
average_partial_effect(cf)
estimate_average_effect(cf)

ggplot(data = as.data.frame(cfPred$predictions), aes(x = V1)) + geom_histogram(bins = 60)
plot(cf$Y.orig, cf$Y.hat, main = "Estimated versus actual incumbent expenditures", xlab = "Actual", ylab = "Estimated")
abline(0,1)
table(cf$Y.orig >= 0.5, cf$Y.hat >= 0.5)

variable_importance(cf)

plot(rf$predicted, cf$Y.hat, main = "Comparison of predicted vote share", xlab = "Random forest", ylab = "Causal forest")
abline(0,1)

cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 50, 54, 56)], cfTrain[,54], cfTrain[,50], mtry = 10, num.trees = 1000)
cfPred2 <- predict(cf, cfData[,-c(1,2,11,22,43,50,54,56)])
cfDataCounterFactuals <- cbind(cfData, logIncContPred = cfPred2$predictions)

cf <- causal_forest(cfTrain[,-c(1, 2, 11, 22, 43, 53, 54, 56)], cfTrain[,54], cfTrain[,53], mtry = 10, num.trees = 1000)
cfPred3 <- predict(cf, cfData[,-c(1,2,11,22,43,53,54,56)])
cfDataCounterFactuals <- cbind(cfDataCounterFactuals, logNincContPred = cfPred3$predictions)

write.csv(cfDataCounterFactuals, "cfDataCounterFactualsNew.csv", row.names = FALSE)

###### Examples #####
n = 50; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.5)
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)

X.test = matrix(0, 101, p)
X.test[,1] = seq(-2, 2, length.out = 101)

c.forest = causal_forest(X, Y, W)
c.pred = predict(c.forest, X.test)
plot(X.test[,1], c.pred$predictions)

# Generate data.
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
X.test = matrix(0, 101, p)
X.test[,1] = seq(-2, 2, length.out = 101)

# Perform treatment effect estimation.
W = rnorm(n)  # the treatment is continuous
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
tau.forest = causal_forest(X, Y, W)
tau.hat = predict(tau.forest, X.test)
plot(X.test[,1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[,1], pmax(0, X.test[,1]), col = 2, lty = 2)
