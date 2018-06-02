
if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("data.table")) install.packages("data.table"); library("data.table") 
if(!require("mlr")) install.packages("mlr"); library("mlr") 
if(!require("xgboost")) install.packages("xgboost"); library("xgboost") 
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap") 



library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

known_test <- read.csv("known_tr_w40t40t20_modeltest.csv", sep=",", header=TRUE)
known_tr <- read.csv("known_test_w40t40t20_modeltest.40.csv", sep=",", header=TRUE)


#########
###xgb###
#########

tr <- known_tr 
ts <-  known_test



task <- makeClassifTask(data = tr, target = "return", positive = "1")
task
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)


xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", par.vals = list("verbose" = 1)) # prediction type needs to be specified for the learner
xgb.learner

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeDiscreteParam("nrounds", values = c(20, 100, 200)), 
  makeDiscreteParam("max_depth", values = c(2, 4)), 
  makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 0.8),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.8)
)

parallelStartSocket(3, level = "mlr.tuneParams")

xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)

parallelStop()
# Extract optimal parameter values after tuning 
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

# Train the model on the full training data (not only a CV-fold)
modelLib[["xgb"]] <- mlr::train(xgb.learner, task = task)

# Make prediction on test data
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = ts)

# Calculate AUC performance on test set 
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)

# Compare RF and GB
auc

### Part 1 combining the bayes optimal and empirical threshold for the cost minimization problem
### calculate the best threshold. If the frequency of item price is larger than 
### 5 in this interval,we pick up the best threshold from the sequence from 0 to
###  1,by 0.01 for each step. Otherwise, we take the optimal bayes threshold.

a <- as.data.frame(yhat[["xgb"]])
ts$pred.probabiliy <- a[3]                    


ts$price_group <- floor(ts$item_price)+1
maxPriceGroup <- ts$price_group[which.max(ts$price_group)]
groupList<- vector(mode="numeric", length = length(maxPriceGroup))
costList = vector(mode="numeric", length = length(maxPriceGroup))
thresholdList = vector(mode="numeric", length = length(maxPriceGroup))

  
for (i in 1:maxPriceGroup){
    tsPriceGroup <- subset(ts, subset = (ts$price_group == i))
    p <- seq(0,1,0.01)
    totalCost <- numeric()
    if (nrow(tsPriceGroup) <= 5){
      groupList[i]<- i
      thresholdList[i] <- (i-1)/(15+1.5*(i-1)) 
      costList[i] <- 0
    }else{
      for(j in seq_along(p)){
        tsPriceGroup$prediction<- ifelse(tsPriceGroup$pred.probabiliy >= p[j], 1, 0)
        tsPriceGroup$cost <- ifelse(tsPriceGroup$return == tsPriceGroup$prediction, 0, ifelse(tsPriceGroup$return == 1, -0.5*5*(3+0.1*tsPriceGroup$item_price),-0.5*tsPriceGroup$item_price))
        totalCost[j] <- sum(tsPriceGroup$cost)
      }
      opt_cutoff <- p[which.max(totalCost)]
      groupList[i]<- i
      thresholdList[i] <- opt_cutoff
      costList[i] <- totalCost[opt_cutoff*100 + 1 ]
  }
}    
print(groupList)
print(costList)
print(thresholdList)
print(sum(costList))
  
thresholdList
### assign each threshold for each price  
ts$threshold <- thresholdList[floor(ts$item_price) +1]


### Part two
### code for balanced the subgroup
### I try to balanced all the subgroup by item_price, which means that in each 
### interval price, the negative and positive sample is the same, but it seems 
### make not sensethe AUC is a little lower and the cost won't decrease, just ### for reference
### It should be used after woe.values of each variable are assigned and before
### The prediction model applied on the training set.

tr$priceInt <- floor(tr$item_price)
maxPriceInt <- tr$priceInt[which.max(tr$priceInt)]
for (i in 0:maxPriceInt){
  subgroup0 <- subset(tr, subset = (tr$priceInt == i & tr$return == 0))
  subgroup1 <- subset(tr, subset = (tr$priceInt == i & tr$return == 1))
  if (nrow(subgroup0)>=nrow(subgroup1)){
    majority_samples <- subgroup0
    minority_samples <- subgroup1
  }else{
    majority_samples <- subgroup1
    minority_samples <- subgroup0
  }
  Difference <- nrow(majority_samples) - nrow(minority_samples)
  addedminority <- minority_samples[sample(1:nrow(minority_samples), size = Difference, replace = TRUE),]
  tr<- rbind.data.frame(tr, addedminority)
}

### naive bayes
a <- as.data.frame(yhat[["xgb"]])
ts$pred.probabiliy <- a[3]
ts$bayesThreshold <- ts$item_price/(15+1.5*ts$item_price)
ts$prediction<- ifelse(ts$pred.probabiliy >= ts$bayesThreshold, 1, 0)
ts$cost <- ifelse(ts$return == ts$prediction, 0, ifelse(ts$return == 1, -0.5*5*(3+0.1*ts$item_price),-0.5*ts$item_price))
sum(ts$cost)
