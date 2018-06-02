
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(!require("rJava")) install.packages("rJava"); library("rJava")
if(!require("caretEnsemble")) install.packages("caretEnsemble"); library("caretEnsemble") 
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") 
if(!require("doMC")) install.packages("doMC"); library("doMC") 
if(!require("mlbench")) install.packages("mlbench"); library("mlbench") 
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
if(!require("extraTrees")) install.packages("extraTrees"); library("extraTrees")
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("rpart")) install.packages("rpart"); library("rpart")
if(!require("data.table")) install.packages("data.table"); library("data.table") 
if(!require("pROC")) install.packages("pROC"); library("pROC") 
if(!require("mlr")) install.packages("mlr"); library("mlr") 
if(!require("randomForest")) install.packages("randomForest"); library("randomForest") 
if(!require("xgboost")) install.packages("xgboost"); library("xgboost")


setwd("/Users/apple/Documents/Ders dost/Master/Business Analytics & Data Science/Final Project")

class <- read.csv("class_fin2.csv", sep=",", header=TRUE)
known_tr <- read.csv("known_tr_fin2.csv", sep=",", header=TRUE)

known_tr$return <- ifelse(known_tr$return==0,"No","Yes")




nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

ctrl  <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE,  savePredictions = "final", 
                      summaryFunction = twoClassSummary, allowParallel = TRUE, returnData = TRUE)

xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2,4),
                         eta = c(0.01, 0.05, 0.1, 0.15),
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)

rf.parms <- expand.grid(mtry = 1:10)



modelList <- list(caretModelSpec(method = "rf", ntree = 80, tuneGrid = rf.parms, metric = "ROC"),
                  caretModelSpec(method = "xgbTree", tuneGrid = xgb.parms, metric = "ROC"))


models <- caretList(return ~ ., 
                    data = known_tr, trControl = ctrl, tuneList = modelList, 
                    continue_on_fail = FALSE)

ens.stack <- caretStack(models, method='glm')

ens.stack.pred <- predict(ens.stack, newdata = class, type = "prob")


stopCluster(cl)
