if(!require("caretEnsemble")) install.packages("caretEnsemble"); library("caretEnsemble")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark")
if(!require("doMC")) install.packages("doMC"); library("doMC")
if(!require("mlbench")) install.packages("mlbench"); library("mlbench")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
#if(!require("extraTrees")) install.packages("extraTrees"); library("extraTrees")
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("rpart")) install.packages("rpart"); library("rpart")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("xgboost")) install.packages("xgboost"); library("xgboost")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")


library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#class <- read.csv("known_test_w40t40t20_modeltest.csv", sep=",", header=TRUE)
known_tr <- read.csv("known_tr_w40t40t20_modeltest.csv", sep=",", header=TRUE)
known_test <- read.csv("known_test_w40t40t20_modeltest.csv", sep=",", header=TRUE)

known_tr = known_tr[,-91]
known_test = known_test[,-91]

#GLM
yhat <- list()
yhat[['lr']]
yhat[['nn']]
yhat[['rf']]
lr <- glm(return ~., data = known_tr, family = binomial(link = "logit"))
pred <- predict (lr, newdata = known_test, type = "response")
h <- HMeasure( true.class = known_test$return, scores = pred, severity.ratio = 0.1)

summary(h)
task <- makeClassifTask(data = known_tr, target = "return", positive = "1")
#RF
rf <- makeLearner("classif.randomForest",
predict.type = "prob", # prediction type needs to be specified for the learner
par.vals = list("replace" = TRUE, "importance" = FALSE))
rf

rf.parms <- makeParamSet(
# The recommendation for mtry by Breiman is squareroot number of columns
makeIntegerParam("mtry", lower = 2, upper = 6), # Number of features selected at each node, smaller -> faster
makeDiscreteParam("sampsize", values = c(300, 200)), # bootstrap sample size, smaller -> faster
makeIntegerParam("ntree", lower = 200, upper = 1000) # Number of tree, smaller -> faster
)

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)
timing <- list()
timing[["simple"]] <- system.time(
tuning <- tuneParams(rf, task = task, resampling = rdesc,
par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)
tuning$x
tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)
tuning_results$data
tapply(tuning_results$data$auc.test.mean, INDEX = c(tuning_results$data$mtry), mean)
rf
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)
rf_tuned
modelLib <- list()
modelLib[["rf"]] <- mlr::train(rf_tuned, task = task)
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = known_test)
str(yhat[["rf"]])

# Calculate AUC performance on test set
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)


#XGB
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
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = known_test)

# Calculate AUC performance on test set
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)


#NN

nn <- nnet(return~., data = known_tr, # the data and formula to be use
trace = FALSE, maxit = 1000, # general options
size = 3, # the number of nodes in the model
decay = 0.001, MaxNWts = 1500)

yhat[["nn"]]
yhat[["nn"]]   <- predict(nn, newdata = known_test , type = "raw") # raw or class

h <- HMeasure(true.class = (as.numeric(ts$return)-1), scores = data.frame(yhat) )
h$metrics["AUC"]



