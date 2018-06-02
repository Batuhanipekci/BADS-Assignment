if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("klaR")) install.packages("klaR"); library("klaR") 


library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
known <- read.csv("BADS_WS1718_known.csv", sep=",", header=TRUE)
class <- read.csv("BADS_WS1718_class_20180115.csv", sep=",", header=TRUE)

#Dealing with NA's and calculating frequencies
source("Preprocessing1.R")
prepare(known, class)

#1)This below is for the preparation of the class set

#set.seed(123)
#idx.train.woe <- createDataPartition(y = known$return, p = 0.4, list = FALSE)
#known_tr <- known[idx.train.woe, ] 
#woe.set <-  known[-idx.train.woe, ]
#known_test <- class

#2) Split as below when developing models on only known set
set.seed(124)

idx.train.woe <- createDataPartition(y = known$return, p = 0.6, list = FALSE)
known_total_tr <- known[idx.train.woe, ] 
woe.set <-  known[-idx.train.woe, ]

idx.test.woe <- createDataPartition(y = known_total_tr$return, p = 0.3333, list = FALSE)
known_test <- known_total_tr[idx.test.woe, ] 
known_tr <-  known_total_tr[-idx.test.woe, ]

woe.object <- woe(return ~., data = woe.set, zeroadj = 0.5)
woe.set <- predict(woe.object, newdata = woe.set, replace = F)

source("Preprocessing2.R")
Preprocessing(woe.set,known_tr,known_test)

#Dummy Encoding

known_tr$return <- (as.numeric(known_tr$return)-1)
dmy_tr <- dummyVars(" ~ .", data = known_tr, fullRank = TRUE)
known_tr <- data.frame(predict(dmy_tr, newdata = known_tr, replace=FALSE))
known_tr$return <- as.factor(known_tr$return)

if("return" %in% colnames(known_test)){
    known_test$return <- (as.numeric(known_test$return)-1)
}
dmy_test <- dummyVars(" ~ .", data = known_test, fullRank = TRUE)
known_test <- data.frame(predict(dmy_test, newdata = known_test, replace=FALSE))
if("return" %in% colnames(known_test)){
    known_test$return <- as.factor(known_test$return)
}

woe.set$return <- (as.numeric(woe.set$return)-1)
dmy_woe <- dummyVars(" ~ .", data = woe.set, fullRank = TRUE)
woe.set <- data.frame(predict(dmy_woe, newdata = woe.set, replace=FALSE))
woe.set$return <- as.factor(woe.set$return)

#We wrote training and test sets as: known_tr_w40t40t20_modeltest and known_test_w40t40t20_modeltest
#We wrote known and class sets as: known_tr_fin2 and class_fin2
