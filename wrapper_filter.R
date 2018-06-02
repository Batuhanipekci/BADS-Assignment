if(!require("Boruta")) install.packages("Boruta"); library("Boruta")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("klaR")) install.packages("klaR"); library("klaR")


library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

known_tr <- read.csv("known_tr_nodummies.csv", sep=",", header=TRUE)
known_test <- read.csv("known_test_nodummies.csv", sep=",", header=TRUE)
sapply(known_tr,class)


fisherScore <- function(feature, targetVariable){
  classMeans <- tapply(feature, targetVariable, mean)
  classStds <- tapply(feature, targetVariable, sd)
  classDiff <- abs(diff(classMeans))
  score <- as.numeric(classDiff / sqrt(sum(classStds^2)))
  return(score)
}

fisher_scores <- apply(known_tr[,sapply(known_tr, is.numeric)], 
                       2, fisherScore, known_tr$return)

sort(fisher_scores)

known_tr$return <- as.factor(known_tr$return)
woe.object <- woe(return ~ ., data = known_tr, zeroadj = 0.5)
woe.object$IV


# Choose: return, item_price.cat,woe.user_reg_date.cat,woe.user_dob.cat,
# woe.user_id.cat,woe.brand_id.cat,woe.item_color.cat,woe.item_size.cat,
# woe.item_id.cat,woe.delivery_date.cat,woe.order_date.cat,woe.delivery_date,
# woe.item_id,woe.user_id,woe.brand_id,item_price.st,woe.item_size,
# woe.user_dob, woe.item_color

known_tr <- known_tr[,c("return", "item_price.cat","woe.user_reg_date.cat","woe.user_dob.cat",
                        "woe.user_id.cat","woe.brand_id.cat","woe.item_color.cat","woe.item_size.cat",
                        "woe.item_id.cat","woe.delivery_date.cat","woe.order_date.cat","woe.delivery_date",
                         "woe.item_id","woe.user_id","woe.brand_id","item_price.st","woe.item_size",
                        "woe.user_dob", "woe.item_color")]

Boruta.vs <- Boruta(return~., data=known_tr,doTrace=2,ntree=500)
finalvars = getSelectedAttributes(Boruta.vs, withTentative = F)

plot(Boruta.vs)

#We observed that variables which are selected by the filters Fisher Score, Information Value
#and wrapper Boruta do not result in higher AUC score. Therefore, we used in model development
#all the variables except very highly correlated ones. See in report.

known_tr$return <- as.numeric(known_tr$return)
dmy_tr <- dummyVars(" ~ .", data = known_tr, fullRank = TRUE)
known_tr <- data.frame(predict(dmy_tr, newdata = known_tr, replace=FALSE))
known_tr$return <- as.factor(known_tr$return)

known_test$return <- as.numeric(known_test$return)
dmy_test <- dummyVars(" ~ .", data = known_test, fullRank = TRUE)
known_test <- data.frame(predict(dmy_test, newdata = known_test, replace=FALSE))
known_test$return <- as.factor(known_test$return)

 
