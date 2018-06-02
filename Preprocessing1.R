
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


prepare <- function(x,y) {
  if(!require("forcats")) install.packages("forcats"); library("forcats") 
 
#Combine known and class as to calculate frequencies accurately.
  return <- as.factor(x$return)
  total <- rbind(known[,-14], y)

  total$user.reg.time <- as.numeric(as.Date("2017-01-01", format= "%Y-%m-%d") - as.Date(total$user_reg_date, format= "%Y-%m-%d"))
                        total$user.reg.time[is.na(total$user.reg.time)] <- median(total$user.reg.time, na.rm=T)

  total$user_dob <- as.Date(total$user_dob, format= "%Y-%m-%d")
                        total$user.age <- as.numeric(as.Date("2017-01-01", format= "%Y-%m-%d") - as.Date(total$user_dob,format= "%Y-%m-%d"))
                        total$user.age[is.na(total$user.age)] <- median(total$user.age, na.rm=T)
                        
                        total$user_dob[as.Date(known$user_dob, format= "%Y-%m-%d") > as.Date("2000-11-18",format= "%Y-%m-%d") ] <- as.Date("2011-01-01",format= "%Y-%m-%d")
                        total$user_dob[as.Date(known$user_dob, format= "%Y-%m-%d") < as.Date("1930-11-23",format= "%Y-%m-%d") ] <- as.Date("1900-01-01",format= "%Y-%m-%d")
                        total$user_dob <- as.factor(total$user_dob)
                        total$user_dob <- fct_explicit_na(total$user_dob,"?")
                        
  total$delivery_date <- as.Date(total$delivery_date,format= "%Y-%m-%d")
                        total$order_date <- as.Date(total$order_date,format= "%Y-%m-%d")
                        total$delivery.time <- as.numeric(total$delivery_date - total$order_date)
                        total$delivery.time[is.na(total$delivery.time)] <- max(total$delivery.time, na.rm=T)
                        
                        total$delivery_date[total$delivery_date < total$order_date ] <- as.Date("1900-01-01",format= "%Y-%m-%d")
                        total$delivery_date <- as.factor(total$delivery_date)
                        total$delivery_date <- fct_explicit_na(total$delivery_date,"?")

# Calculating counts of each factor variable                        
  for(i in c("order_date","delivery_date","item_id","item_size",
             "item_color","brand_id","user_id","user_dob","user_reg_date")) {
                                      total[,i] <- as.factor(total[,i])
                                      total[,paste("f",i, sep=".")] <- ave(as.numeric(total[,i]),total[,i],FUN=length)
  }    

                            
known <<- subset(total, order_item_id < 100001)
known <<- cbind(known, return)
class <<- subset(total, order_item_id > 100000)
                        
                        }


