
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Preprocessing <- function(woe.set, known_tr, known_test){
  if(!require("devtools")) install.packages("devtools") ; library(devtools)
  if(!require("woe")) install_github("riv","tomasgreif") ; library(woe)
  if(!require("rpart")) install.packages("rpart"); library("rpart") 
  if(!require("data.table")) install.packages("data.table"); library("data.table") 
  
  migrate <- function(x,y,z){
    
    setDT(x, key='order_date')
    x$woe.order_date <- ifelse(x$f.order_date<2, 0,x$woe.order_date)
    
    setDT(y, key='order_date')
    y[x, woe.order_date := i.woe.order_date]
    y$woe.order_date[is.na(y$woe.order_date)] <- mean(y$woe.order_date, na.rm=TRUE)
    y$woe.order_date <- ifelse(y$f.order_date<2, 0,y$woe.order_date)
    
    setDT(z, key='order_date')
    z[x, woe.order_date := i.woe.order_date]
    z$woe.order_date[is.na(z$woe.order_date)] <- mean(z$woe.order_date, na.rm=TRUE)
    z$woe.order_date <- ifelse(z$f.order_date<2, 0,z$woe.order_date)
    
    setDT(x, key='delivery_date')
    x$woe.delivery_date <- ifelse(x$f.delivery_date<2, 0,x$woe.delivery_date)
    
    setDT(y, key='delivery_date')
    y[x, woe.delivery_date := i.woe.delivery_date]
    y$woe.delivery_date[is.na(y$woe.delivery_date)] <- mean(y$woe.delivery_date, na.rm=TRUE)
    y$woe.delivery_date <- ifelse(y$f.delivery_date<2, 0,y$woe.delivery_date)
    
    setDT(z, key='delivery_date')
    z[x, woe.delivery_date := i.woe.delivery_date]
    z$woe.delivery_date[is.na(z$woe.delivery_date)] <- mean(z$woe.delivery_date, na.rm=TRUE)
    z$woe.delivery_date <- ifelse(z$f.delivery_date<2, 0,z$woe.delivery_date)
    
    
    setDT(x, key='item_id')
    x$woe.item_id <- ifelse(x$f.item_id<2, 0,x$woe.item_id)
    
    setDT(y, key='item_id')
    y[x, woe.item_id := i.woe.item_id]
    y$woe.item_id[is.na(y$woe.item_id)] <- mean(y$woe.item_id, na.rm=TRUE)
    y$woe.item_id <- ifelse(y$f.item_id<2, 0,y$woe.item_id)
    
    setDT(z, key='item_id')
    z[x, woe.item_id := i.woe.item_id]
    z$woe.item_id[is.na(z$woe.item_id)] <- mean(z$woe.item_id, na.rm=TRUE)
    z$woe.item_id <- ifelse(z$f.item_id<2, 0,z$woe.item_id)
    
    setDT(x, key='item_size')
    x$woe.item_size <- ifelse(x$f.item_size<2, 0,x$woe.item_size)
    
    setDT(y, key='item_size')
    y[x, woe.item_size := i.woe.item_size]
    y$woe.item_size[is.na(y$woe.item_size)] <- mean(y$woe.item_size, na.rm=TRUE)
    y$woe.item_size <- ifelse(y$f.item_size<2, 0,y$woe.item_size)
    
    setDT(z, key='item_size')
    z[x, woe.item_size := i.woe.item_size]
    z$woe.item_size[is.na(z$woe.item_size)] <- mean(z$woe.item_size, na.rm=TRUE)
    z$woe.item_size <- ifelse(z$f.item_size<2, 0,z$woe.item_size)
    
    
    
    setDT(x, key='item_color')
    x$woe.item_color <- ifelse(x$f.item_color<2, 0,x$woe.item_color)
    setDT(y, key='item_color')
    y[x, woe.item_color := i.woe.item_color]
    y$woe.item_color[is.na(y$woe.item_color)] <- mean(y$woe.item_color, na.rm=TRUE)
    y$woe.item_color <- ifelse(y$f.item_color<2, 0,y$woe.item_color)
    
    setDT(z, key='item_color')
    z[x, woe.item_color := i.woe.item_color]
    z$woe.item_color[is.na(z$woe.item_color)] <- mean(z$woe.item_color, na.rm=TRUE)
    z$woe.item_color <- ifelse(z$f.item_color<2, 0,z$woe.item_color)
    
    
    setDT(x, key='brand_id')
    x$woe.brand_id <- ifelse(x$f.brand_id<2, 0,x$woe.brand_id)
    setDT(y, key='brand_id')
    y[x, woe.brand_id := i.woe.brand_id]
    y$woe.brand_id[is.na(y$woe.brand_id)] <- mean(y$woe.brand_id, na.rm=TRUE)
    y$woe.brand_id <- ifelse(y$f.brand_id<2, 0,y$woe.brand_id)
    
    setDT(z, key='brand_id')
    z[x, woe.brand_id := i.woe.brand_id]
    z$woe.brand_id[is.na(z$woe.brand_id)] <- mean(z$woe.brand_id, na.rm=TRUE)
    z$woe.brand_id <- ifelse(z$f.brand_id<2, 0,z$woe.brand_id)
    
    
    setDT(x, key='user_id')
    setDT(y, key='user_id')
    y[x, woe.user_id := i.woe.user_id]
    y$woe.user_id[is.na(y$woe.user_id)] <- mean(y$woe.user_id, na.rm=TRUE)
    x$woe.user_id <- ifelse(x$f.user_id<2, 0,x$woe.user_id)
    y$woe.user_id <- ifelse(y$f.user_id<2, 0,y$woe.user_id)
    
    setDT(z, key='user_id')
    z[x, woe.user_id := i.woe.user_id]
    z$woe.user_id[is.na(z$woe.user_id)] <- mean(z$woe.user_id, na.rm=TRUE)
    z$woe.user_id <- ifelse(z$f.user_id<2, 0,z$woe.user_id)
    
    setDT(x, key='user_dob')
    x$woe.user_dob <- ifelse(x$f.user_dob<2, 0,x$woe.user_dob)
    setDT(y, key='user_dob')
    y[x, woe.user_dob:= i.woe.user_dob]
    y$woe.user_dob[is.na(y$woe.user_dob)] <- mean(y$woe.user_dob, na.rm=TRUE)
    y$woe.user_dob <- ifelse(y$f.user_dob<2, 0,y$woe.user_dob)
    
    setDT(z, key='user_dob')
    z[x, woe.user_dob := i.woe.user_dob]
    z$woe.user_dob[is.na(z$woe.user_dob)] <- mean(z$woe.user_dob, na.rm=TRUE)
    z$woe.user_dob <- ifelse(z$f.user_dob<2, 0,z$woe.user_dob)
    
    
    setDT(x, key='user_reg_date')
    x$woe.user_reg_date <- ifelse(x$f.user_reg_date<2, 0,x$woe.user_reg_date)
    setDT(y, key='user_reg_date')
    y[x, woe.user_reg_date:= i.woe.user_reg_date]
    y$woe.user_reg_date[is.na(y$woe.user_reg_date)] <- mean(y$woe.user_reg_date, na.rm=TRUE)
    y$woe.user_reg_date <- ifelse(y$f.user_reg_date<2, 0,y$woe.user_reg_date)
    setDT(z, key='user_reg_date')
    z[x, woe.user_reg_date := i.woe.user_reg_date]
    z$woe.user_reg_date[is.na(z$woe.user_reg_date)] <- mean(z$woe.user_reg_date, na.rm=TRUE)
    z$woe.user_reg_date <- ifelse(z$f.user_reg_date<2, 0,z$woe.user_reg_date)
    returnlist <- list(x,y,z)
  }
                migrate(woe.set, known_tr, known_test)
                migrateList <- list()
                migrateList <- migrate(woe.set, known_tr, known_test)
                woe.set <- migrateList[[1]]
                known_tr <- migrateList[[2]]
                known_test <- migrateList[[3]]
                
disc <- function(x,y,z){
  
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  z <- as.data.frame(z)
  
  x[,"return"] <- as.factor(x[,"return"])
  
  for(i in c("woe.order_date", "woe.delivery_date", "woe.item_id","woe.item_size",
             "woe.item_color","woe.brand_id","woe.user_id","woe.user_dob","woe.user_reg_date",
             "item_price")){
    
    disc <- iv.num(x, i, "return",rcontrol=rpart.control(cp=.001))
    a1 <- vector()
    disc[,"class"] <- as.character( disc[,"class"])
    a1 <- sapply(strsplit(disc[,"class"], split = ";"), "[", 2)
    b1 <- gsub('\\)','',a1)
    v1 <- na.omit(as.numeric(b1))
    v1 <- append(v1, c(max(x[,i]), min(x[,i]), max(y[,i]), min(y[,i]),max(z[,i]), min(z[,i])))
    v1 <- sort(unique(v1))
    
    x[,paste(i, "cat", sep=".")] <- cut(x[,i], v1, include.lowest=T, include.highest=T)
    y[,paste(i, "cat", sep=".")] <- cut(y[,i], v1, include.lowest=T, include.highest=T)
    z[,paste(i, "cat", sep=".")] <- cut(z[,i], v1, include.lowest=T, include.highest=T)
    
  }
  returnlist <- list(x,y,z)
  return(returnlist)
}


            discList <- list()
            discList <- disc(woe.set, known_tr, known_test)
            woe.set <- discList[[1]]
            known_tr <- discList[[2]]
            known_test <- discList[[3]]
  
standardize <- function(x){
    item_price <- x[,"item_price"]
              for(i in c("delivery.time","user.age","item_price","user.reg.time",
                         "f.order_date","f.delivery_date","f.item_id","f.item_size",
                         "f.item_color","f.brand_id","f.user_id","f.user_dob",
                         "f.user_reg_date")){
                x[,i] <- (x[,i] -  mean(x[,i]))/sd(x[,i])
              }
    x <- cbind(x, org.item_price = item_price)
    return(x)
  }
            
  woe.set <- standardize(woe.set)
  known_tr <- standardize(known_tr)
  known_test <- standardize(known_test)
  
  colnames(known_tr)[2] <- "item_price.st"
  colnames(known_tr)[length(colnames(known_tr))] <- "item_price"
  
  colnames(known_test)[2] <- "item_price.st"
  colnames(known_test)[length(colnames(known_test))] <- "item_price"
  
  colnames(woe.set)[2] <- "item_price.st"
  colnames(woe.set)[length(colnames(woe.set))] <- "item_price"
  
  
  varset <- c("order_item_id","item_price","user_title","user_state",
              "user.reg.time","user.age","delivery.time","f.order_date",
              "f.delivery_date","f.item_id","f.item_size","f.item_color",
              "f.brand_id","f.user_id","f.user_dob","f.user_reg_date","return",
              "woe.order_date","woe.delivery_date","woe.item_id",
              "woe.item_size","woe.item_color","woe.brand_id",
              "woe.user_id","woe.user_dob","woe.user_reg_date",
              "woe.order_date.cat","woe.delivery_date.cat","woe.item_id.cat",
              "woe.item_size.cat","woe.item_color.cat","woe.brand_id.cat",
              "woe.user_id.cat","woe.user_dob.cat","woe.user_reg_date.cat",
              "item_price.cat","org.item_price")
  
woe.set <- subset(woe.set, select= colnames(woe.set) %in% intersect(colnames(woe.set), varset))
known_tr <- subset(known_tr, select= colnames(known_tr) %in% intersect(colnames(known_tr), varset))
known_test <- subset(known_test , select= colnames(known_test) %in% intersect(colnames(known_test), varset))

known_tr$return <- as.factor(known_tr$return)
known_test$return <- as.factor(known_test$return)
woe.set$return <- as.factor(woe.set$return)

  woe.set <<- woe.set
  known_tr <<- known_tr
  known_test <<- known_test
}


