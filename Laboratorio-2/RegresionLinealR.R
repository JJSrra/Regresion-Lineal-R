## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
california = read.csv("../Datos/california.dat", header = FALSE, comment.char = "@")
head(california)

## ------------------------------------------------------------------------
names(california) = c("Longitude", "Latitude", "HousingMedianAge",
"TotalRooms", "TotalBedrooms", "Population", "Households",
"MedianIncome", "MedianHouseValue")

head(california)

## ------------------------------------------------------------------------
require("kknn")

## ------------------------------------------------------------------------
fitknn1 = kknn(MedianHouseValue ~ ., california, california)
names(fitknn1)

## ------------------------------------------------------------------------
head(fitknn1$fitted.values)

## ------------------------------------------------------------------------
plot(california$MedianHouseValue~california$MedianIncome)
points(california$MedianIncome,fitknn1$fitted.values,col="blue",pch=20)

## ------------------------------------------------------------------------
yprime = fitknn1$fitted.values
sqrt(sum((california$MedianHouseValue-yprime)^2)/length(yprime))

## ------------------------------------------------------------------------
nombre = "../Datos/california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}

lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

lmMSEtrain
lmMSEtest

## ------------------------------------------------------------------------
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

knnMSEtrain
knnMSEtest

