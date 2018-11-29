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
temp = california
plotY <- function (x,y) {
plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
ylab=names(temp)[y])
}
par(mfrow=c(3,3)) # Fijar ventana para gráficas
x = sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1)) # Cambiar el tipo de ventana a 1,1 otra vez

## ------------------------------------------------------------------------
plotY(8,dim(temp)[2])

## ------------------------------------------------------------------------
fit1 = lm(MedianHouseValue~MedianIncome, data=california)
summary(fit1)

## ------------------------------------------------------------------------
fit2 = lm(MedianHouseValue~MedianIncome + TotalBedrooms, data=california)
summary(fit2)

## ------------------------------------------------------------------------
fit3 = lm(MedianHouseValue~MedianIncome + TotalBedrooms + TotalRooms, data=california)
summary(fit3)

## ------------------------------------------------------------------------
fit4 = lm(MedianHouseValue~MedianIncome + TotalBedrooms + TotalRooms + Households, data=california)
summary(fit4)

## ------------------------------------------------------------------------
fit5 = lm(MedianHouseValue~., data=california)
summary(fit5)

## ------------------------------------------------------------------------
fit6 = lm(MedianHouseValue~.-Households, data=california)
summary(fit6)

## ------------------------------------------------------------------------
fit7 = lm(MedianHouseValue~.-Households-HousingMedianAge, data=california)
summary(fit7)

## ------------------------------------------------------------------------
fit8 = lm(MedianHouseValue~.-Households-HousingMedianAge-Population, data=california)
summary(fit8)

## ------------------------------------------------------------------------
fit9 = lm(MedianHouseValue~MedianIncome*TotalBedrooms, data=california)
summary(fit9)

## ------------------------------------------------------------------------
fit10 = lm(MedianHouseValue~MedianIncome*HousingMedianAge, data=california)
summary(fit10)

## ------------------------------------------------------------------------
fit11 = lm(MedianHouseValue~MedianIncome*HousingMedianAge*TotalBedrooms, data=california)
summary(fit11)

## ------------------------------------------------------------------------
fit12 = lm(MedianHouseValue~MedianIncome+I(MedianIncome^2), data=california)
summary(fit12)

## ------------------------------------------------------------------------
plot(california$MedianHouseValue~california$MedianIncome)
points(california$MedianIncome,fitted(fit12),col="red",pch=20)

## ------------------------------------------------------------------------
fit13 = lm(MedianHouseValue~.+I(MedianIncome^2)-HousingMedianAge-Households,
        data=california)
summary(fit13)

