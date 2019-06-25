getwd()
setwd("D:/Rdata")
library(forecast)
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")

training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset = training, 
          na.action = na.exclude)
pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1,2,8,11)],
                  na.action = na.pass)

accuracy(pred_t, toyota.corolla.df[training,]$Price)
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

hist(reg$residuals, breaks = 50)
reg_1.residuals<-toyota.corolla.df[validation,]$Price[!is.na(pred_v)] - 
  pred_v[!is.na(pred_v)]
boxplot(reg$residuals, reg_1.residuals, names=c("training", "validation"))

###############################################################

toyota.corolla.df <- read.csv("ToyotaCorolla.csv", header=TRUE)
str(toyota.corolla.df)

#Lift/Decile Lift Chart, Remove Price missing
toyota.corolla.df <- toyota.corolla.df[!is.na(toyota.corolla.df$Price), ]
dim(toyota.corolla.df)

training <- sample(rownames(toyota.corolla.df), 600)
validation <- sample(setdiff(rownames(toyota.corolla.df), training), 400)

reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset=training)
pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1,2,8,11)])

library(gains)
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)],
              pred_v[!is.na(pred_v)])
gain
options(scipen=999)
price <- toyota.corolla.df[validation, ]$Price[!is.na(toyota.corolla.df[validation, ]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0, gain$cume.obs), xlab="#cases", ylab="Cummulative Price", main="Lift Chart", type="l")

####baseline
lines(c(0, sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]),col="RED", lty=2)

###십분위향상차트 
barplot(gain$mean.resp/mean(price), names.arg = gain$depth, xlab="Percentile", ylab="Mean Response", ylim=c(0,2), main="Decile lift chart")


