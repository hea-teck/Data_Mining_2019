setwd("D:/Rdata")

boston.df <- read.csv("BostonHousing.csv")

set.seed(1)

##training <- sample(rownames(boston.df[,-c(3,7)]), 306)         ##10번째 모델
##training <- sample(rownames(boston.df[,-c(7)]), 306)           ##11번째 모델
training <- sample(rownames(boston.df[,]), 306)                  ##12번째 모델

validation <- sample(setdiff(rownames(boston.df), training))

##reg <- lm(MEDV~., data=boston.df[,-c(3,7)], subset=training)   ##10번째 모델
##reg <- lm(MEDV~., data=boston.df[,-c(7)], subset=training)     ##11번째 모델
reg <- lm(MEDV~., data=boston.df[,], subset=training)            ##12번째 모델

summary(reg)

library(forecast)
##pred_v <-predict(reg, newdata=boston.df[validation, -c(3,7)])  ##10번째 모델
##pred_v <-predict(reg, newdata=boston.df[validation, -c(7)])    ##11번째 모델
pred_v <-predict(reg, newdata=boston.df[validation, ])           ##12번째 모델

library(gains)
gain <- gains(boston.df[validation, ]$MEDV[!is.na(pred_v)], pred_v[!is.na(pred_v)])
options(scipen=999)
medv <-boston.df[validation, ]$MEDV[!is.na(boston.df[validation, ]$MEDV)]
plot(c(0,gain$cume.pct.of.total*sum(medv))~c(0, gain$cume.obs), xlab="# cases", 
     ylab="Cummulative Price", main="Lift Chart", type="l")

lines(c(0, sum(medv))~c(0,dim(boston.df[validation,])[1]),col="RED", lty=2)
