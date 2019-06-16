setwd("D:/Rdata")
library(e1071)
delay.df <- read.csv("FlightDelays.csv")
str(delay.df)

## change numerical variables to categorical first
delay.df$DAY_WEEK <- factor(delay.df$DAY_WEEK)

### delay.df$DEP_TIME <- factor(delay.df$DEP_TIME)
delay.df$CRS_DEP_TIME <- factor(round(delay.df$CRS_DEP_TIME/100))

## Create training and validation sets
selected.var <- c(10, 1, 8, 4, 2, 13)
train.index <- sample(c(1:dim(delay.df)[1]), dim(delay.df)[1]*0.6)
train.df <- delay.df[train.index, selected.var]
valid.df <- delay.df[-train.index, selected.var]

## run naive bayes / 비행기 연착에 대한 예측
delay.nb <- naiveBayes(Flight.Status~., data=train.df)
delay.nb

## PIVOT TABLE OF FLIGHT STATUS BY DESTINATION AIRPORT (TRAINING DATA)
## use prop.table() with margin = 1 to convert a count table to a proportion table,
## where each row sums up to 1 (use margin = 2 for column sums).
## margin = 1 : 행방향 상대밀도 / 2 : 열방향 상대밀도
prop.table(table(train.df$Flight.Status, train.df$DEST),margin = 1)


######################### 새로운 데이터 분류
## predict probabilities
pred.prob <- predict(delay.nb, newdata = valid.df, type = "raw")
## predict class membership
pred.class <- predict(delay.nb, newdata = valid.df)

df <- data.frame(actual=valid.df$Flight.Status, predicted=pred.class, pred.prob)

df[valid.df$CARRIER == "DL" & valid.df$DAY_WEEK == 7 & valid.df$CRS_DEP_TIME == 10 & 
     valid.df$DEST == "LGA" & valid.df$ORIGIN == "DCA",]

## confusion matrices for flight delay using a naive bayes classifier
library(caret)
## training
pred.class <- predict(delay.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Flight.Status)
## validation
pred.class <- predict(delay.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)

## Lift Chart : Naive Bayes is Effective in capturing the delayed flights
library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[ ,1], groups=100)
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed")) ~c(0, gain$cume.obs), xlab="# cases", ylab="Cummulative", main="Lift chart",type="l")                     
lines(c(0, sum(valid.df$Flight.Status=="delayed"))~c(0, dim(valid.df)[1]), lty=2)

