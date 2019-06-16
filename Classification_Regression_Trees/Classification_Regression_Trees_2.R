library(rpart)
library(rpart.plot)
setwd("D")
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5)] # Drop ID and Zip code columns.

## partition
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

## classification tree
default.ct <- rpart(Personal.Loan~., data=train.df, method="class")

## plot tree
prp(default.ct, type=1, extra=1, under=TRUE, split.font=1, varlen = -10)

## Full Grown Tree
deeper.ct <- rpart(Personal.Loan~., data=train.df, method = "class", cp=0, minsplit=1)
## count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
## plot tree
prp(deeper.ct, type=1, extra=1, under = TRUE, split.font = 1, varlen=-10,
    box.col = ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))


## full grown에서 검증데이터 셋 Accuracy 낮은 것 확인 (default.ct<->deeper.ct)
library(caret) ## 과적합이 발생한 것이다.
#train.df
default.ct.point.pred.train <- predict(default.ct, train.df, type="class")
confusionMatrix(default.ct.point.pred.train, factor(train.df$Personal.Loan))
#valid.df
default.ct.point.pred.valid <- predict(default.ct, valid.df, type="class")
confusionMatrix(default.ct.point.pred.valid, factor(valid.df$Personal.Loan))



################# cross validation procedure
cv.ct <- rpart(Personal.Loan~., data=train.df, method="class", cp=0.00001,
               minsplit=5, xval=5)
prp(cv.ct, type = 1, extra = 1, under=TRUE, split.font = 1, varlen = -10,
    box.col = ifelse(cv.ct$frame$var=="<leaf>",'gray', 'white'))
length(cv.ct$frame$var[cv.ct$frame$var=="<leaf>"])
## use printcp() tp print the table
printcp(cv.ct)

## prune by lower cp
pruned.ct <- prune(cv.ct, cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen = -10)

## best pruned
pruned.ct <- prune(cv.ct, cp=0.0068729 )  #minimum cp within xerror +- std(xerror)
length(pruned.ct$frame$var[pruned.ct$frame$var=="<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen=-10,
    box.col=ifelse(pruned.ct$frame$var=="<leaf>", 'gray', 'white'))

#--------------------------------------------------------------------------------------

## random forest
library(randomForest)
rf <- randomForest(as.factor(Personal.Loan) ~., data = train.df, ntree=500,
                   mtry=4, nodesize=5, importance=TRUE)
## VARIABLE IMPORTANCE PLOT (어떤 예측변수가 중요한지 알 수 있음)
varImpPlot(rf, type = 1)
## confusion matrix
rf.pred <- predict(rf, valid.df)
library(caret)
confusionMatrix(rf.pred, factor(valid.df$Personal.Loan))


## Boosted Trees
library(adabag)
library(rpart)
library(caret)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
valid.df$Personal.Loan <- as.factor(valid.df$Personal.Loan)

boost <- boosting(Personal.Loan ~ ., data = train.df)
pred <- predict(boost, valid.df)

pred$class <- as.factor(pred$class)

confusionMatrix(pred$class, valid.df$Personal.Loan)
