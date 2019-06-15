setwd("D:/Rdata")
mower.df <- read.csv("RidingMowers.csv")
str(mower.df) ## Ownership: Factor 변수이다.
mower.df

set.seed(111)

train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1]) ## dim: 행, 열 개수를 알려준다.
length(train.index)
valid.index <- setdiff(row.names(mower.df), train.index)

train.df <- mower.df[train.index,] ## 총 24개 중 *0.6 => 14개
train.df
valid.df <- mower.df[valid.index,] ## 24 - 14 = 10개
valid.df

## new data
new.df <- data.frame(Income=60, Lot_Size=20)

## scatter plot 산점도 표시. 초기 데이터가 아닌 train.df 이용한 것이다. 14개만 표시 된 것임
plot(Lot_Size~Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner",1,3)) ## 1: O, 3: +, 4: X 표시

## text(60,20,4) 위 new data 위치를 X로 표현해 주는 것 
text(60,20,"X")
## 그림 우측 상단에 범주 표시해 주는 것
legend("topright", c("owner", "nonowner", "newhousehold"), pch=c(1,3,4))

## Initialzing normalized training, validation, complete data frame
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

## Use preProcess() from caret package to normalize Income and Lot_Size
library(caret) ## center: 예측변수에서 평균을 빼라. scale: 표준편차로 나누어라.
norm.values <- preProcess(train.df[,1:2], method=c("center", "scale"))

## mean and standard deviation
mean(train.df[,1])
mean(train.df[,2])
sd(train.df[,1])
sd(train.df[,2])

## predict는 첫 인수의 class에 따라 method가 달라짐
train.norm.df[,1:2] <- predict(norm.values, train.df[,1:2])
valid.norm.df[,1:2] <- predict(norm.values, valid.df[,1:2])
mower.norm.df[,1:2] <- predict(norm.values, mower.df[,1:2])

new.norm.df<- predict(norm.values, new.df)
new.norm.df

## use knn() to compute knn. knn is available in the library FNN
## “nn.index” : train.df 의 index
## row.names(train.df)는 mower.df에서 가져온 rownames
## train.df[c("9","14","1"), ]
## train.norm.df[c(3, 12, 7),]
library(FNN)
nn <- knn(train = train.norm.df[,1:2], test=new.norm.df, cl=train.norm.df[,3], k=3, prob=TRUE)
row.names(train.df)[attr(nn,"nn.index")]
nn

## 각기 다른 k값에 대하여 정확도를 측정하는 R코드
library(caret)

## initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k=seq(1,14,1), accuracy=rep(0,14))
dim(accuracy.df)

## compute knn for different k on validation
for(i in 1:14){
  knn.pred <- knn(train.norm.df[,1:2], valid.norm.df[,1:2], cl = train.norm.df[,3], k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred, valid.norm.df[,3])$overall[1]
}
## 참고 conf <- confusionMatrix(knn.pred, valid.norm.df[,3])
## 참고 str(conf) ## overall의 첫번째 원소가 Accuracy 
accuracy.df

## Classifying new data using best k=4
knn.pred.new <- knn(train=mower.norm.df[,1:2], test = new.norm.df, cl=mower.norm.df[,3], k=4, prob = TRUE)
knn.pred.new
## row.names(train.df)[attr(nn, "nn.index")]
row.names(mower.df)[attr(knn.pred.new, "nn.index")]
