setwd("D:/Rdata")
library(e1071)
accidents.df <- read.csv("accidents.csv")

accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")

## 나이브 베이즈를 적용하기 위해서는 모든 factor형 이어야 한다.
for( i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}
str(accidents.df)

## 관련된 예측변수들과 완전한 학습 세트를 사용하여 나이브 베이즈 분류기를 실행
selected.var <- c(3,7,8,14,15,16,17,19,25)
selected.var

train.index <- sample(c(1:dim(accidents.df)[1]), dim(accidents.df)[1]*0.6)
train.df <- accidents.df[train.index, selected.var]
valid.df <- accidents.df[-train.index, selected.var]

accidents.nb <- naiveBayes(INJURY~., data=train.df)
accidents.nb

## 학습데이터의 정오행렬
library(caret)
pred.class <- predict(accidents.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$INJURY)

## 검증데이터의 정오행렬
pred.class <- predict(accidents.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$INJURY)
