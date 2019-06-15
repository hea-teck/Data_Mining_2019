setwd("D:/Rdata")
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
set.seed(1)

bank.df$Education <- factor(bank.df$Education)
edu.df <- as.data.frame(model.matrix(~0 + Education, data = bank.df))

## Education이 범주형변수이므로 가변수 Education1, Education2, Education3으로 설정하여 열을 추가하였다. 
bank.df <- cbind(bank.df[,-c(1,5,8)], edu.df[,])
bank.df

## 데이터를 학습 세트 60%와 검증 세트 40%로 분리
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])
length(train.index)
valid.index <- setdiff(row.names(bank.df), train.index)

train.df <- bank.df[train.index,]
train.df
valid.df <- bank.df[valid.index,]
valid.df

## 새로운 데이터 new data 추가
new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, 
                     CCAvg = 2, Mortgage = 0, Securities.Account = 0, 
                     CD.Account = 0, Online = 1, CreditCard = 1, 
                     Education1 = 0, Education2 = 1, Education3 = 0)
new.df

## 정규화 작업 시작
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df

library(caret)
norm.values <- preProcess(train.df[,-7], method=c("center", "scale"))
norm.values

## 개인 대출 여부를 결정하는 목표변수 Personal Loan 7번째 열 제외
train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
bank.norm.df[,-7] <- predict(norm.values, bank.df[,-7])
new.norm.df <- predict(norm.values, new.df) ## new.df에는 목표변수가 당연히 없으니 -7을 하지 않는 것이다.
new.norm.df

## k=1 사용
library(FNN)
## train=train.norm.df : KNN을 찾는 데이터
## test=new.norm.df : 목표변수값을 예측하기 위한 데이터
## cl=train.norm.df : 소속 클래스를 확인 및 결정할 수 있도록 
nn <- knn(train=train.norm.df[,-7], test=new.norm.df, cl=train.norm.df[,7], k=1, prob=TRUE)
nn
row.names(train.df)[attr(nn, "nn.index")] ## 전체데이터에서 자신이 갖고 있는 row.name이 어디있는지 출력

## best k 찾기
library(caret)
accuracy.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
accuracy.df
## 보통 k값이 1부터 20이므로 for문 범위 적용
for(i in 1:20){
  knn.pred <- knn(train=train.norm.df[,-7], test=valid.norm.df[,-7], cl=train.norm.df[,7], k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred, as.factor(valid.norm.df[,7]))$overall[1]
}
## knn.pred : 예측값(predicted)
## valid.norm.df[,7] : 실제 값(actual)
## int형 1,0으로 되어있으므로 confusionMatrix적용시키기 위해 factor형 변수로 변경
accuracy.df

## best k를 사용하여 검증 세트에 대한 정오행렬표 출력
knn.pred <- knn(train=bank.norm.df[,-7], test=valid.norm.df[,-7], cl=bank.norm.df[,7], k=3, prob=TRUE)
knn.pred
confusionMatrix(knn.pred, as.factor(valid.norm.df[,7]), positive = "1")

## best k를 찾았으므로 실제 전체 데이터(bank.norm.df)와 비교
knn.pred.new <- knn(train=bank.norm.df[,-7], test=new.norm.df, cl=bank.norm.df[,7], k=3, prob = TRUE)
knn.pred.new
row.names(bank.df)[attr(knn.pred.new, "nn.index")]
