setwd("D:/Rdata")
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
set.seed(1)

bank.df$Education <- factor(bank.df$Education)
edu.df <- as.data.frame(model.matrix(~0 + Education, data = bank.df))

## Education이 범주형변수이므로 가변수 Education1, Education2, Education3으로 설정하여 열을 추가하였다. 
bank.df <- cbind(bank.df[,-c(1,5,8)], edu.df[,])
bank.df

## 데이터를 학습 세트 50%와 검증 세트 30%, 평가 세트 20%로 분리
train.index <- sample(row.names(bank.df),0.5*dim(bank.df)[1])
valid.index <- sample(setdiff(row.names(bank.df), train.index), 0.3*dim(bank.df)[1])
test.index <- setdiff(rownames(bank.df), union(train.index, valid.index))

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]
test.df <- bank.df[test.index,]


## 새로운 데이터 new data 추가
new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, 
                     CCAvg = 2, Mortgage = 0, Securities.Account = 0, 
                     CD.Account = 0, Online = 1, CreditCard = 1, 
                     Education1 = 0, Education2 = 1, Education3 = 0)
new.df

## 정규화 작업 시작
train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
bank.norm.df <- bank.df

library(caret)
norm.values <- preProcess(train.df[,-7], method=c("center", "scale"))
norm.values

## 개인 대출 여부를 결정하는 목표변수 Personal Loan 7번째 열 제외
train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
test.norm.df[,-7] <- predict(norm.values, test.df[,-7])
bank.norm.df[,-7] <- predict(norm.values, bank.df[,-7])
new.norm.df <- predict(norm.values, new.df) ## new.df에는 목표변수가 당연히 없으니 -7을 하지 않는 것이다.
new.norm.df


## best k=3을 사용
## 평가 세트에 대한 분류행렬을 학습 세트 및 검증 세트의 정오행렬표와 비교
nn <- knn(train=train.norm.df[,-7], test=test.norm.df[,-7], cl=train.norm.df[,7], k=3, prob=TRUE)
nn
confusionMatrix(nn, as.factor(test.norm.df[,7]), positive = "1")

nn <- knn(train=train.norm.df[,-7], test=valid.norm.df[,-7], cl=train.norm.df[,7], k=3, prob=TRUE)
nn
confusionMatrix(nn, as.factor(valid.norm.df[,7]), positive = "1")



























