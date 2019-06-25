setwd("D:/Rdata")
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
set.seed(1)

bank.df$Education <- factor(bank.df$Education)
edu.df <- as.data.frame(model.matrix(~0 + Education, data = bank.df))
edu.df
bank.df <- cbind(bank.df[,-c(1,5,8)], edu.df[,])
bank.df

train.index <- sample(row.names(bank.df),0.5*dim(bank.df)[1])
valid.index <- sample(setdiff(row.names(bank.df), train.index), 0.3*dim(bank.df)[1])
test.index <- setdiff(rownames(bank.df), union(train.index, valid.index))

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]
test.df <- bank.df[test.index,]


new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, 
                     CCAvg = 2, Mortgage = 0, Securities.Account = 0, 
                     CD.Account = 0, Online = 1, CreditCard = 1, 
                     Education1 = 0, Education2 = 1, Education3 = 0)
new.df

train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
bank.norm.df <- bank.df

library(caret)
norm.values <- preProcess(train.df[,-7], method=c("center", "scale"))
norm.values

train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
test.norm.df[,-7] <- predict(norm.values, test.df[,-7])
bank.norm.df[,-7] <- predict(norm.values, bank.df[,-7])
new.norm.df <- predict(norm.values, new.df)
new.norm.df

library(FNN)
nn <- knn(train=train.norm.df[ , -7], test=valid.norm.df[,-7], cl=train.norm.df[ , 7], k=3, prob=TRUE)
nn
confusionMatrix(nn, as.factor(valid.norm.df[,7]), positive = "1")
#####################################################################
library(caret)
accuracy.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
accuracy.df

for(i in 1:20){
  knn.pred <- knn(train=train.norm.df[ , -7], test=valid.norm.df[ , -7], 
                  cl=train.norm.df[,7], k=i)
  accuracy.df[i,2]<-  confusionMatrix(knn.pred, as.factor(valid.norm.df[, 7]))$overall[1]
}

accuracy.df
#####################################################################
knn.pred.new <- knn(train = bank.norm.df[ ,-7], test = valid.norm.df[,-7], 
                    cl=bank.norm.df[ , 7], k=3, prob=TRUE)
knn.pred.new

confusionMatrix(knn.pred.new, as.factor(valid.norm.df[,7]), positive = "1")





















