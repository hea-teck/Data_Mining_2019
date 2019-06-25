setwd("D:/Rdata")
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)
mower.df
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
length(train.index)
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index, ]
train.df
valid.df <- mower.df[valid.index, ]
valid.df
##new data
new.df <- data.frame(Income=60, Lot_Size=20)
##scatter plot
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
#text(60, 20, 4)
text(60, 20, "X")
legend("topright", c("owner", "nonowner", "newhousehold"), pch=c(1,3,4))

#Initialzing normalized training, validation, complete data frame
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

#use preProcess() from caret package to normalize Income and Lot_Size
library(caret)
norm.values <- preProcess(train.df[ , 1:2], method=c("center", "scale")) 
str(norm.values)
#mean and standard deviation
mean(train.df[,1]);mean(train.df[ , 2]);sd(train.df[,1]);sd(train.df[ , 2])

#predic는 첫 인수의 class에 따라 method가 달라짐
train.norm.df[ , 1:2] <- predict(norm.values, train.df[ , 1:2])
valid.norm.df[ , 1:2] <- predict(norm.values, valid.df[ , 1:2])
mower.norm.df[ , 1:2] <- predict(norm.values, mower.df[ , 1:2])
new.norm.df <- predict(norm.values, new.df)
new.norm.df
#train.df의 변수의 평균과 표준편차를 이용하여 new.norm.df 표준화 체크해 보기
(60-mean(train.df[,1]))/sd(train.df[,1])
(20-mean(train.df[,2]))/sd(train.df[,2])

install.packages("FNN")
library(FNN)
nn <- knn(train=train.norm.df[ , 1:2], test=new.norm.df, cl=train.norm.df[ , 3], k=3, prob=TRUE)
row.names(train.df)[attr(nn, "nn.index")]
nn
train.df
#########################################
library(caret)
accuracy.df <- data.frame(k=seq(1,14,1), accuracy=rep(0,14))
dim(accuracy.df)
accuracy.df
for(i in 1:14){
  knn.pred <- knn(train=train.norm.df[ , 1:2], test=valid.norm.df[ , 1:2], 
                  cl=train.norm.df[,3], k=i)
  accuracy.df[i,2]<- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1]
}
conf <-confusionMatrix(knn.pred, valid.norm.df[, 3])
conf
str(conf) #overall의 첫번째 원소가  Accuracy
#########################################
knn.pred.new <- knn(train=mower.norm.df[ , 1:2], test=new.norm.df, 
                    cl=mower.norm.df[ , 3], k=4, prob=TRUE)
knn.pred.new
#row.names(train.df)[attr(nn, "nn.index")]: 교과서가  틀림
row.names(mower.df)[attr(knn.pred.new, "nn.index")]

