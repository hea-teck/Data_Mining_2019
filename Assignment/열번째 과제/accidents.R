setwd("D:/Rdata")
library(e1071)
accidents.df <- read.csv("accidents.csv")
str(accidents.df)

accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")

for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}

str(accidents.df)

selected.var  <- c(3,7,8,14,15,16,17,19,25)
selected.var

train.index <- sample(c(1:dim(accidents.df)[1]), dim(accidents.df)[1]*0.6)
train.df <-accidents.df[ train.index, selected.var]
valid.df <-accidents.df[ - train.index, selected.var]

accidents.nb <- naiveBayes(INJURY~., data=train.df)
accidents.nb

library(caret)
pred.class <- predict(accidents.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$INJURY)

