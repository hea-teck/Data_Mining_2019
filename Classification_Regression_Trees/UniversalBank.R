setwd("D:Rdata")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)


class.tree <- rpart(Ownership ~ ., data=mower.df, control = rpart.control(maxdepth=2), method="class")
prp(class.tree, type=1, extra=1, split.font =1, varlen=-10)
prp(class.tree, under=TRUE, type=1, extra=1, split.font =1, varlen=-10)

#Full Grown Tree
class.tree <- rpart ( Ownership ~ ., data=mower.df, control = rpart.control(minsplit=2), method="class")
prp(class.tree, under=TRUE, type=1, extra=1, split.font =1, varlen=-10)

########################################################################################################

bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
bank.df <- bank.df[ , -c(1,5)]
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index, ]
###############Default CT
default.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class")
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10)
str(default.ct)
length(default.ct$frame$var[default.ct$frame$var=="<leaf>"])
default.ct$frame$var  # tree 순회 
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(default.ct$frame$var=="<leaf>", 'gray', 'white'))  #leaf node gray
#################Full CT
deeper.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0, minsplit=1)
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])
prp(deeper.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(deeper.ct$frame$var=="<leaf>", 'gray', 'white'))

#############################################################################
library(caret)
#train.df
default.ct.point.pred.train <- predict(default.ct, train.df, type="class")
confusionMatrix(default.ct.point.pred.train, factor(train.df$Personal.Loan))
#valid.df
default.ct.point.pred.valid <- predict(default.ct, valid.df, type="class")
confusionMatrix(default.ct.point.pred.valid, factor(valid.df$Personal.Loan))

#############################################################################

#train.df
deeper.ct.point.pred.train <- predict(deeper.ct, train.df, type="class")
confusionMatrix(deeper.ct.point.pred.train, factor(train.df$Personal.Loan))
#valid.df
deeper.ct.point.pred.valid <- predict(deeper.ct, valid.df, type="class")
confusionMatrix(default.ct.point.pred.valid, factor(valid.df$Personal.Loan))

###########################################################################
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
bank.df <- bank.df[ , -c(1,5)]
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index, ]
################################################################full grown
cv.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0.00001, 
               minsplit=5, xval=5)
str(cv.ct)
prp(cv.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(cv.ct$frame$var=="<leaf>", 'gray', 'white'))
length(cv.ct$frame$var[cv.ct$frame$var=="<leaf>"])
printcp(cv.ct)
################################################################min error tree
pruned.ct <- prune(cv.ct, cp=cv.ct$cptable[which.min(cv.ct$cptable[ , "xerror"]), 
                                           "CP"] )
length(pruned.ct$frame$var[pruned.ct$frame$var=="<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen=-10,
    box.col=ifelse(pruned.ct$frame$var=="<leaf>", 'gray', 'white'))
0.15120+0.022627
#################################################################best pruned tree
pruned.ct <- prune(cv.ct, cp=0.0068729 )  #minimum cp within xerror +- std(xerror)
length(pruned.ct$frame$var[pruned.ct$frame$var=="<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen=-10,
    box.col=ifelse(pruned.ct$frame$var=="<leaf>", 'gray', 'white'))

#################################################################random forest
install.packages("randomForest")
library(randomForest)
rb <- randomForest(as.factor(Personal.Loan)~., data = train.df, ntree = 500,
                   mtry=4, nodesize = 5, importance = TRUE)
varImpPlot(rf, type =1)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Personal.Loan)
