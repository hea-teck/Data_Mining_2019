setwd("D:/Rdata")
#use first 1000 rows of data
car.df <- read.csv("ToyotaCorolla.csv")
str(car.df)
car.df <- car.df[1:1000, ]
#select variables for regression
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
str(train.df)
#use lm() to run a linear regression of Price on all 10 predictors in 
#training set.
#use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price~., data=train.df)
class(car.lm)
str(car.lm)
options(scipen=999)
summary(car.lm)
##some residual
library(forecast)
#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits=0)
some.residuals <-valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted"=car.lm.pred[1:20], "Actual"=valid.df$Price[1:20],
           "Residual"=some.residuals)
###all residual
all.residuals <- valid.df$Price - car.lm.pred
options(scipen=999, digits=5)
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks=25, xlab="Residuals")
accuracy(car.lm.pred, valid.df$Price)
accuracy(car.lm$fitted.values, train.df$Price)
################################################
#####exhaustive search
install.packages("leaps")
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, 	data=train.df))
head(Fuel_Type)
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, 	
                     nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2
sum$cp
################search best model
#select variables for regression
selected.var <- c(3,4,7,8,9,12,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
str(train.df)
car.lm.best_8 <- lm(Price~., data=train.df)
options(scipen=999)
summary(car.lm.best_8)
###############best with 7
selected.var <- c(3,4,7,8,9,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
str(train.df)
car.lm.best_7 <- lm(Price~., data=train.df)
options(scipen=999)
summary(car.lm.best_7)
###########################Forward selection######
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
str(train.df)
#use lm() to run a linear regression of Price on all 10 predictors in 
#training set.
#use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price~., data=train.df)
car.lm.null <- lm(Price~1, data = train.df)

# use step() to run forward selection
car.lm.step <- step(car.lm.null,   
                    scope=list(lower=car.lm.null, upper=car.lm), direction =  
                      "forward")

summary(car.lm.step) 
# Which variables were added?
######BACKWARD SELECTION
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 
# Which variables were dropped?
#########sTEPWISE#
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) 


