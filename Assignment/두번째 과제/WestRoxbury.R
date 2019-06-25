setwd("D:/Rdata")
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
str(housing.df)
dim(housing.df)
head(housing.df)
View(housing.df)
housing.df$TOTAL.VALUE[1:10]
length(housing.df$TOTAL.VALUE)
mean(housing.df$TOTAL.VALUE)
summary(housing.df)
s <- sample(row.names(housing.df), 5)
s
housing.df[s,]
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS>10, 0.9, 0.1))
s
housing.df[s,]
?sample
#####################################################################Week3
names(housing.df)
t(names(housing.df))
t(t(names(housing.df)))
class(housing.df$REMODEL)
levels(housing.df$REMODEL)
###########################################################dummy variables
xtotal <- model.matrix( ~ 0 + BEDROOMS + REMODEL, data=housing.df)
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)  
xtotal <- xtotal[ , -4 ]
head(xtotal)
housing.df <- cbind(housing.df[, -c(9, 14)], xtotal)
t(t(names(housing.df)))
############################################################missing
rows.to.missing <- sample(row.names(housing.df), 10)
rows.to.missing
housing.df[rows.to.missing, ] $BEDROOMS <- NA                          
summary(housing.df$BEDROOMS)
housing.df[rows.to.missing, ] $BEDROOMS <- median(housing.df $BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)
############################################################data partition
set.seed(1)
dim(housing.df)
train.rows <- sample(row.names(housing.df), dim(housing.df)[1]*0.5)
head(train.rows)
train.data <- housing.df[train.rows, ]
str(train.data)
valid.rows <- sample(setdiff(row.names(housing.df), train.rows) , dim(housing.df)[1]*0.3)
head(valid.rows)
valid.data <- housing.df[valid.rows, ]
test.rows <- setdiff(row.names(housing.df), union(train.rows, valid.rows))
test.data <- housing.df[test.rows, ]
############################################################regression
reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows) 
str(reg)
reg
tr.res <- data.frame(train.data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)
########################################################### validation data
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals =
                       valid.data$TOTAL.VALUE - pred)
head(vl.res)
install.packages(("forecast"))
library(forecast)
accuracy(pred, valid.data$TOTAL.VALUE)
########################################################### train data
pred <- predict(reg, newdata = train.data)
vl.res <- data.frame(train.data$TOTAL.VALUE, pred, residuals =
                       train.data$TOTAL.VALUE - pred)
head(vl.res)
install.packages(("forecast"))
library(forecast)
accuracy(pred, train.data$TOTAL.VALUE)
########################################################### test data
pred <- predict(reg, newdata = test.data)
vl.res <- data.frame(test.data$TOTAL.VALUE, pred, residuals =
                       test.data$TOTAL.VALUE - pred)
head(vl.res)
install.packages(("forecast"))
library(forecast)
accuracy(pred, test.data$TOTAL.VALUE)




