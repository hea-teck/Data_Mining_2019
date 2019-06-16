setwd("D:/Rdata")

boston.df <- read.csv("BostonHousing.csv")

set.seed(1)
train.index <- sample(c(1:506), 306)
## train.df <- boston.df[train.index, c(1:13)]

train.df <- boston.df[train.index, -c(3,7)] ## 10번째 모델
valid.df <- boston.df[-train.index, -c(3,7)] ## 10번째 모델

## 모든 변수 사이에 상관계수를 구한 결과 (그 중 3개의 관계를 알기위해 따로 추출)
cor(boston.df) ## TAX와 RAD 둘 사이 밀접한 관계가 존재-> 동일한 것을 측정할 것 같다고 말할 수 있음
cor(boston.df[,c("INDUS", "NOX", "TAX")])

library(leaps)
search <- regsubsets(MEDV~., data=train.df, nbest = 1, nvmax = dim(train.df)[2], method="exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2
sum$cp

boston.lm <- lm(MEDV~., data=train.df)
summary(boston.lm)

library(forecast)
boston.lm.pred <- predict(boston.lm, valid.df)
accuracy(boston.lm.pred, valid.df$MEDV) ## 10번째 모델
