setwd("D:/Rdata")

boston.df <- read.csv("BostonHousing.csv")

selected.var <- c(1,4,6,13)

set.seed(1)
train.index <- sample(c(1:506), 306)
train.df <- boston.df[train.index, selected.var]
valid.df <- boston.df[-train.index, selected.var]

boston.lm <- lm(MEDV~., data=train.df)
options(scipen=999)
summary(boston.lm)
