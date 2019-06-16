setwd("D:/Rdata")
library(rpart)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")

## use rpart() to run a classification tree.
## define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~., data=mower.df,
                    control = rpart.control(maxdepth = 2), method = "class")

## plot tree
## use prp() to plot the tree. You can control plotting parameters such as color, shape,
## and infomation displayed (which and where)
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)
