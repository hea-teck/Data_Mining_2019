setwd("D:/Rdata")
owner.df <- read.csv("ownerExample.csv", header = TRUE)
str(owner.df)
head(owner.df)
install.packages("caret")
library(caret)
install.packages('e1071')
library(e1071)
#cutoff 0.5
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner')),
                owner.df$Class, positive = 'owner')
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner')),
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.25, 'owner', 'nonowner')),
                owner.df$Class)
