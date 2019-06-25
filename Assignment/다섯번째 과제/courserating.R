setwd("D:/Rdata")
library(recommenderlab)
fp.df <- read.csv("courserating.csv")
str(fp.df)
fp.df
row.names(fp.df) <- fp.df[,1] 
fp.df
m <- as.matrix(fp.df[, -1]) 
m



r <- as(m, "realRatingMatrix") #"binaryRatingMatrix“

# item-based collaborative filtering
IB.Rec <- Recommender(r, "IBCF")
ipred <- predict(IB.Rec, r, n=5, type="topNList")
as(ipred, "matrix") 
as(ipred, "list")
