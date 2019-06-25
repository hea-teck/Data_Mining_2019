getwd()
setwd("D:/Rdata")
install.packages("arules")
library(arules)

fp.df <- read.csv("Faceplate.csv")
fp.df

fp.mat <- as.matrix(fp.df[,-1])
fp.mat

fp.trans <- as(fp.mat, "transactions")
inspect(fp.trans)

rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf=0.5, target = "rules"))
inspect(head(sort(rules, by="lift"), n=6))

###########################################################
all.books.df <- read.csv("CharlesBookClub.csv")
str(all.books.df)

count.books.df <- all.books.df[,8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df[,-1])

books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

itemFrequencyPlot(books.trans, type="absolute")

rules <- apriori(books.trans, parameter=list(supp=200/4000, conf=0.5, target="rules"))
inspect(sort(rules, by="lift"))

##################################################################

library(recommenderlab)

m <- matrix(nrow = 1000, ncol = 100)
m[sample.int(100*1000, 1000)] <- ceiling(runif(1000, 0, 5))

r <- as(m, "realRatingMatrix")

UB.Rec <- Recommender(r, "UBCF", parameter="Cosine")
pred <- predict(UB.Rec, r, type="ratings")
as(pred, "matrix")
as(pred, "list")

IB.Rec <- Recommender(r, "IBCF", parameter="Cosine")
ipred <- predict(IB.Rec, r, type="ratings")
ipred <- predict(IB.Rec, r, n=5)
as(ipred, "matrix")
as(ipred, "list")
