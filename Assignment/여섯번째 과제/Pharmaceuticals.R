setwd("D:/Rdata")
getwd()
Pharmaceuticals.df <- read.csv("Pharmaceuticals.csv")
row.names(Pharmaceuticals.df) <- Pharmaceuticals.df[,1]
Pharmaceuticals.df <- Pharmaceuticals.df[,-c(1,2)] ##3열부터 시작.
Pharmaceuticals.df

d <- dist(Pharmaceuticals.df, method = "euclidean")
d

Pharmaceuticals.df.norm <- sapply(Pharmaceuticals.df[,1:9], scale) ##1번부터 9번까지의 변수만을 사용.

row.names(Pharmaceuticals.df.norm) <- row.names(Pharmaceuticals.df)
d.norm <- dist(Pharmaceuticals.df.norm, method = "euclidean")

hc1 <- hclust(d.norm, method = "complete") ##method 변경해주면서 파악할 것. 
plot(hc1, hang = -1, ann = FALSE)

rect.hclust(hc1, k=4, border="red")
memb <- cutree(hc1, k=4)
memb

table(memb)
table(Pharmaceuticals.df$Median_Recommendation, memb)
table(Pharmaceuticals.df$Location, memb)
table(Pharmaceuticals.df$Exchange, memb)

row.names(Pharmaceuticals.df.norm)<-paste(memb, ": ", row.names(Pharmaceuticals.df), sep="")
heatmap(as.matrix(Pharmaceuticals.df.norm), Colv=NA, hclustfun=hclust, col=rev(paste("gray",1: 99,sep="")))


