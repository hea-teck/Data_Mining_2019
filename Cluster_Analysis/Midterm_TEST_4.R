getwd()
setwd("D:/Rdata")
utilities.df <- read.csv("Utilities.csv")
utilities.df

row.names(utilities.df) <- utilities.df[,1] ##첫번째 열 복사해서 행이름으로 변경 
utilities.df

utilities.df <- utilities.df[,-1] ##회사명 열을 삭제함. 즉 문자열이 다 없어짐
utilities.df

d <- dist(utilities.df, method ="euclidean") ##유클리드 거리 측
d ##21x21행렬. 자기 자신과의 거리는 제외!! 알아서 제외시키고 계산한다!!

utilities.df.norm <- sapply(utilities.df, scale) ##모든 열 방향으로 정규화 작업 진행 
utilities.df.norm

row.names(utilities.df.norm) <- row.names(utilities.df)
utilities.df.norm

d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")
d.norm ##21x21 행렬에서 c(6,8) 부분만 뽑아내서 거리를 측정한다.

#######################################################################

d <- matrix(1:9, nrow=3)
d <- matrix(1:9, ncol=3)
d

########################

result <- lapply(1:3, function(x) { x*2 })
result
unlist(result)

########################

d <- as.data.frame(matrix(unlist(lapply(iris[,1:4], mean)), ncol=4, byrow=TRUE))
d

names(d) <- names(iris[, 1:4])
d

d <- as.data.frame(unlist(lapply(iris[,1:4], mean)))
d

lapply(iris[, 1:4], mean)

sapply(iris[, 1:4], mean)
class(sapply(iris[, 1:4], mean))

##########################################################
d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm

hc1 <- hclust(d.norm, method="single")
plot(hc1, hang=-1, ann=FALSE)

hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-1, ann=FALSE)
plot(hc2, hang=-1)

memb <- cutree(hc1, k=6)
memb

memb <- cutree(hc2, k=6)
memb

############################################################

row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, col=rev(paste("gray", 1:99, sep="")))

############################################################

km <- kmeans(utilities.df.norm, 6)
str(km)

km$cluster
km$centers
km$withinss
km$size

plot(c(0), xaxt = 'n', ylab = "", type ="l", ylim =c(min(km$centers), max(km$centers)), xlim=c(0,8))

axis(1, at = c(1:8), labels=names(utilities.df))

for (i in c(1:6))
lines(km$centers[i,], lty=i, lwd=2, col=ifelse(i %in% c(1,3,5), "black", "dark grey"))

text(x=0.5, y=km$centers[,1], labels=paste("cluster", c(1:6)))

dist(km$centers)