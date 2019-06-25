setwd("D:/Rdata")
df <- read.csv("Assignment.csv", header = TRUE)
str(df)

library(gains)
dim(df)
gain<-gains(df$actual, df$propensity,)
barplot(gain$mean.resp/mean(df$actual), names.arg=gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")
