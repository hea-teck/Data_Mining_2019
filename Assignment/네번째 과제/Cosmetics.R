setwd("D:/Rdata")
install.packages("arules")
library(arules)
fp.df <- read.csv("Cosmetics.csv")
str(fp.df)
# remove first column and convert to binary incident matrix format
fp.mat <- as.matrix(fp.df[, -1])

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions") #arules package 설치되어 있어야 함
inspect(fp.trans)

## get rules
# when running apriori(), include minimum support & confidence, & target as arguments.
rules <- apriori(fp.trans, parameter = list(supp = 0.1, conf = 0.5, target = "rules"))
inspect(head(sort(rules, by="lift"), n=10)) 

