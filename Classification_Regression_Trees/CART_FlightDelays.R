setwd("D:/Rdata")
delay.df <- read.csv("FlightDelays.csv")
str(delay.df)

## DAY_WEEK 변수를 범주형 변수로 변경
delay.df$DAY_WEEK <- factor(delay.df$DAY_WEEK)
