setwd("D:/Rdata")
library(e1071)
accidents.df <- read.csv("accidents.csv")

accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")

## 나이브 베이즈를 적용하기 위해서는 모든 factor형 이어야 한다.
for( i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}
str(accidents.df)

## 나이브 rule 적용 (상대빈도 테이블 만듬)
prop.table(table(accidents.df$INJURY))

## 3차원 테이블 작성
## WEATHER_R : 1-보통 2-rain,snow
## TRAF_CON_R : 0-아무것도 없음 1-신호 2-표지판, 경찰관
table(accidents.df[1:12, c("INJURY", "WEATHER_R", "TRAF_CON_R")])

## 2차원 테이블 작성 2가지 경우
table(accidents.df[1:12, c("INJURY", "WEATHER_R")])
table(accidents.df[1:12, c("INJURY", "TRAF_CON_R")])

## 맨 위 12개의 데이터만 출력하여 '정확한 베이즈 분류기' 적용
accidents.df <- head(accidents.df[, c("INJURY", "WEATHER_R", "TRAF_CON_R")], 12)

## 12개 레코드에 대해서 나이브베이즈 분류기를 돌려 결과에 대한 정오분류표를 작성
## 하나의 반응변수 Y:INJURY | 두개의 예측변수 X1:WEATHER_R, X2:TRAF_CON_R
accidents.nb <- naiveBayes(INJURY~., data=accidents.df)
accidents.nb

library(caret)
pred.class <- predict(accidents.nb, newdata = accidents.df)
confusionMatrix(pred.class, accidents.df$INJURY) ## 예측값:pred.class / 실제값:accidents.df$INJURY
