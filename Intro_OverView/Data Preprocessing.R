getwd()
setwd("D:/Rdata")
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
dim(housing.df) ##행과 열의 수 파악가능
head(housing.df) ##맨 위 6개만 출력
View(housing.df) ##새로운 탭이 생겨 데이터를 엑셀과 같이 보여준다.

## ':' 기호는 '~'로 해석 할 것 

housing.df[1:10, 1] ##1행부터 10행까지 1열의 데이터 값만 출력 
housing.df[1:10, ] ##1행부터 10행까지 모든 데이터 출력 
housing.df[5, 1:10] ##5행의 1열부터 10열까지의 데이터까지만 출력 
housing.df[5, c(1:2, 4, 8:10)] ##5행의 1열부터 2열 / 4열 / 8열부터 10열의 테이터만 출력 
housing.df[,1] ##1열의 데이터값 모두 출력 
housing.df$TOTAL.VALUE ## '$'뒤에 붙는 열의 이름에 따라 모든 데이터 값이 출력 
housing.df$TOTAL.VALUE[1:10] ## TOTAL.VALUE 열의 1행부터 10행까지의 데이터 값 출력 
length(housing.df$TOTAL.VALUE) ## TOTAL.VALUE 열의 길이 (즉 행의 개수와 동일, 데이터 몇개 있는지) 
mean(housing.df$TOTAL.VALUE) ## TOTAL.VALUE 열의 평균 계산 
summary(housing.df) ## 각 열의 모든 데이터값을 토대로 요약 (최소,최대,중간값, 평균 등) 

row.names(housing.df) ## 데이타프레임의 numeric 형태인 행의 이름 모두 출력) 

s <- sample(row.names(housing.df), 5) ## sample로 5개의 행 추출 
housing.df[s,] ## 위 샘플링 한 행 추출 

s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,] ##oversample 뒤에 0.9/0.01 확인 

names(housing.df) ##열변수 이름 출력 
t(t(names(housing.df))) ##행렬이 생성된다. 
colnames(housing.df)[1] <- c("TOTAL_VALUE") ##첫번째 열변수이름을 TOTAL_VALUE로 변경
names(housing.df)
class(housing.df$REMODEL) ##factor는 범주형 변수이다. 즉 REMODEL은 범주형 변수.
class(housing.df$BEDROOMS) ##BEDROOMS는 Integer
class(housing.df$TOTAL_VALUE) ##TOTAL_VALUE는 Numeric 
class(housing.df[,14])
levels(housing.df[,14]) ##값을 분리시켜 정리해준다.
######REMODEL은 범주형 변수이다. 3개의 level (None, Old, Recent) 에 대해서 2개의 가변수(Dummy Variable)을 만든다. 

xtotal <- model.matrix(~0 + BEDROOMS + REMODEL, data = housing.df) ##dummy variable을 만들어 준다. 
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)
xtotal <- xtotal[,-4]
head(xtotal)
housing.df <- cbind(housing.df[, -c(9,14)], xtotal)
housing.df
t(t(names(housing.df)))
t(t(names(xtotal)))
head(xtotal) ##lm함수를 이용하면 위와 같은 dummy variable을 생성시키는 과정이 필요하지 않다. 

rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA ##일부로 결측 데이터 생성 
summary(housing.df$BEDROOMS)
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS) ##중앙값으로 결측치를 대체 

#####################################################################

set.seed(1)

train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows, ]
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]

####################################################################

train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
train.data <- housing.df[train.rows, ]
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)
valid.data <- housing.df[valid.rows, ]
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))
test.data <- housing.df[test.rows, ]

######################################################################
##웨스트 록스베리 팍습데이터를 사용하여 모델을 구축하는 코드 
reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows)
tr.res <- data.frame(train.data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)
##웨스트 록스베리 검증데이터를 사용하여 모델을 검증하는 코드
pred <- predict(reg, newdata = train.data)
vl.res <- data.frame(train.data$TOTAL.VALUE, pred, residuals =
                       train.data$TOTAL.VALUE - pred)
##모델의 평가측도를 계산하는 코드
library(forecast)
accuracy(pred, train.data$TOTAL.VALUE)
########################################
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals =
                       valid.data$TOTAL.VALUE - pred)
accuracy(pred, valid.data$TOTAL.VALUE)
########################################
pred <- predict(reg, newdata = test.data)
vl.res <- data.frame(test.data$TOTAL.VALUE, pred, residuals =
                       test.data$TOTAL.VALUE - pred)
accuracy(pred, test.data$TOTAL.VALUE)


######## Test Complited. Good!













