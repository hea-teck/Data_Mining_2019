setwd("D:Rdata")
car.df<-read.csv("ToyotaCorolla.csv")
car.df <- car.df[1:1000,] ## 1행부터 1000행까지만 이용
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18) ## 11개의 예측변수 사용 
## Fuel Type은 범주형, 반드시 이진변수로 전환되어야 함

set.seed(1)
train.index <- sample(c(1:1000), 600) ## 60% 훈련데이터 사용
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

## lm은 자동으로 dummy variable 생성하여 적용
## Fuel_TypeDiesel, Fuel_TypePetrol만 사용 / Fuel_TypeCNG 미사용
car.lm <- lm(Price~. , data=train.df)
options(scipen = 999) ## 지수표기법X
summary(car.lm) ## p-value는 계수 값들이 의미가 있는지 없는지를 파악할 수 있도록 도와준다.

## Make the Predictions for the Validation Data (예측과 정확성 측정을 위한 코드)
library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
options(scipen = 999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20], "Residual" = some.residuals)

## How Well did the Model Do With the Vallidation Data?
accuracy(car.lm.pred, valid.df$Price)

## ME = mean error / RMSE = root mean squared error / MAE = mean absolute error
## MPE = mean percent error / MAPE = mean absolute percent error

## 검증 오차의 히스토그램을 작성하기 위한 코드
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)]) / 400
hist(all.residuals, breaks = 25, xlab = "Residuals")

## 예측변수 감소를 위한 전역탐색 결과 / 최적의 모델 선택을 위한 코드
## Exhaustive search requires library leaps and manual coding into binary dummies
## Use regsubsets() in package leaps
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = train.df))
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which ## show medels
sum$reg ## show metrics
sum$adjr2 ## show metrics 8번째 값에서 최대치 달성, 이후 안정화 단계. 즉 7개의 예측변수를 사용하는 것이 좋은 모델 형성. 즉 회귀모델의 계수의 수는 9
sum$cp ## show metrics  [1] 403.447981 254.334352 114.044327  46.102024  23.718705   8.210361   5.211719   5.954440   7.559869   9.103921  11.000000 / Mallow's Cp는 7~8개 예측변수를 사용하면 모델이 좋다.

############################## 초기 예측변수없이 시작. 전방선택 (Forward Selection)
car.lm.null <- lm(Price~1, data = train.df) ## Price 예측변수 하나로 시작하겠다는 의미.
car.lm.step <- step(car.lm.null, scope = list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step) ## 출력되는 예측변수로 몇개의 예측변수를 사용해야 좋은모델인지 파악 가능 (Intercept)는 제외할 것.

############################# 모든 예측변수를 사용하는 것으로 시작. 후방제거 (Backward Elimination)
car.lm.step <- step(car.lm, direction = "backward") ## car.lm이 모든 예측변수를 사용한다는 의미.
summary(car.lm.step)

############################# Stepwise 단계적 선택
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)






