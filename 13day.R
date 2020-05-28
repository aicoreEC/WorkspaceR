# 
# 11일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 다중선형 회귀분석( multiple linear regression analysis )
#   2.2 로지스틱 회귀분석( logistic regression analysis )
#   2.3 조별 1차 프로젝트
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#
install.packages( "tidyverse" )

#
# 2.1 다중선형 회귀분석( multiple linear regression analysis )
#
# 다중선형 회귀모델 : 여러개의 독립변수를 다루는 회귀모델
#                     어떤 결과에 영향을 미치는 요인이 하나이기
#                     보다는 여러 개인 경우가 대부분이기 때문에
#                     다중선형 회귀분석( 모델 )에 대한 이해는 중요
#
# 회귀식 : y = W1X1 + W2X2 + ... + WnXn + b
#
# 독립변수가 n개인 다중선형 회귀에서 주어진 자료를 이용해
# W1, W2, ... , Wn, b의 값을 알아내는 회귀모델
#
# R에서 다중선형 회귀모델 생성에도 lm() 함수 사용
library( tidyverse )

install.packages( "car" )
library( car )

str( Prestige )
head( Prestige )
# education : 교육연수( 독립 ), income : 연봉( 종속 )
# women : 여성 비율( 독립 ), prestige : 직군에 대한 평판( 종속 )

newdata <- Prestige[ , c( 1:4 ) ]
head( newdata )
plot( newdata, pch = 16, col = 'blue', 
      main = 'Matrix Scatter plot' )

c <- data.frame( Prestige$education,
                    Prestige$income,
                    Prestige$women,
                    Prestige$prestige )
cor( c )

# 다중선형 회귀모델 생성
model <- lm( income~education + prestige + women, data = newdata )
model

# 회귀모델의 결과를 이용한 회귀식
income = ( -253.8 ) + ( 177.2 * newdata$education + 
                        141.4 * newdata$prestige -
                        50.9 * newdata$women )
income

# 다중선형 회귀모델에 대한 값 확인
coef( model )         # 매개변수( 계수 )  값 확인 - W값, b값
fitted( model )       # 훈련 집합에 있는 샘플에 대한 예측값
residuals( model )    # 잔차
deviance( model )     # 잔차 제곱합
deviance( model ) / length( newdata$education ) # 평균제곱오차(MSE)

# 생성된 다중선형 회귀모델에 대한 요약
summary( model )

#
# 다중선형 회귀모델에서 변수 선택
# : 종속변수를 설명하는 데 도움이 되는 독립변수가 다수 존재한다.
#   그런데 모든 독립변수가 종속변수를 설명하는 데 동일하게 기여하는
#   것은 아니다.
#   어떤 변수는 기여도가 높고 어떤 변수는 기여도가 낮다.
#
#   기여도가 낮거나 거의 없는 변수들은 모델에서 제외하는 것이 좋다.
#   적은 변수를 이용해 현실을 잘 설명할 수 있는 것이 좋은 모델이기
#   때문이다.
#
# R에서 모델에 기여하는 변수들을 선별할 수 있는 stepAIC() 함수 제공
newdata2 <- Prestige[ , c( 1:5 ) ]
model2 <- lm( income~., data = newdata2 )
summary( model2 )

library( MASS )

model3 <- stepAIC( model2 )
model3

summary( model2 )
# Multiple R-squared:  0.6436,	Adjusted R-squared:  0.6289 
summary( model3 )
# Multiple R-squared:   0.64,	Adjusted R-squared:  0.6327 






#
# 2.2 로지스틱 회귀분석( logistic regression analysis )
#
# 회귀모델에서 종속 변수의 값의 형태가 범주형 값인 경우를 다루기
# 위해서 만들어지 통계적 모델
#
# 주어진 데이터로부터 어떤 범주를 예측하는 분야를 회귀와 구분하여
# 분류( classification )라 하고 분류 문제를 회귀 방법으로 해결하
# 고자 개발된 회귀분석
#
# 로지스틱 회귀도 기본적으로 회귀기법이기 때문에 종속변수는 숫자로
# 표현되어야 한다.
#
# 예) YES와 NO는 0과 1로 변환
# 예) setoda, versicolor, verginica는 1, 2, 3과 같이 숫자로 바꾼
#     후에 로지스틱 회귀 적용
#
iris.new <- iris
# 범주형 자료를 정수로 변환
iris.new$Species <- as.integer( iris.new$Species )
head( iris.new )
tail( iris.new )

# 로지스틱 회귀모델 생성
iris_model <- glm( Species~., data = iris.new )
iris_model

# 매개변수 확인
coef( iris_model )

# 로지스틱 회귀 모델 요약
summary( iris_model )

# 로지스틱 회귀 모델을 통한 분류
unknown <- data.frame( rbind( c( 5.1, 3.5, 1.4, 0.2 ) ) )
names( unknown ) <- names( iris )[ 1:4 ]
unknown

t = 1.18649525 - ( 0.11190585 * 5.1 ) -
                ( 0.04007949 * 3.5 ) +
                ( 0.22864503 * 1.4 ) +
                ( 0.60925205 * 0.2 )
t
pred <- predict( iris_model, unknown ) # 품종 예측
pred
round( pred, 0 ) # 예측 결과( 소수 첫째 자리에서 반올림 )

# 예측값에 대한 실제 품종 이름 알아보기
pred <- round( pred, 0 )
# levels() : 펙터 타입의 자료에 대해 어떤 종류 값인지 출력
levels( iris$Species )[ pred ] 

# 다수 데이터에 대한 예측
test <- iris[ , 1:4 ]

pred <- predict( iris_model, test )
pred <- round( pred, 0 )
pred

# 예측 정확도 확인
answer <- as.integer( iris$Species )
pred == answer
acc <- mean( pred == answer )
acc










#
# 2.3 조별 1차 프로젝트
#







