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


#
# 2.1 다중선형 회귀분석( multiple linear regression analysis )
#
# 다중선행 회귀모델 : 여러개의 독립변수를 다루는 회귀모델
#                     어떤 결과에 영향을 미치는 요인이 하나이기 보다는 여러 개인
#                     경우가 대부분이기 때문에 다중선형 회귀분석에 대한 이해는 중요
# 회귀식
# y = W1X1 + W2X2 + W3X3 + ... + WnXn + b
#
# 독립변수가 n개인 다중선형 회귀에서 주어진 자료를 이용해
# b, W1, W2, W3, ... , Wn의 값을 알아내는 회귀모델
#
# R에서 다중선형 회귀모델에도 lm() 함수 사용
#
library( tidyverse )
library( car )

str( Prestige )
head( Prestige ) # education : 교육연수( 독립 ), income : 연봉( 종속 )
                 # women : 여성 비율( 독립 ), prestige : 직군에 대한 평판도( 독립 )

newdata <- Prestige[ , c( 1:4 ) ]   # 회귀식 작성을 위한 데이터 준비
head( newdata )
plot( newdata, pch=16, col= 'blue',
      main = 'Matrix Scatterplot' ) # 산점도를 통해 변수 간 관계 확인
# 교육연수( education )와 평판도( prestige )는 연봉( income )과 양의 상관 관계
# 여성 비율( women )은 연봉( income )과 음의 상관 관계

c <- data.frame( Prestige$education, Prestige$income, Prestige$women, Prestige$prestige )
cor( c ) # 상관 계수

# 종속변수~독립변수 + 독립변수 + ...
model <- lm( income~education + prestige + women, data = newdata ) # 회귀식 도출
model

#Call:
#  lm(formula = income ~ education + prestige + women, data = newdata)
#
#Coefficients:
#      b값           W1값          W2값          W3값
#  (Intercept)    education     prestige        women  
#     -253.8        177.2        141.4          -50.9  

# 회귀식에 따른 회귀 모델
income = ( -253.8 ) + ( 177.2 * newdata$education  + 141.4 * newdata$prestige  -  50.9  * newdata$women )
income

coef( model )    # 매개변수( 계수 ) 값 확인, W값, b값
fitted(model)    # 훈련 집합에 있는 샘플에 대한 예측값
residuals(model) # 잔차 
deviance(model)  # 잔차 제곱합
deviance(model) / length(newdata$education)   # 평균 제곱 오차

summary(model)

#
# Call:
#lm(formula = income ~ education + prestige + women, data = newdata)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7715.3  -929.7  -231.2   689.7 14391.8 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347    
# prestige     141.435     29.910   4.729 7.58e-06  *** -> income을 설명하는데 
#  women        -50.896      8.556  -5.948 4.19e-08 ***    얼마나 중요한 변수이지
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2575 on 98 degrees of freedom
#Multiple R-squared:  0.6432,	Adjusted R-squared:  0.6323 -> 모델의 설명력 0~1사이 값
#                             이 값이 클수록 회귀모델이 현실을 잘 설명할 수 있다는 뜻
#F-statistic: 58.89 on 3 and 98 DF,  p-value: < 2.2e-16
#                                    회귀모델이 의미있는 모델인지, 혹은 신뢰할 수 있는
#                                    모델인지를 나타내는 값, p-value < 0.05이면 이 모델의
#                                    신뢰수준이 95% 이상임을 나타낸다


# 다중선형 회귀모델에서 변수 선택
# : 종속변수를 설명하는 데 도움이 되는 독립변수가 다수 존재한다.
#   그런데 모든 독립변수가 종속변수를 설명하는 데 동일하게 기여하는 것은 아니다.
#   어떤 변수는 기여도가 높고 어떤 변수는 기여도가 낮다.
#
#   기여도가 낮거나 거의 없는 변수들은 모델에서 제외하는 것이 좋다.
#   적은 변수를 이용해 현실을 잘 설명할 수 있는 것이 좋은 모델이기 때문
#
# R에서 모델에 기여하는 변수들을 선별할 수 있는 stepAIC() 함수 제공
#
newdata2 <- Prestige[ , c(1:5) ]
model2 <- lm( income~., data = newdata2 )             # '.'은 이하 모두 선택이란 뜻
summary( model2 )

library( MASS )                 # setpAIC() 포함

model3 <- stepAIC( model2 )     # 기여도가 높은 변수 선택, 불필요한 변수 제거해 나가는 방식
model3                          # 기여도가 높은 변수 결과

# 회귀식
# income = 431.57 + ( 165.87 x prestige ) - ( 48.38 x women )
#
summary( model3 )               # 회귀 모델 상세 내용
#
# 설명력
# Multiple R-squared:   0.64,	Adjusted R-squared:  0.6327 
#

summary( model2 )
#
# 설명력
# Multiple R-squared:  0.6436,	Adjusted R-squared:  0.6289 
#

#
# 2.2 로지스틱 회귀분석( logistic regression analysis )
#
# 회귀모델에서 종속 변수의 값의 형태가 범주형 값인 경우를 다루기 위해서 만들어진
# 통계적 방법
#
# 주어진 데이터로부터 어떤 범주를 예측하는 분야를 회귀와 구분하여 
# 분류(classification)라고 하고 분류 문제를 회귀 방법으로 해결하고자 개발된 것
#
# 로지스틱 회귀도 기본적으로 회귀기법이기 때문에 종속변수는 숫자로 표현되어야 한다.
#
# 예) YES와 NO는 0과 1로 serosa, versicolor, virginica는 
# 1,2,3과 같이 숫자로 바꾼 후에 로지스틱 회귀 적용
iris.new <- iris
iris.new$Species <- as.integer( iris.new$Species ) # 범주형 자료를 정수로 변환
head( iris.new )

iris_model <- glm( Species~., data = iris.new ) # 로지스틱 회귀보델 도출
iris_model

# Call:  glm(formula = Species ~ ., data = iris.new)
#
# Coefficients:
#   (Intercept)  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width  
#    1.18650      -0.11191      -0.04008       0.22865       0.60925  
#
#
# Degrees of Freedom: 149 Total (i.e. Null);  145 Residual
# Null Deviance:	    100 
# Residual Deviance: 6.961 	AIC: -22.87
#
coef( iris_model )
#
# 회귀식 : Species = 1.18650 - ( 0.11191 x Sepal.Length ) - ( 0.04008 x Sepal.Width )
#                    + ( 0.22865 x Petal.Length ) + ( 0.60925 x Petal.Width )
#

summary( iris_model )

# Call:
# glm(formula = Species ~ ., data = iris.new)
#
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.59215  -0.15368   0.01268   0.11089   0.55077  
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|) 모델 중요도    
# (Intercept)     1.18650    0.20484   5.792 4.15e-08 ***
#   Sepal.Length -0.11191    0.05765  -1.941   0.0542 .   -> 기여도가 낮음
#   Sepal.Width  -0.04008    0.05969  -0.671   0.5030     -> 거의 도움이 되지 않는 변수
#   Petal.Length  0.22865    0.05685   4.022 9.26e-05 ***
#   Petal.Width   0.60925    0.09446   6.450 1.56e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.04800419)
#
# Null deviance: 100.0000  on 149  degrees of freedom
# Residual deviance:   6.9606  on 145  degrees of freedom
# AIC: -22.874
#
# Number of Fisher Scoring iterations: 2
#

# 로지스틱 회귀모델을 이용한 예측
unknown <- data.frame( rbind( c( 5.1, 3.5, 1.4, 0.2 ) ) )
names( unknown ) <- names( iris )[ 1:4 ]
unknown # 예측 대상 데이터

# Species = 1.18650 - ( 0.11191 x Sepal.Length ) - ( 0.04008 x Sepal.Width )
#           + ( 0.22865 x Petal.Length ) + ( 0.60925 x Petal.Width )
#         = 1.18650 - ( 0.11191 x 5.1 ) - ( 0.04008 x 3.5 )
#           + ( 0.22865 x 1.4 ) + ( 0.60925 x 0.2 )
#         = 0.917439 -> 소수 첫째 자리에서 반올림한 값이 예측값이된다.
# 로지스틱 회귀식에 의한 결고는 실수가 도출된다.
# y = 1 / 1 + e(지수 -1승) -> sigmoid 함수
#

pred <- predict( iris_model, unknown ) # 품종 예측, predict() 회귀모델에 예측을 원하는
                                       # 데이터를 넣어 결과를 예측하는 역활
pred             # 예측 결과 출력
round( pred, 0 ) # 예측 결과 출력( 소수 첫째 자리에서 반올림 )

# 실제 품종명 알아보기
pred <- round( pred, 0 )
levels( iris$Species )[ pred ] # levels()는 팩터 타입의 자료에 대해 어떤 종류 값인지출력

# 다수의 데이터에 대한 예측
test <- iris[ , 1:4 ] # 예측모델을 만들 때 사용하는 데이터를 훈련 데이터( training data )
                      # 예측 대상이 되는 데이터를 테스트 데이터( test data )

pred <- predict( iris_model, test )  # 모델을 이용한 예측
pred <- round( pred, 0 )
pred                                 # 예측 결과 출력

answer <- as.integer( iris$Species ) # 실제 품종 정보
pred == answer                       # 예측 품종과 실제 품종이 같은지 비교
acc <- mean( pred == answer )        # 예측 정확도 계산
       # 예측 정확도는 바르게 예측한 데이터의 개수를 전체 예측 데이터의 개수로 나눈 것
acc                                  # 예측 정확도 출력

plot( iris_model, pch=16, col= 'blue', main = 'Matrix Scatterplot' )

# 군집화(clustering) / 분류(classification)
#
# 군집화(clustering) : 주어진 대상 데이터들을 유사성이 높은 것끼리 
#                      묶어주는 기술(군집, 범주, 그룹)
#
# k-means (평균) 군집화 알고리즘
mydata <- iris[ , 1:4]
fit <- kmeans(x=mydata, centers = 3)
fit

# K-means clustering with 3 clusters of sizes 62, 50, 38
# 
# Cluster means:   ->                                         #3개 군집의 중심점 좌표
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.901613    2.748387     4.393548    1.433871
# 2     5.006000    3.428000     1.462000    0.246000
# 3     6.850000    3.073684     5.742105    2.071053
# 
# Clustering vector:                                        #->각 데이터에 대한 군집번호
#   [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 3 1 1 1
# [57] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 1 3 3 3 3 3
# [113] 3 1 1 3 3 3 3 1 3 1 3 1 3 3 1 1 3 3 3 3 3 1 3 3 3 3 1 3 3 3 1 3 3 3 1 3 3 1
# 
# Within cluster sum of squares by cluster:
#   [1] 39.82097 15.15100 23.87947
# (between_SS / total_SS =  88.4 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
# [8] "iter"         "ifault"      
# > 

fit$cluster
fit$centers

library(cluster)            #차원축소 후 군집 시각화 패키지
clusplot(mydata,            # 군집대상
         fit$cluster,       # 군집번호
         color=T,           # 원의 색
         shade = T,         # 원의 빗금표시 유무
         labels=1,          # 관측값 출력 형태
         lines=0)           # 중심선 연결 표시
subset( mydata, fit$cluster==2)

# 대상 데이터 표준화 후 군집화

# 데이터와 데이터의 거리를 계산할 때 발생하는 문제
# 모든 변수가 거리 계산에 동등한 영향을 갖도록 하기 위해서 모든 변수의 자료 범위를 0~1 사이로
#표준화한 후 거리 계산을 한다.

# (x-min(A)) / (max(A)-min(A))
# x : 변수 A의 임의의 관측값
# max(A), min(A)는 변수 A 관측값 중 최대 / 최소값

std <- function(x){
  return((x-min(x)) / (max(x)-min(x)) )
}
mydata <- apply(iris[ , 1:4], 2, std)
fit <- kmeans(x=mydata, centers = 3)
fit


#
# KNN( K-Nearest Neighbor, k-최근접 이웃) 분류 알고리즘
#
library(class)

# 훈련용/ 테스트용 데이터 준비
tr.idx <- c(1:25, 51:75, 101:125)
ds.tr <- iris[tr.idx, 1:4]                      #훈련용
ds.ts <- iris[-tr.idx, 1:4]                     #테스트용
cl.tr <- factor(iris[tr.idx, 5])                #훈련용 그룹정보
cl.ts <- factor(iris[-tr.idx, 5])               #테스트용 그룹정보
pred <- knn(ds.tr, ds.ts, cl.tr, k=3, prob = T)
pred
acc <- mean(pred==cl.ts)
acc
table(pred, cl.ts)

#
#교차 검증 방법(K-fold cross validation)

install.packages("cvTools")
library(cvTools)

k = 10
folds <-cvFolds(nrow(iris), K=k)

acc <- c()
for (i in 1:k) {
  ts.idx <- folds$which == i
  ds.tr <- iris[-ts.idx, 1:4]
  ds.ts <- iris[ts.idx, 1:4]
  cl.tr <- factor(iris[-ts.idx, 5])
  cl.ts <- factor(iris[ts.idx, 5])
  pred <- knn(ds.tr, ds.ts, cl.tr, k =5)
  acc[i] <- mean(pred == cl.ts)             #예측 정확도
}
acc                                         #폴드별 예측 정확도
mean(acc)                                   #폴드평균 예측 정확도


#
# 2.3 조별 1차 프로젝트
#