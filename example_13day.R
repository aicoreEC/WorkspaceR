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
#
# 회귀식
# y = W1X1 + W2X2 + W3X3 + ... + WnXn + b

# 독립변수가 n개인 다중선형 회귀에서 주어진 자료를 이용해
# b, W1, W2, W3, ... , Wn의 값을 알아내는 회귀모델
library( tidyverse )
library( car )

str( Prestige )
head( Prestige )

newdata <- Prestige[ , c( 1:4 ) ]
head( newdata )
plot( newdata, pch=16, col= 'blue',
      main = 'Matrix Scatterplot' )

c <- data.frame( Prestige$education, Prestige$income, Prestige$women, Prestige$prestige )
cor( c )

model <- lm( income~education + prestige + women, data = newdata )
model
coef( model )

income = ( -253.8 ) + ( 177.2 * newdata$education  + 141.4 * newdata$prestige  -  50.9  * newdata$women )
income

fitted(model)
residuals(model)      
deviance(model)                               # 잔차
deviance(model) / length(newdata$education)   # 평균 제곱 오차

summary(model)

newdata2 <- Prestige[ , c(1:5) ]
model2 <- lm( income~., data = newdata2 )             # '.'은 이하 모두 선택이란 뜻
summary( model2 )

library( MASS )

model3 <- stepAIC( model2 )

summary( model3 )
summary( model2 )

#
# 2.2 로지스틱 회귀분석( logistic regression analysis )
#
# 회귀모델에서 종속 변수의 값의 형태가 범주형인 경우 예측모델
#
# 주어진 데이터로부터 어떤 범주를 예측하는 분야를 회귀와 구분하여 
# 분류(classification)이라고 한다.
#
# 로지스틱회귀도 기본적으로 회귀기법이기 때문에 종속변수는 숫자로 표현되어야 한다.
#
# 예) YES와 NO는 0과 1로 serosa, versicolor, virginica는 
# 1,2,3과 같이 숫자로 바꾼 후에 로지스틱 회귀 적용
iris.new <- iris
iris.new$Species <- as.integer( iris.new$Species )
head( iris.new )

iris_model <- glm( Species~., data = iris.new )
iris_model

coef( iris_model )
summary( iris_model )

unknown <- data.frame( rbind( c( 5.1, 3.5, 1.4, 0.2 ) ) )
names( unknown ) <- names( iris )[ 1:4 ]
unknown

pred <- predict( iris_model, unknown )
pred
round( pred, 0 )

pred <- round( pred, 0 )
levels( iris$Species )[ pred ]

test <- iris[ , 1:4 ]

pred <- predict( iris_model, test )
pred <- round( pred, 0 )

answer <- as.integer( iris$Species )
pred == answer
acc <- mean( pred == answer )
acc

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