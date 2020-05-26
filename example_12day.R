# 
# 11일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 차원 축소( dimension reduction )
#   2.2 단순선형 회귀분석( simple linear regression analysis )
#   2.3 조별 1차 프로젝트
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 차원 축소( dimension reduction )
#
# 고차원의 데이터에서는 관측 step도 기하급수적으로 증가하고 메모리 문제가 생긴다.
#
# 차원의 저주(Curse of dimensionality) : 차원이 증가하면 그것을 표현하기 위한 데이터
#                                        양이 기하급수적으로 증가한다는 것입니다.
# 
# 차원 축소 (Dimensionality reduction) : 데이터에서 불필요한 feature를 제거하는 작업
#                                 데이터의 의미를 제대로 표현하는 특징을 추려내는 것
#
# 주성분 분석 (Principal Component Analysis, PCA): 데이터의 분포를 가장 잘 표현하는
#                                                  성분을 찾아주는 것
# 데이터의 분산(Variance)을 최대한
# 보존하면서 서로 직교하는 축을 찾아 고차원의
# 공간의 표본을 저 차원으로 변환하는 차원 축소 기법
#
# Stochastic Neighbor Embedding(SNE)란 고차원의 원공간에 존재하는 데이터 x의 이웃 간의
#   거리를 최대한 보존하는 저차원의 y를 학습하는 방법론
# stochastic이란 이름이 붙은 이유는 거리 정보를 확률적으로 나타내기 때문
#
#
# 산점도는 2차원 평면상에 두 변수의 값으로 좌표를 정하여
# 위치를 나타내는 방법으로 데이터의 분포를 관찰할 수 있는
# 시각화 도구
#
# 산점도의 한계는 변수가 두 개인 경우, 즉 2차원 데이터에
# 대해서만 그래프를 그릴 수 있다는 것이다.
#
# 차원 축소( dimension reduction ) : 고차원 데이터를 2, 3차원
#      데이터로 축소하는 기법이다.
#      2, 3차원으로 축소된 데이터로 산점도를 작성하여 데이터
#      분포를 확인하면 고차원상의 데이터 분포를 추정할 수 있다.
#
# 3차원상의 물체에 빛을 비추면 2차원 평면에 물체의 그림자가 생기는
# 것과 비슷한 방법( 3차원이 2차원으로 축소됨 )으로 진행
#
# 고차원의 데이터를 저차원으로 축소하면 어쩔 수 없이 정보의 손실이 발생
# 고차원상의 데이터는 눈으로 분포를 관찰할 수 없기 때문에 정보의 손실을
#   감수하고라도 저차원상으로 축소하여 분포를 관찰하는 것은 의미가 있다.
#
# R에서는 t-sne( 티스니 ) 방법으로 차원 축소 진행
install.packages( "Rtsne" ) # t-sne 알고리즘이용 

library( Rtsne )
library( ggplot2 )

ds <- iris[ , -5 ] # 품종 제거

# 중복 데이터 제거
dup <- which( duplicated( ds ) )
dup

ds <- ds[ -dup, ]
ds.y <- iris$Species[ -dup ] # 중복 제외한 품종 정보

# t-sne 실행
#
# Rtsne( ds,                # 차원 축소 대상 dataset
#        dims = 2,          # 축소할 차원, 2 or 3
#        perplexity = 10 )  # 차원 축소 과정에서 데이터를 샘플링하는 샘플 수  
#                           # ( 대상 데이터의 행의 수 ) / 3 보다 작게 지정
tsne <- Rtsne( ds, dims = 2, perplexity = 10 )
tsne

# 차원 축소 결과 시각화
df.tsne <- data.frame( tsne$Y )
head( df.tsne )

ggplot( df.tsne, aes( x = X1, y = X2, color = ds.y ) ) +
  geom_point( size =2 )

# 4차원 데이터를 3차원 산점도로 작성
#
# 3차원 산점도 : Rtsne 패키지를 이용하여 ds를 3차원 데이터로 축소하고,
#                3차원 산점도로 표현
# 3차원 산점도를 이용하기 위해 rgl, car 패키지 설치
install.packages( c( "rgl", "car" ) )

library( car )
library( rgl )
library( mgcv )

tsne <- Rtsne( ds, dims = 3, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )
head( df.tsne )

# 회귀면이 포함된 3차원 산점도
scatter3d( x = df.tsne$X1, y = df.tsne$X2, z = df.tsne$X3 )

# 회귀면이 없는 3차원 산점도
points <- as.integer( ds.y )
color <- c( 'red', 'green', 'blue' )
scatter3d( x = df.tsne$X1, y = df.tsne$X2, z = df.tsne$X3,
           point.col = color[ points ],
           surface = FALSE ) # 회귀면이 표시하지 않음


#
# 2.2 단순선형 회귀분석( simple linear regression analysis )
#
# Modeling : 현실 세계에서 일어나는 현상을 수학식으로 표현하는 행위
#
#                              (훈련 데이터)
# 데이터 과학에서 Modeling이란 수집한 data를 이용하여 최적의 모델을 찾아내는 과정
#
#
# 데이터 과학에서 독립변수( independent variable ) X를 
#                 설명변수( explanatory variable ), 특징(feature)
# 종속변수( dependent variable ) y를 반응변수( response variable ), 레이블(label)
# x가 입력되면 y를 맞추어야 하는 문제, y를 ground truth로 간주
#
# 회귀분석(Regreesion Analysis)
# 회귀 이론을 기초로 독립변수( 설명변수 )가 종속변수( 반응변수 )에 미치는 영향을
# 파악하여 예측모델을 도출하는 통계적 방법
#
# 예측모델을 도출할 때 회귀 이론에 기초하기 때문에 회귀모델이라 부른다.
#
# 회귀 모델에서는 독립변수와 종속변수 사이의 관계가 수학식의 형태로 표현되기
# 때문에 이 관계식을 회귀식( reqression equation )이라 한다.
#
# 회귀식에서 일반적으로 독립변수 X로, 종속변수 y로 표현
#
# 회귀모델을 구하는 이유는 회귀모델을 통해서 종속변수를 예측할 수 있기 때문
#
# 관찰된 연속형 변수들에 대해 두 변수 사이의 모형을 구한 뒤 적합도를 측정해 내는
# 분석방법
#
# 시간에 따라 변화하는 데이터나 어떤 영향, 가설적 실험, 
# 인과 관계의 모델링 등의 통계적 예측에 이용 될 수 있다.
#
# 

# 단순선형 회귀분석(simple linear regression analysis)
#   : 독립변수( X )와 종속변수( y ) 사이의 선형 관계를 파악하고 이를 예측에
#     활용하는 통계적 방법
#
# 독립변수와 종속변수와의 관계가 선형으로 표현
# 하나의 독립변수를 다루는 분석방법
#
# 단순선형 회귀모델의 회귀식( regression equation ) : y = WX + b  ( W, b는 상수 )
# W, b는 어떻게 찾을 수 있을까? -> 수집된 데이터를 관찰하여 관계식을 만들어야 하는 것
# x, y로 구성된 data를 이용하여 W, b를 찾아내는 모형
# 
# 최적의 모델을 찾는 과정
# 모델 : y = WX + b           -> W, b를 계수(매개변수)
# -> 수집된 데이터 X, y를 이용해 산점도를 작성, 회귀식은 산점도 상에서 X, y의 추세를
#    나타내는 선
# -> 현재 수집된 데이터의 추세를 가장 잘 반영할 수 있는 W와 b를 찾는 것인
#    단순 선형 회귀의 목표
#
# 1. 모델 선택 -> 선형 방정식 선택
# 2. 주어진 data (훈련data)를 적용하여 매개변수 결정
# 3. 예측은 훈련 data에 없는 새로운 data로 모델이 레이블을 추정하는 과정
# 4. 완성된 모델에 대한 품질 평가

# 주행거리와 제동거리 사이의 회귀모델
str( cars )
head( cars )

# 산점도를 통한 선형 관계 확인
plot( dist~speed, data = cars )
plot( cars )

# 회귀모델 구하기,
# 종속( 반응 ) 변수 ~ 독립( 설명 ) 변수 순서로 지정
model <- lm( dist~speed, cars )
model

# lm() 
#     cost 함수 : 회귀선과의 거리( 차 ) 계산
#     Gradiant descent( 경사 하강법 ) : cost함수를 최소화 하는 함수(기울기 계산, 미분 )
#
# Coefficients: 매개변수, 계수
#     b에 해당        W에 해당
#   (Intercept)        speed  
#     -17.579          3.932
#
# dist = 3.932 x speed - 17.579
#

abline(model)                           # 회귀선

coef(model)                             # 매개변수(계수) _ w, b값 출력
coef( model )[ 1 ]                      # b값
coef( model )[ 2 ]                      # W값


# 주행속도에 따른 제동거리 구하기
b <- coef(model)[1]
b
W <- coef(model)[2]
W

speed <- 21.5
dist <- W * speed + b
dist

speed <- 30
dist <- W * speed + b
dist

speed <- 35
dist <- W * speed + b
dist

speed <- 40
dist <- W * speed + b
dist

# 여기서 구한 단순선형 회귀모델을 예측에 활용할 때 기억할 사항
#   예측한 값이 실제값과 오차가 있을 수 있다는 것
# 단순선형 회귀식은 예측값과 실제값사이의 오차를 최소화하는 식이지만 오차가 없을수
#   없다
#   따라서 회귀식을 통해 구한 값은 실제와 어느 정도 오차가 있을 수 있는 '예측값'
#   이라는 사실을 기억하고 실제 업무에 활용해야 한다.

df <- data.frame(speed=c(21.5, 25.0, 25.5, 26.0, 26.5, 27.5, 28.0))

predict(model, df)                                                    #예측 수행 함수

plot(df$speed, predict(model, df), col = 'red', cex=2, pch=20)
abline(model)


# 예상 제동거리, 실제 제동거리, 오차 구하기
str(cars)
speed <- cars[ ,1]
pred <- W * speed + b
pred

compare <- data.frame(pred, cars[,2],
                      pred-cars[,2])
compare

colnames(compare) <- c('예상', '실제', '오차')

head(fitted(model), 3)       #예측
head(residuals(model), 3)    #추정된 값과의 차이
head(compare, 3)



cars

summary(model)

fitted(model)                           #훈련data에 있는 샘플에 대한 예측값
residuals(model)                        #잔차 : 회귀식으로 추정된 값과의 차이
deviance(model)/length(cars$speed)      #잔차 제곱합을 평균제곱오차(MES-mean squared error)로 변환

# 데이터의 크기는 평균은 클수록, 분산은 작을수록, 데이터 크기가 클수록 믿음이 커진다. 
# 이들 세 가직 값을 묶늑 값을 t-통계량(t-statistics) 또는 t-값(t-value)
#
# t-값이 크면 대립가설에 대한 믿음이 강해지고 귀무가설에 대한 믿음은 약해진다.
# t-값이 작으면 대립가설에 대한 믿음이 약해지고 귀무가설에 대한 믿음은 강해진다.
#
# 데이터를 통해'대립가설이 통계학적으로 유의미하다'라는 것을 
# 증명하고 확인하는 작업을 t-검정(t-test)이라 한다.
#
#'귀무가설이 참이라고 가정했을 때, 표본으로부터 얻어지는 통계치가 나타날 (관측될)
# '확률'을 계산하는데 이때 계산된 확률값을 p값이라 한다.
# p값이 매우 낮으면, 이러한 표본 통계값은 우연히 나타나기 어려운 케이스이기 때문에, 
# 우리는 귀무가설을 채택하지 않고(기각하고), 대안적인 가설인 가설, 즉 대립가설을 채택한다.




# > summary(model)
# Call:
#   lm(formula = dist ~ speed, data = cars)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -29.069  -9.525  -2.272   9.215  43.201 
# 
# Coefficients:
#           Estimate Std.  Error       t      value Pr(>|t|)    
# (Intercept) -17.5791     6.7584    -2.601   0.0123   *  
#   speed      3.9324      0.4155     9.464   1.49e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

# 귀무 가설(歸無假說, 영어: null hypothesis, 기호 H0) 또는 영 가설(零假說)은 통계학에서
# 처음부터 버릴 것을 예상하는 가설이다. 차이가 없거나 의미있는 차이가 없는 경우의
# 가설이며 이것이 맞거나 맞지 않다는 통계학적 증거를 통해 증명하려는 가설이다. 
# 예를 들어 범죄 사건에서 용의자가 있을 때 형사는 이 용의자가 범죄를 저질렀다는 추정인
# 대립가설을 세우게 된다. 이때 귀무가설은 용의자는 무죄라는 가설이다.

# 대립 가설(對立假說, 영어: alternative hypothesis, 기호 H1) 또는 연구 가설 또는 
# 유지 가설은 귀무가설에 대립하는 명제이다. 
# 보통, 모집단에서 독립변수와 결과변수 사이에 어떤 특정한 관련이 있다는 꼴이다. 
# 어떤 가능성에 대해 확률적인 가설검정을 할 때 귀무가설과 함께 사용된다. 
# 이 가설은 귀무가설처럼 검정을 직접 수행하기는 불가능하며 귀무가설을 기각함으로써
# 받아들여지는 반증의 과정을 거쳐 받아들여질 수 있다.

str(cars)
head(cars)
car_model <- lm(dist~speed, data= cars)
coef(car_model)
plot(car_model);abline(car_model,col='red')
summary(car_model)

str(women)
head(women)
women_model <- lm(weight~height, data= women)
coef(women_model)
plot(women_model)
abline(women_model, col ='red')
summary(women_model)
