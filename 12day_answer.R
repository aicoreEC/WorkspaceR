#
# 12day_answer.R
#
# 12일차 단순선형 회귀분석 실습
#
#문1)
#   state.x77 데이터셋에서 문맹률(Illiteracy)을 이용해 범죄율(Murder)을 예측
#   하는 단순선형 회귀모델을 만드시오. 그리고 문맹률이 0.5, 1.0, 1.5일 때 범
#   죄율을 예측하여 보시오.
str( state.x77 )
head( state.x77 )

state.x77_df <- data.frame( state.x77 )
str( state.x77_df )
head( state.x77_df )

state.x77_model <- lm( Murder~Illiteracy , data = state.x77_df )
plot( Murder~Illiteracy , data = state.x77_df )
abline( state.x77_model )

coef( state.x77_model )
summary( state.x77_model )
#
#Call:
#  lm(formula = Murder ~ Illiteracy, data = state.x77_df)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.5315 -2.0602 -0.2503  1.6916  6.9745 

#Coefficients:
#  모델계수
#  Estimate Std. Error t value Pr(>|t|)             변수의 중요도, *가 많을수록 통계적으로 중요
# (Intercept)   2.3968     0.8184   2.928   0.0052  ** 
#  Illiteracy    4.2575     0.6217   6.848 1.26e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.653 on 48 degrees of freedom
#Multiple R-squared:  0.4942,	Adjusted R-squared:  0.4836  -> 모델 설명력, 0~1사이의 값, 이 값이 클수록 구한 회귀모델이
#                                                             현실을 잘 설명할 수 있다는 뜻
#     회귀모델이 현실을 잘 설명할 수 있다는 것은 회귀모델에 대한 예측값과 실제 측정값 사이 오차가 적다는 뜻
#     모델이 현실을 48% 정도 설명할 수 있다는 것을 의미
#
#F-statistic: 46.89 on 1 and 48 DF,  p-value: 1.258e-08 -> 모델이 의미있는(신뢰할수 있는) 모델인지
#                                                          작을수록 의미있는 모델
#                                                          p-value < 0.05이면 모델의 신뢰수준이 95%이상
#
# 금번 회귀모델의 설명력은 현실에 대해 48%의 설명력을 가지며, p값은 유의수준보다 작으므로 귀무가설은 기각된다.
#
# lm()의 귀무가설은 '독립변수 X와 종속변수 y가 아무 관련이 없다'이다. 대립가설은 두 변수는 관련이 있다.
#

# 회귀식
Murder = 4.257457 * state.x77_df$Illiteracy + 2.396776
Murder
fitted( state.x77_model )
residuals( state.x77_model )

# 범죄율 예측
Illiteracy_df <- data.frame( Illiteracy = c( 0.5, 1.0, 1.5 ) )
Murder_pred <- predict( state.x77_model, Illiteracy_df )
Murder_pred

#문맹률 0.5 : 4.525504
#문맹률 1.0 : 6.654232
#문맹률 1.5 : 8.782961

#문2)
#   trees 데이터셋에서 나무둘레(Girth)로 나무의 볼륨(Volume)을 예측하는 단
#   선형 회귀모델을 만드시오. 그리고 나무 둘레가 8.5, 9.0, 9.5일 때, 나무의
#   볼륨(Volume)을 예측하여 보시오.
str( trees )
head( trees )

trees_model <- lm( Volume~Girth , data = trees )
plot( Volume~Girth , data = trees )
abline( trees_model )

coef( trees_model )
summary( trees_model )

# Call:
#lm(formula = Volume ~ Girth, data = trees)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-8.065 -3.107  0.152  3.495  9.587 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -36.9435     3.3651  -10.98 7.62e-12 ***
#  Girth         5.0659     0.2474   20.48  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.252 on 29 degrees of freedom
#Multiple R-squared:  0.9353,	Adjusted R-squared:  0.9331 
#F-statistic: 419.4 on 1 and 29 DF,  p-value: < 2.2e-16
#
# 금번 회귀모델의 설명력은 현실에 대해 93%의 설명력을 가지며, p값은 유의수준보다 작으므로 귀무가설은 기각된다.
#

# 회귀식
Volume = 5.065856 * trees$Girth - 36.943459
Volume
fitted( trees_model )
residuals( trees_model )

# 볼륨 예측
Grith_df <- data.frame( Girth = c( 8.5, 9.0, 9.5 ) )
Volume_pred <- predict( trees_model, Grith_df )
Volume_pred

#나무 둘레 8.5 : 6.116320
#나무 둘레 9.0 : 8.649249
#나무 둘레 9.5 : 11.182177

#문3) 
#   pressure 데이터셋에서 온도(temperature)로 기압(pressure)을 예측하는 단
#   순선형 회귀모델을 만드시오. 그리고 온도가 65, 95, 155일 때 기압을 예측
#   하여 보시오.
data( pressure )

str( pressure )
head( pressure )

pressure_model <- lm( pressure~temperature , data = pressure )
plot( pressure~temperature , data = pressure )
abline( pressure_model )

coef( pressure_model )
summary( pressure_model )

#Call:
#  lm(formula = pressure ~ temperature, data = pressure)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-158.08 -117.06  -32.84   72.30  409.43 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -147.8989    66.5529  -2.222 0.040124 *  
#  temperature    1.5124     0.3158   4.788 0.000171 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 150.8 on 17 degrees of freedom
#Multiple R-squared:  0.5742,	Adjusted R-squared:  0.5492 
#F-statistic: 22.93 on 1 and 17 DF,  p-value: 0.000171
#
# 금번 회귀모델의 설명력은 현실에 대해 54%의 설명력을 가지며, p값은 유의수준보다 작으므로 귀무가설은 기각된다.
#

# 회귀식
pressure = 1.51242 * pressure$temperature -147.89887
pressure
fitted( pressure_model )
residuals( pressure_model )

# 기압 예측
temperature_df <- data.frame( temperature = c( 65, 95, 155 ) )
pressure_pred <- predict( pressure_model, temperature_df )
pressure_pred


#온도 65 : -49.591581
#온도 95 : -4.218984
#온도 155 : 86.526208
