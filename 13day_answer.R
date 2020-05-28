#
# 13day_answer.R
#
# 13일차 다중선형 회귀분석 / Logistic Regression 실습
#
#문1)
#trees 데이터셋에 대해 다음의 문제를 해결하는 R 코드를 작성하시오.
#
#(1) 나무 둘레(Girth)와 나무의 키(Height)로 나무의 볼륨을 예측하는 다중선형 회귀
#모델을 만드시오.
str( trees )
head( trees )

trees_height_model <- lm( Volume~. , data = trees )

coef( trees_height_model )
summary( trees_height_model )

# 회귀식
Height = 83.2957705 - 1.8615109 * trees$Girth + 0.5755946 * trees$Volume
Height
fitted( trees_height_model )
residuals( trees_height_model )

#(2) 다중선형 회귀모델을 이용하여 trees 데이터셋의 나무 둘레(Girth)와 나무의 키
#(Height)로 나무의 볼륨을 예측하시오.
trees_volume_model <- lm( Volume~Girth + Height, data = trees )

coef( trees_volume_model )
summary( trees_volume_model )

# 회귀식
Volume = -57.9876589 + 4.7081650 * trees$Girth + 0.3392512 * trees$Height
Volume
fitted( trees_volume_model )
residuals( trees_volume_model )

#(3) (2)에서 예측한 볼륨과 실제 trees 데이터셋의 볼륨(Volume)이 얼마나 차이가
#나는지 보이시오. (예측값, 실제값, 예측값-실제값을 나타낸다.)

# 예측값
pred <- predict( trees_volume_model, trees[ , c( 'Girth', 'Height' ) ] )
pred

# 예측값, 실제값, 예측값-실제값
result <- data.frame( 예측값 = pred, 실제값 = trees$Volume, 예측값_실제값 = ( pred - trees$Volume ) )
result


#문2)
#mtcars 데이터셋에서 다른 변수들을 이용하여 연비(mpg)를 예측하는 다중 회귀모델을 만드시오.
#
#(1) 전체 변수를 이용하여 연비(mpg)를 예측하는 회귀모델을 만들고 회귀식을 나타
#내시오.
str( mtcars )
head( mtcars )

mtcars_mpg_model <- lm( mpg~., data = mtcars )

coef( mtcars_mpg_model )
summary( mtcars_mpg_model )

# 회귀식
mpg = 12.30337416 - 
  0.11144048 * mtcars$cyl + 
  0.01333524 * mtcars$disp -
  0.02148212 * mtcars$hp +
  0.78711097 * mtcars$drat - 
  3.71530393 * mtcars$wt + 
  0.82104075 * mtcars$qsec + 
  0.31776281 * mtcars$vs + 
  2.52022689 * mtcars$am + 
  0.65541302 * mtcars$gear - 
  0.19941925 * mtcars$carb
mpg
fitted( mtcars_mpg_model )
residuals( mtcars_mpg_model )

#(2) 연비(mpg)를 예측하는 데 도움이 되는 변수들만 사용하여 예측하는 회귀모델을
#만들고 회귀식을 나타내시오.
library( MASS )

mtcars_mpg_model2 <- stepAIC( mtcars_mpg_model )

coef( mtcars_mpg_model2 )
summary( mtcars_mpg_model2 )

# 회귀식
mpg = 9.617781 -
  3.916504 * mtcars$wt +
  1.225886 * mtcars$qsec +
  2.935837 * mtcars$am
mpg
fitted( mtcars_mpg_model2 )
residuals( mtcars_mpg_model2 )

#(3) (1), (2)에서 만든 예측모델의 설명력(Adjusted R-squared)을 비교하시오.
summary( mtcars_mpg_model )
summary( mtcars_mpg_model2 )

# mtcars_mpg_model의 설명력은 0.8066, mtcars_mpg_model2의 설명력은 0.8336로서
# 변수를 선별한 후 설명력이 더 높아졌다.

#문3) 
#UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.

mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )

#(1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
str( mydata )
head( mydata )

mydata_model <- glm( admit~gre + gpa + rank, data = mydata )

coef( mydata_model )
summary( mydata_model )

#(2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여 
#합격 여부를 예측하고 실제값과 예측값을 나타내시오.

# 예측값
pred <- predict( mydata_model, mydata[ , c( 'gre', 'gpa', 'rank' ) ] )
pred

# one-hot encoding
pred <- round( pred, 0 )
pred

# 예측값, 정답
result <- data.frame( predict = pred, answer = mydata$admit )
result

#(3) 만들어진 모델의 예측 정확도를 나타내시오.
acc <- mean( result$predict == result$answer )
acc

#예측 정확도 : 0.705
