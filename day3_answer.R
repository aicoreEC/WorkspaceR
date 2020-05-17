#
# day3_answer.R
#
# 3일차 Matrix/Data Frame 실습
#
#문1) 
#>score
#m  f
#[1,] 10 21
#[2,] 40 60
#[3,] 60 70
#[4,] 20 30

#1. 위와 같은 내용의 matrix score를 생성
m <- c(10,40,60,20)
f <- c(21,60,70,30)
score <- matrix( c( m, f ), nrow = 4 )
score

score <- data.frame(m,f)
score

#2. score의 열 이름을 각각 male, female로 바꾸시오
colnames(score) <- c('male','female')
score

#3. 2행에 있는 모든 값을 출력
score[ 2, ]

#4. female의 모든 값을 출력
score[ , 'female']

#5. 3행 2열의 값을 출력
score[ 3, 2 ]

#문2)
#R에서 제공하는 state.x77 데이터셋을 이용하여 작성

#1. state.x77을 변환하여 st에 data frame으로 작성
st <- data.frame(state.x77)

#2. st의 내용을 출력
st

#3. st의 열 이름 출력
colnames( st )
names( st )

#4. st의 행 이름 출력
rownames( st )

#5. st의 행의 개수와 열의 개수 출력
nrow( st )
ncol( st )
dim( st )

#6. st의 요약 정보 출력
str( st )

#7. st의 행별 합계와 평균 출력
rowSums( st )
apply( st, 1, sum )
rowMeans( st )
apply( st, 1, mean )

#8. st의 열별 합계와 평균 출력
colSums( st )
apply( st, 2, sum )
colMeans( st )
apply( st, 2, mean )

#9. Florida 주의 모든 정보 출력
st[ 'Florida', ]

#10. 50개 주의 수입(Income) 정보만 출력
st[ , 'Income' ]

#11. Texas 주의 면적(Area)을 출력
st[ 'Texas', 'Area' ]

#12. Ohio 주의 인구(Population)와 수입(Income) 출력
st[ 'Ohio', c( 'Population', 'Income' ) ]

#13. 인구가 5,000 이상인 주의 데이터만 출력
subset( st, Population >= 5000 )

#14. 수입이 4,500 이상인 주의 인구, 수입, 면적을 출력
subset( st, Income >= 4500, select = c( 'Income','Area' ) ) # 방법1
subset( st, Income >= 4500 )[ , c( 'Income', 'Area' ) ] # 방법2

#15. 수입이 4,500 이상인 주는 몇 개인지 출력
nrow( subset( st, Income >= 4500 ) )

#16. 전체 면적(Area)이 100,000 이상이고, 결빙일수(Frost)가 120 이상인 주의 정보 출력
subset( st, Area>=100000 & Frost > 120 )

#17. 인구(Population)가 2,000 미만이고, 범죄율(Murder)이 12미만인 주의 정보 출력
subset( st, Population < 2000 & Murder < 12 )

#18. 문맹률(Illiteracy)이 2.0 이상인 주의 평균 수입은 얼마인지 출력
#방법 1
tmp <- subset( st, Illiteracy >= 2.0 )
tmp <- tmp$Income
mean( tmp )

# 방법 2
mean( subset( st, Illiteracy >= 2.0 )$ Income )

#19. 문맹률(Illiteracy)이 2.0 미만인 주와 2.0 이상인 주의 평균 수입의 차이 출력
# 방법 1
tmp1 <- subset( st, Illiteracy < 2.0 )
tmp1 <- tmp1$Income
tmp2 <- subset( st, Illiteracy >= 2.0 )
tmp2 <- tmp2$Income
abs( mean( tmp1 ) - mean( tmp2 ) )

# 방법 2
abs( mean( subset( st, Illiteracy < 2.0 )$Income ) - mean( subset( st, Illiteracy >= 2.0 )$Income ) )

#20. 기대수명(Life Exp)이 가장 높은 주는 어디인지 출력
max.life <- max( st$Life.Exp )
rownames( subset( st, Life.Exp == max.life ) )

#21 Pennsylvania 주보다 수입(Income)이 높은 주들 출력
pen.income <- st[ 'Pennsylvania','Income' ]
rownames( subset( st, Income > pen.income ) )

#문3)
#R에서 제공하는 mtcars 데이터셋은 자동차 모델에 대한 제원 정보를 담고 있다.

#1. 이 데이터셋의 자료구조 출력
class( mtcars )

#2. 이 데이터셋의 행의 개수와 열의 개수 출력
dim( mtcars )

#3. 이 데이터셋 열들의 자료형 출력
str( mtcars )

#4. 연비(mpg)가 가장 좋은 자동차 모델 출력
max.mpg <- max( mtcars$mpg )
rownames( mtcars[ mtcars$mpg == max.mpg, ] ) # 방법 1
rownames( subset( mtcars, mpg == max.mpg ) ) # 방법 2

#5. gear가 4인 자동차 모델 중 연비가 가장 낮은 모델 출력
min.mpg <- min( mtcars[ mtcars$gear == 4, 'mpg' ] )
rownames( mtcars[ mtcars$mpg == min.mpg, ] ) # 방법 1
rownames( subset( mtcars, mpg == min.mpg ) ) # 방법 2

#6. Honda Civic의 연비(mpg)와 gear 수 출력
mtcars[ 'Honda Civic','mpg' ]

#7. Pontiac Firebird 보다 연비가 좋은 자동차 모델 출력
pon.mpg <- mtcars[ 'Pontiac Firebird','mpg' ]
rownames( mtcars[ mtcars$mpg > pon.mpg, ] ) # 방법 1
rownames( subset( mtcars, mpg > pon.mpg ) ) # 방법 2

#8. 자동차 모델들의 평균 연비 출력
mean( mtcars$mpg )

#9. gear의 수 종류 출력
unique(mtcars$gear)
