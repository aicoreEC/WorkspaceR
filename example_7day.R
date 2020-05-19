# 
# 7일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 데이터 전처리
#   2.2 결측치 처리
#   2.3 특이값 처리
#   2.4 데이터 가공
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 데이터 전처리 이해
#
# 데이터 전처리( data preprocessing ) : 초기에 확보한 데이터를 정제하고
#                                       가공해서 분석에 적합한 데이터를 확보
#                                       하는 과정
#
# 데이터 전처리는 전체 분석 과정 중에서 매우 오랜 시간을 차지하기 때문에
# 이를 효과적으로 처리하는 방법을 아는 것은 매우 중요
#
# 데이터 전처리 내용
# 1. 결측치 처리
# 2. 특이값 처리

#
# 2.2 결측값 처리
#
#
# 결측값( missing value ) : 데이터를 수집하고 저장하는 과정에서 저장할 값을
#                           얻지 못하는 경우 발생
#
# 결측값을 처리하는 방법
# 1. 결측값을 제거하거나 제외한 다음 데이트를 분석한다.
# 2. 결측값을 추정하여 적당한 값으로 치환한 후 데이터를 분석한다.
#

#
# 벡터의 결측값 처리
#
# R에서는 결측값을 다루기 위해 NA라고 하는 특별한 데이터값을 제공
# NA는 숫자형, 문자형, 논리형 데이터 어디에서나 결측값을 나타내는 용도로 사용
#
# 결측값의 특성과 존재 여부 확인
#
z <- c( 1, 2, 3, NA, 5, NA, 8 )
sum( z )               # 정상 계산이 안 됨
is.na( z )             # NA 여부 확인
sum( is.na( z ) )      # NA의 개수 확인
sum( z, na.rm = TRUE ) # NA를 제외하고 합계 계산

#
# 결측값 대체 및 제거
#
z1 <- c( 1, 2, 3, NA, NA, 8 )
z2 <- c( 5, 8, 1, NA, 3, NA, 7 )
z1[ is.na( z1 ) ] <- 0            # NA를 0으로 치환
z1
z3 <- as.vector( na.omit( z2 ) )  # NA를 제거하고 새로운 벡터 생성
z3

#
# 매트릭스와 데이터프레임 결측값 처리
#

# NA를 포함하는 test 데이터 생성
x <- iris
x[ 1, 2 ] <- NA
x[ 1, 3 ] <- NA
x[ 2, 3 ] <- NA
x[ 3, 4 ] <- NA
head( x )

#
# 데이터프레임 열별 결측값 확인
#

# for문 이용
for ( i in 1:ncol( x ) ) {
  this.na <- is.na( x[ i ] )   # 각 변수별로 NA 개수 파악
  cat( colnames( x )[ i ], "\t", sum( this.na ), "\n" )
}

# apply 이용
col_na <- function( y ) { # 각 변수별로 NA 개수 파악 함수
  return( sum( is.na( y ) ) )
}

na_count <- apply( x, 2, FUN = col_na )
na_count

#
# 데이터프레임의 행별 결측값 확인
#
rowSums( is.na( x ) )             # 행별 NA의 개수
sum( rowSums( is.na( x ) ) > 0 )  # NA가 포함된 행의 개수

sum( is.na( x ) )                 # 데이터셋 전체에서의 NA 개수

#
# 결측값을 제외하고 새로운 데이터셋 만들기
#
# complete_case() : 어떤 데이터셋에서 NA를 포함하지 않은 완전한( complete ) 행들을 찾아준다.
head( x )
x[ !complete.cases( x ), ]        # NA가 포함된 행들 출력 
y <- x[ complete.cases( x ), ]    # NA가 포함된 행들 제거
head( y )

# 
# NA 값이 많은 데이터 처리 : 어떤 데이터셋은 NA값을 포함한 행이 많아 이를 모두 제거하는 남는 것이 별로 없어서
#                            데이터를 분석하기 어려운 경우가 잇다. 이런 경우에 만약 NA 값이 특정 열에 몰려
#                            있다면 그 열은 제외하고 데이터를 분석한다.
# NA 값이 여러 열에 흩어져 있는 경우는 NA 값을 적당한 값으로 추정하여 대체한 후 분석할 수 있다.
#

#  
# 결측값 추정을 위한 패키지 : misc 패키지가 결측값 추적을 위해서 사용하는 대표적인 패키지
#
# 결측값 추정을 하여 분석을 하면 분석 결과의 신뢰도가 떨어질 수 밨에 없으나 아무런 분석도 하니 못하는 것보다는 낫다.
#

#
# 2.3 특이값 처리
#
# 특이값( outlier ), 이상치 : 정상적이라고 생각되는 데이터의 분포 범위 밖에 위치하는 값들 
#                             입력 오류나 실제 특이값일수도 있다.
#                             특이값의 성질은 제조공정의 불량품 선별, 은행 거래 시스템의 사기 거래 탐지할 때 사용하기도 함
#
# 데이터 분석시 특이값을 포함한 채 평균 등을 계산하면 전체 데이터 양상 파악에 왜곡을 가져올 수 있으므로 분석시 제외 경우 많음
#
# 데이터셋에 특이값이 포함되어 있는지 여부 기준
# 1. 논리적으로 있을 수 없는 값이 있는지 찾는다. 특별한 방법은 없기 때문에 분석자가 각 변수의 특성을 이해한 후 특이값 탐색
# 2. 상식을 벗어난 값이 있는지 찾는다.
# 3. 상자그래프를 통해 찾아본다.
#

# 특이값 추출 및 제거
#
# 상자그래프를 통한 특이값 확인
st <- data.frame( state.x77 )
boxplot( st$Income )
boxplot.stats( st$Income )$out

# 특이값 포함행 제거
#
# 일반적으로 특이값 포함 행 제거는 이상치를 NA로 바꾸고 NA를 포함한 행을 제거하는 방식으로 진행
out.val <- boxplot.stats( st$Income )$out   # 특이값 추출
st$Income[ st$Income %in% out.val ] <- NA   
                                    # 특이값을 NA로 대체, %in% : 어떤 벡터에 비교하고자 하는 값이 포함되어 있는지 알고 싶을 때 사용
head( st )
newdata <- st[ complete.cases( st ),  ]     # NA 포함된 행 제거
head( newdata )

#
# 2.4 데이터 가공
#
# 1. 데이터 정렬
#
# 정렬( sort ) : 데이터를 주어진 기준에 따라 크기순으로 재배열하는 과정, 데이터분석시 빈번하게 수행하는 과정
#
v1 <- c( 1, 7, 6, 8, 4, 2, 3 )
order( v1 )                       # 주어진 열의 값들에 대해 순서를 붙이는 함수, 값의 크기를 기준으로 작은 값부터 시작해서 번호부여
v1 <- sort( v1 )                  # 내림차순
v1
v2 <- sort( v1, decreasing = T )  # 오름차순
v2

# 매트릭스와 데이터프레임 정렬 : 특정 열의 값들을 기준으로 행들을 재배열하는 형태로 정렬을 
head( iris )
order( iris$Sepal.Length )
iris[ order( iris$Sepal.Length ), ]                 # 오름차순, 결과에서 행 순서 번호는 정렬전 순서 번호
iris[ order( iris$Sepal.Length, decreasing = T ), ] # 내림차순
iris.new <- iris[ order( iris$Sepal.Length ), ]     
head( iris.new )
iris[ order( iris$Species, decreasing = T, iris$Petal.Length ), ] # 정렬 기준이 2개

#
# 2 데이터 분리와 선택
#
# split() : 하나의 데이터셋을 열의 값을 기준으로 여러 개의 데이터셋으로 분리할 때
# subset() : 데이터셋으로부터 조건에 맞는 행들을 추출할 때
#

# 데이터 분리
sp <- split( iris, iris$Species )  # 품종별 데이터 분리
sp                                 # 벡터가 아니고 리스트
summary( sp )                      # 분리 결과 요약내용, 결과에서 Length는 분리된 데이터에서 열의 개수
sp$setosa

# 데이터 선택
subset( iris, Species == "setosa" )
subset( iris, Sepal.Length > 7.5 )
subset( iris, Sepal.Length > 5.1 & Sepal.Width > 3.9 )
subset( iris, Sepal.Length > 7.6, select = c( Petal.Length, Petal.Width ) )

#
# 3. 데이터 샘플링과 조합
#
# 데이터 샘플링( Sampling ) : 통계 용어로 주어진 값들이 있을 때 그중에서 임의의 개수의 값들을 추출하는 작업
#   비복원추출 : 한 번 추출한 값은 다시 추출하지 않도록 하는 추출 방식
#   복원추출 : 추출한 값을 확인한 후 다시 데이터에 합친 후 새로 추출하는 방식
#   * 데이터 분석에서는 비복원 추출을 많이 사용
#
# 샘플링이 필요한 경우는 데이터셋의 크기가 너무 커서 데이터 분석에 시간이 많이 걸릴 때 일부의 데이터만 
# 샘플링하여 대략의 결과를 미리 확인하고자 할 때 사용
#

# 숫자를 임의로 추출
x <- 1:100
y <- sample( x, size = 10, replace = FALSE )  # size : 추출할 값, replace = FALSE : 비복원추출
y

# 행을 의의로 추출
idx <- sample( 1:nrow( iris ), size = 50, replace = FALSE )
iris.50 <- iris[ idx, ]
dim( iris.50 )
head( iris.50 )

sample( 1:20, size = 5 )
sample( 1:20, size = 5 )
sample( 1:20, size = 5 )

# sample()는 임의의 샘플을 추출하는 방식이기 때문에 함수를 실행할 때마다 매번 결과가 다르다.
# 경우에 따라 임의 추출을 하되 다음번에 다시 추출해도 동일한 결과가 나오도록 해야 할 경우가 있다.
# 이런 경우 set.seed() 함수를 sample() 함수 실행 전에 먼저 실행하면 된다.
# set.seed()의 매개변수 값이 같으면 sample() 함수의 결과도 같다.
set.seed( 100 )
sample( 1:20, size = 5 )
set.seed( 100 )
sample( 1:20, size = 5 )
set.seed( 100 )
sample( 1:20, size = 5 )

#
# 데이터 조합( combination ) : 주어진 데이터값들 중에서 몇 개씩 짝을 지어 추출하는 작업
#
# combn() : 데이터 조합시 사용 함수, 결과에서 각 열이 하나의 조합을 의미
combn( 1:5, 3 )         # 1~5에서 3개를 뽑는 조합

x = c( "red", "green", "blue", "black", "white" )
com <- combn( x, 2 )
com

for ( i in 1:ncol( com ) ) {
  cat( com[ , i ], "\n" )
}

#
# 데이터 집계와 병합
#
# 데이터 집계( aggregation ) : 매트릭스와 데이터프레임과 같은 2차원 데이터는 데이터 그룹에 대해서 
#                              합계나 평균을 계산해야 하는 일이 많은데 이와 같은 작업을 말한다.
# aggregate() : 데이터 집계용 함수

# iris의 각 변수의 품종별 평균 출력
agg <- aggregate( iris[ , -5 ], by = list( iris$Species ), FUN = mean )
#                 dataset       집계 기준                  집계 작업 내용
agg   # 결과에서 첫 번째 열( Group.1 )이 집계 기준

agg <- aggregate( iris[ , -5 ], by = list( 품종 = iris$Species ), FUN = mean )
agg

# iris의 각 변수의 품종별 표준편차 출력
agg <- aggregate( iris[ , -5 ], by = list( 표준편차 = iris$Species ), FUN = sd )
agg

# mtcars의 각 변수의 최대값 출력
head( mtcars )
agg <- aggregate( mtcars, by = list( cyl = mtcars$cyl, vs = mtcars$vs ), FUN = max )
agg

#
# 데이터 병합( merge ) : 데이터 분석을 위해 자료를 모으다 보면 연관된 정보가 여러 파일에 흩어져 있는 경우가 있다.
#                        이를 합치는 작업을 말한다.
x <- data.frame( name = c( 'a', 'b', 'c' ), math = c( 90, 80, 40 ) )
y <- data.frame( name = c( 'a', 'b', 'd' ), korean = c( 75, 60, 90 ) )
x
y

z <- merge( x, y, by = c( 'name' ) )  # name을 기준으로 병합
#       병합할 dataset, 병합 기준
z                                     # name이 일치하는 행만 병합

z2 <- merge( x, y )                   # 병합 기준이 되는 열의 값이 같은 경우
z2

merge( x, y, all.x = T )              # 첫 번째 dataset의 행들은 모두 표시
merge( x, y, all.y = T )              # 두 번째 dataset의 행들은 모두 표시
merge( x, y, all = T )                # 두 dataset의 모든 행들이 표시

# 병합의 기준이 되는 열의 이름이 서로 다른 경우에 병합
x <- data.frame( name = c( 'a', 'b', 'c' ), math = c( 90, 80, 40 ) )
y <- data.frame( sname = c( 'a', 'b', 'c' ), korean = c( 75, 60, 90 ) )
x
y
merge( x, y, by.x = c( 'name' ), by.y = c( 'sname' ) ) # by.x는 첫 번째 dataset 병합기준, by.y는 두 번째 dataset 병합기준

