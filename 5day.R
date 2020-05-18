# 
# 5일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 txt/excel 파일 읽기
#   2.2 자료의 종류
#   2.3 단일 변수 범주형 자료 탐색
#   2.4 단일 변수 연속형 자료 탐색
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#

#
# 2.1 txt/excel 파일 읽기
#
# Text File 읽기
setwd( "C:\\WorkspaceBigDaejeon\\WorkspaceR" )
df.txt <- read.table( file = "airquality.txt", 
                  header = T, 
                  encoding = "UTF-8" )
df.txt

class( df.txt )
str( df.txt )
head( df.txt )
tail( df.txt )

#
# Excel 파일 읽기
#
# Excel 파일 읽기용 패키지 설치
install.packages( "xlsx" )      # Excel 파일 읽기 패키지
install.packages( "rJava" )     # Java 실행 패키지

# 기본 패키지 외에 설치된 패키지 사용( library Load )
library( rJava )
library( xlsx )

setwd( "C:\\WorkspaceBigDaejeon\\WorkspaceR" ) # 파일 저장 경로 설정
df.xlsx <- read.xlsx( file = "airquality.xlsx",
                      sheetIndex = 1,
                      encoding = "UTF-8" )
df.xlsx
class( df.xlsx )
str( df.xlsx )
head( df.xlsx )
tail( df.xlsx )

score <- c( 76, 84, 69, 50, 95, 6, 85, 71, 88, 84 )
which( score == 69 )  # 조건에 만족하는 위치의 index
which( score >= 85 )
max( score )
which.max( score )    # 최고값의 index
min( score )
which.min( score )    # 최저값의 index

idx <- which( score >= 60 )
score[ idx ] <- 61
score

idx <- which( df.xlsx[ , 1:2 ] == "NA", arr.ind = TRUE )
                    # arr.ind = TRUE : 해당 조건의 행/열값을 확인할때
idx




#
# 2.2 자료의 종류
#
# - 자료의 종류에 따라 적용할 수 있는 분석 방법이 다르다.
# - 분석을 해야할 자료를 가지고 있을 때 1차적으로 해야 할 일은
#   해당 자료가 어떤 분류에 속하는지를 파악하는 일이다.
#
# - 자료 특성에 따른 분류
#   분석 대상 자료의 특성에 따라 범주형 자료와 연속형 자료로 분류한다.
#
# 1. 범주형 자료( categorical data ) : 
#     - 범주 또는 그룹으로 구분할 수 있는 값
#     - 범주형 자료의 값은 기본적으로 숫자로 표현할 수 없고,
#       대소비교나 산술연산이 적용되지 않는다
#     - 범주형 자료는 대체로 문자형 값을 갖는데, 숫자로 표기할 수 있으나
#       계산이 가능한 연속형 자료가 되는 것은 아니다.
#
# 2. 연속형 자료( numerical data ) :
#     - 크기가 있는 숫자들로 구성된 자료
#     - 연속형 자료의 값들은 대소비교, 산술연산이 가능하기 때문에 다양한
#       분석 방법이 존재한다.
# 


#
# 자료( data ) : 어떤 주제를 가지고 값을 모아 놓은 것 전체
# 관측값( observation ) : 자료에 포함된 값들
#
# 통계학에서의 변수는 "연구, 조사, 관찰하고 싶은 대상의 특성(feature)"
#
# 변수 개수에 따라
#   1. 단일변수 자료( univariate data ), 일변량 자료
#       하나의 변수로 구성된 자료
#   2. 다중변수 자료( multivariate data ), 다변량 자료
#       두 개이상의 변수로 구성된 자료
#       특별히 두 개의 변수로 구성된 자료를 이변량 자료( bivariate data )
#
#   단일 변수 자료는 vector에 저장하여 분석
#   다중 변수 자료는 matrix 또는 data frame에 저장하여 분석
#
# 변수의 개수와 자료의 특성에 따른 분류
# 1. 단일변수 범주형 자료
# 2. 단일변수 연속형 자료
# 3. 다중변수 범주형 자료
# 4. 다중변수 연속형 자료
#



#
# 2.3 단일 변수 범주형 자료 탐색
#
# 단일 변수 범주형 자료 : 특성이 하나이면서 자료의 특성이 범주형인 자료
#   범주형 자료에 할 수 있는 기본 작업 : 자료에 포함된 관측값들의
#                          도수분포표 <- 종류별로 개수를 세는 것,
#                                        종류별로 비율을 구하는 것,
#                               시각화는 막대/원 그래프를 사용
#
#
favorite <- c( 'WINTER', 'SUMMER', 'SPRING',
               'SUMMER', 'SUMMER', 'FALL',
               'FALL', 'SUMMER', 'SPRING', 'SPRING' )
favorite

class( favorite )   # type
str( favorite )     # 내부 구조
dim( favorite )     # data 수 

# 도수 분포표 작성
table( favorite )

# 도수 분포 비율
table( favorite ) / length( favorite )

# 시각화( 막대 그래프 )
ds <- table( favorite )
ds

class( ds )
str( ds )
dim( ds )

barplot( ds, main = 'favorite season' )

ds.new <- ds[ c( 2, 3, 1, 4 ) ]
ds.new
barplot( ds.new, main = 'favorite season' )

pie( ds, main = 'favorite season' )
pie( ds.new, main = 'favorite season' )

favorite.color <- c( 2, 3, 2, 1, 1, 2, 2,
                     1, 3, 2, 1, 3, 2, 1, 2 )
ds <- table( favorite.color )
ds

barplot( ds, main = 'favorite season' )

colors <- c( 'green', 'red', 'blue' )
names( ds ) <- colors
ds
barplot( ds, main = 'favorite season',
         col = colors )
pie( ds, main = 'favorite season',
         col = colors )




#
# 2.4 단일 변수 연속형 자료 탐색
#
# 단일 변수 연속형 자료 : 특성이 하나이면서 자료의 특성이 연속형인 자료
#       연속형 자료는 관측값들이 크기를 가지므로 다양한 분석방법 존재
#
# 1. 평균( mean ) : 하나의 값으로 전체를 대표할 수 있는 값, 
#           이상값에 영향을 받는다.
# 2. 중앙값( median ) : 자료의 값들을 크기순으로 정렬하였을 때 
#                      가장 중앙에 위치한 값, 
#                      이상값에 영향을 받지 않는다.
# 3. 절사평균( trimmed mean ) : 자료의 관측값들 중에서 작은 값들의 하위
#                      n%와 큰 값의 상위 n%를 제외하고 중간에 있는
#                      나머지값들만 가지고 평균을 계산하는 방식
# 4. 사분위수( quatrile ) : 주어진 자료에 있는 값들을 크기순으로 나열
#                           했을 때 4등분하는 지점에 있는 값
#           1사분위수(Q1), 2사분위수(Q2, 중앙값과 동일), 3사분위수(Q3)
#           전체 자료를 4개로 나누므로 4개 구간은 25%의 자료가 존재
# 5. 산포( distribution ) : 주어진 자료에 있는 값들이 퍼져 있는 정도
#   분산( variance ) : 주어진 자료의 각각의 값이 평균으로부터 떨어져
#                      있는 정도를 계산하여 합산한 후 값들의 개수로 
#                      나누어 계산
#   표준편차( standard deviation ) : 분산의 제곱근으로 계산
#
#   시각화는 히스토그램( Histogram )과 상자 그래프( box plot )를 사용
#
weight <- c( 60, 62, 64, 65, 68, 69 )
weight

weight.heavy <- c( weight, 120 )
weight.heavy

# 평균
mean( weight )
mean( weight.heavy )

# 중앙값
median( weight )
median( weight.heavy )

# 절사 평균
mean( weight, trim = 0.2 )
mean( weight.heavy, trim = 0.2 )

# 사분위수
quantile( weight.heavy )
quantile( weight.heavy, ( 0:10 ) / 10 )
    # ( 0:10 ) / 10 : 구간을 몇개로 나눌것인지를 지정
    # 0.1 ~ 1.0사이 백분율로 10% ~ 100%
summary( weight.heavy )

# 산포
# 분산
var( weight )

# 표준편차
sd( weight )

# 값의 범위( 최소값과 최대값 )
range( weight )

# 최대값과 최소값의 차이
diff( range( weight ) )

# 연속형 자료 시각화
# Histogram : 연속형 자료의 분포를 시각화하는 도구
#             연속형 자료에서의 구간을 나누고 구간에 속한
#             값들의 개수를 세는 방법으로 사용
class( cars )
str( cars )

dist <- cars[ , 2 ]
dist
boxplot.stats( dist ) # 구체적인 값을 파악 할 때 사용
                      # $stats : 사분위수
                      # $n : 관측값의 수
                      # $conf : 중앙값에 대한 신뢰구간
                      # $out : 특이값( 이상치 ) 목록

hist( dist, main = "Histogram for 제동거리",
      xlab = "제동거리", ylab = "빈도수",
      border = "blue", col = "green",
      las = 2, breaks = 5 )

# 상자 그래프( 상자그림,  boxplot, 상자 수염 그래프 )
#   사분위수를 그래프형태로 시각화하는 도구
#   상자그래프는 하나의 그래프로 데이터의 분포
#   형태를 포함한 다양한 정보를 전달
#   - 자료의 전반적인 분포를 이해하는데 도움
#   - 구체적인 최소/최대/중앙값을 알기는 어렵다.
boxplot( dist, main = '자동차 제동거리' )
 
boxplot.stats( dist )
boxplot.stats( dist )$stats # 정상범위 사분위수
boxplot.stats( dist )$n     # 관측치 수
boxplot.stats( dist )$conf  # 중앙값 신뢰구간
boxplot.stats( dist )$out   # 특이값( 이상치 ) 목록

# 일변량 연속형 데이터중 그룹으로 구성된
# 자료의 상자그래프
boxplot( Petal.Length~Species, data = iris,
         main = '품종별 꽃잎 길이' )

str( iris )

boxplot( iris$Petal.Length~iris$Species,
         main = '품종별 꽃잎 길이' )


# 한 화면에 여러 그래프 작성
par( mfrow = c( 1, 3 ) )    # 1 X 3 가상화면 분할
barplot( table( mtcars$carb ), main = "C",
         xlab = 'carburetors', ylab = 'freq',
         col = 'blue' )
barplot( table( mtcars$cyl ), main = 'Cyl',
         xlab = 'cyl', ylab = 'freq',
         col = 'red' )
barplot( table( mtcars$gear ), main = 'g',
         xlab = 'gear', ylab = 'freq',
         col = 'green' )
par( mfow = c( 1, 1 ) ) # 가상화면 분할 해제










