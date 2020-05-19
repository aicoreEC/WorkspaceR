#
# day6_answer.R
#
# 6일차 다변량 자료 탐색 / EDA 실습
#
#문1)
#다음은 직장인 10명의 수입과 교육받은 기간을 조사한 자료이다. 산점도와 상관계수를 구하고, 
#수입과 교육기간 사이에 어떤 상관관계가 있는지 설명하시오.
#
#수입 	 121 99 41 35 40 29 35 24 50 60
#교육기간 19  20 16 16 18 12 14 12 16 17
income <- c( 121, 99, 41, 35, 40, 29, 35, 24, 50, 60 )
period <- c( 19, 20, 16, 16, 18, 12, 14, 12, 16, 17 )

tbl <- data.frame( income, period )
tbl 

plot( income~period, data = tbl )
res <- lm( income~period, data = tbl )
abline( res )

cor( income, period )

# 상관계수가 0.7292108으로 비교적 강한 양의 상관관계를 가진다.
# 교육받은 기간이 길수록 수입이 증가

#문2)
#다음은 대학생 10명의 성적과 주당 TV 시청시간을 조사한 자료이다. 산점도와 상관계수를 구하고, 
#성적과 TV 시청시간 사이에 어떤 상관관계가 있는지 설명하시오.
#
#성적 	 77.5 60 50 95 55 85 72.5 80 92.5 87.5
#시청시간 14   10 20  7 25  9 15   13  4   21
score <- c( 77.5, 60, 50, 95, 55, 85, 72.5, 80, 92.5, 87.5 )
tv <- c( 14, 10, 20, 7, 25, 9, 15, 13, 4, 21 )

tbl <- data.frame( score, tv )
tbl 

plot( score~tv, data = tbl )
res <- lm( score~tv, data = tbl )
abline( res )

cor( score, tv )

# 상관계수가 -0.6283671로 비교적 강한 음의 상관관계를 가진다.
# TV 시청 시간이 증가할수록 성적은 감소한다.

#문3) 
#R에서 제공하는 mtcars 데이터셋에서 mpg와 다른 변수들 간의 상관계수를
#구하시오. 어느 변수가 mpg와 가장 상관성이 높은지 산점도와 함께 설명하시오.			
head( mtcars )

cor( mtcars )
cor( mtcars )[ 1, ]

# disp와의 상관계수가 -0.8475514로 가장 상관성이 높다.  

#문4)
#다음은 2015년부터 2026년도까지의 예상 인구수 추계자료이다. 연도를 x
#축으로 하여 선그래프를 작성하시오.
#
#연도		총인구 (천명)		연도		총인구 (천명)
#2015		51014				2021		52123
#2016		51245				2022		52261
#2017		51446				2023		52388
#2018		51635				2024		52504
#2019		51811				2025		52609
#2020		51973				2026		52704
year <- 2015:2026
pop <- c( 51014, 51245, 51446, 51635, 51811, 51973, 52123, 52261, 52388, 52504, 52609, 52704 )
plot( year, pop, type = 'l', main = '인구수 추계' )

#
#문5)
#도로교통공단 시도 시군구별 월별 교통사고.csv 파일에 대한 EDA를 수행하시오.
#

# 1. Data 읽기 및 data frame 생성
setwd( "C:\\WorkspaceBigDaejeon\\WorkspaceR" ) # 파일 저장 경로 설정
df <- read.csv( file = "도로교통공단_시도_시군구별_월별_교통사고(2018).csv",
                header = T )

# 2. data frame 구조 확인
class( df )
dim( df )

str( df )
df$시도 <- as.character( df$시도 )
df$시군구 <- as.character( df$시군구 )
df$월 <- as.character( df$월 )
str( df )

month <- c()
for ( i in 1:nrow( df ) ) {
  if ( df[ i, "월" ] == "01월" ) {
    month[ i ] <- 1
  } else if ( df[ i, "월" ] == "02월" ) {
    month[ i ] <- 2
  } else if ( df[ i, "월" ] == "03월" ) {
    month[ i ] <- 3
  } else if ( df[ i, "월" ] == "04월" ) {
    month[ i ] <- 4
  } else if ( df[ i, "월" ] == "05월" ) {
    month[ i ] <- 5
  } else if ( df[ i, "월" ] == "06월" ) {
    month[ i ] <- 6
  } else if ( df[ i, "월" ] == "07월" ) {
    month[ i ] <- 7
  } else if ( df[ i, "월" ] == "08월" ) {
    month[ i ] <- 8
  } else if ( df[ i, "월" ] == "09월" ) {
    month[ i ] <- 9
  } else if ( df[ i, "월" ] == "10월" ) {
    month[ i ] <- 10
  } else if ( df[ i, "월" ] == "11월" ) {
    month[ i ] <- 11
  } else if ( df[ i, "월" ] == "12월" ) {
    month[ i ] <- 12
  }  
}
month

df <- data.frame( df, month )
str( df )

head( df )
tail( df )

# 3. 시도 이름 분리
local <- unique( df[ , 1 ] )
local

# 4. 서울 데이터 분리
local.seoul <- df[ df$시도 == local[ 1 ], ]
dim( local.seoul )
str( local.seoul )
head( local.seoul )
tail( local.seoul )

# 4. 서울 구별 이름 분리
local.gu <- unique( local.seoul[ , 2 ] )
local.gu <- as.vector( local.gu )

local.gu.index <- 1:length( local.gu )

# 5. 월 분리
month <- as.character( unique( local.seoul[ , 3 ] ) )
month <- as.vector( month )

month.index <- 1:12

# 6. 서울 월별 사고 건수 
seoul.accident <- c()
for ( i in 1:length( month.index ) ) {
  seoul.accident[ i ] <- sum( subset( local.seoul, month == i )$발생건수 )
}
names( seoul.accident ) <- month
seoul.accident

max.month <- max( seoul.accident )
max.month.name <- names( seoul.accident[ seoul.accident == max.month ] )
min.month <- min( seoul.accident )
min.month.name <- names( seoul.accident[ seoul.accident == min.month ] )
cat( "서울 사고 발생이 최고인 월 : ", max.month.name, " [ ", max.month, " ]", "\n", 
     "서울 사고 발생이 최저인 월 : ", min.month.name, " [ ", min.month, " ]" )

barplot( seoul.accident, main = "서울시 월별 발생 건수" )

# 7. 서울 구별 사고 건수
seoul.gu.accident <- c()

for ( i in 1:length( local.gu ) ) {
  seoul.gu.accident[ i ] <- sum( subset( local.seoul, local.seoul$시군구 == local.gu[ i ] )$발생건수 )
}
names( seoul.gu.accident ) <- local.gu
seoul.gu.accident

max.gu <- max( seoul.gu.accident )
max.gu.name <- names( seoul.gu.accident[ seoul.gu.accident == max.gu ] )
min.gu <- min( seoul.gu.accident )
min.gu.name <- names( seoul.gu.accident[ seoul.gu.accident == min.gu ] )

cat( "서울 사고 발생이 최고인 구 : ", max.gu.name, " [ ", max.gu, " ]", "\n", 
     "서울 사고 발생이 최저인 구 : ", min.gu.name, " [ ", min.gu, " ]" )

barplot( seoul.gu.accident, main = "서울시 구별 발생 건수" )

# 8. 서울 월별 구별 사고 건수
seoul.1 <- subset( local.seoul, month == 1 )$발생건수
names( seoul.1 ) <- local.gu
print( "1월 구별 사고 건수" ); seoul.1
max.seoul.1 <- max( seoul.1 )
max.seoul.1.name <- names( seoul.1[ seoul.1 == max.seoul.1 ] )
min.seoul.1 <- min( seoul.1 )
min.seoul.1.name <- names( seoul.1[ seoul.1 == min.seoul.1 ] )
cat( "서울 1월 사고 발생이 최고인 구 : ", max.seoul.1.name, " [ ", max.seoul.1, " ]", "\n", 
     "서울 1월 사고 발생이 최저인 구 : ", min.seoul.1.name, " [ ", min.seoul.1, " ]" )

seoul.2 <- subset( local.seoul, month == 2 )$발생건수
names( seoul.2 ) <- local.gu
print( "2월 구별 사고 건수" ); seoul.2
max.seoul.2 <- max( seoul.2 )
max.seoul.2.name <- names( seoul.2[ seoul.2 == max.seoul.2 ] )
min.seoul.2 <- min( seoul.2 )
min.seoul.2.name <- names( seoul.2[ seoul.2 == min.seoul.2 ] )
cat( "서울 2월 사고 발생이 최고인 구 : ", max.seoul.2.name, " [ ", max.seoul.2, " ]", "\n", 
     "서울 2월 사고 발생이 최저인 구 : ", min.seoul.2.name, " [ ", min.seoul.2, " ]" )

seoul.3 <- subset( local.seoul, month == 3 )$발생건수
names( seoul.3 ) <- local.gu
print( "3월 구별 사고 건수" ); seoul.3
max.seoul.3 <- max( seoul.3 )
max.seoul.3.name <- names( seoul.3[ seoul.3 == max.seoul.3 ] )
min.seoul.3 <- min( seoul.3 )
min.seoul.3.name <- names( seoul.3[ seoul.3 == min.seoul.3 ] )
cat( "서울 3월 사고 발생이 최고인 구 : ", max.seoul.3.name, " [ ", max.seoul.3, " ]", "\n", 
     "서울 3월 사고 발생이 최저인 구 : ", min.seoul.3.name, " [ ", min.seoul.3, " ]" )

seoul.4 <- subset( local.seoul, month == 4 )$발생건수
names( seoul.4 ) <- local.gu
print( "4월 구별 사고 건수" ); seoul.4
max.seoul.4 <- max( seoul.4 )
max.seoul.4.name <- names( seoul.4[ seoul.4 == max.seoul.4 ] )
min.seoul.4 <- min( seoul.4 )
min.seoul.4.name <- names( seoul.4[ seoul.4 == min.seoul.4 ] )
cat( "서울 4월 사고 발생이 최고인 구 : ", max.seoul.4.name, " [ ", max.seoul.4, " ]", "\n", 
     "서울 4월 사고 발생이 최저인 구 : ", min.seoul.4.name, " [ ", min.seoul.4, " ]" )


seoul.5 <- subset( local.seoul, month == 5 )$발생건수
names( seoul.5 ) <- local.gu
print( "5월 구별 사고 건수" ); seoul.5
max.seoul.5 <- max( seoul.5 )
max.seoul.5.name <- names( seoul.5[ seoul.5 == max.seoul.5 ] )
min.seoul.5 <- min( seoul.5 )
min.seoul.5.name <- names( seoul.5[ seoul.5 == min.seoul.5 ] )
cat( "서울 5월 사고 발생이 최고인 구 : ", max.seoul.5.name, " [ ", max.seoul.5, " ]", "\n", 
     "서울 5월 사고 발생이 최저인 구 : ", min.seoul.5.name, " [ ", min.seoul.5, " ]" )

seoul.6 <- subset( local.seoul, month == 6 )$발생건수
names( seoul.6 ) <- local.gu
print( "1월 구별 사고 건수" ); seoul.6
max.seoul.6 <- max( seoul.6 )
max.seoul.6.name <- names( seoul.6[ seoul.6 == max.seoul.6 ] )
min.seoul.6 <- min( seoul.6 )
min.seoul.6.name <- names( seoul.6[ seoul.6 == min.seoul.6 ] )
cat( "서울 6월 사고 발생이 최고인 구 : ", max.seoul.6.name, " [ ", max.seoul.6, " ]", "\n", 
     "서울 6월 사고 발생이 최저인 구 : ", min.seoul.6.name, " [ ", min.seoul.6, " ]" )

seoul.7 <- subset( local.seoul, month == 7 )$발생건수
names( seoul.7 ) <- local.gu
print( "7월 구별 사고 건수" ); seoul.7
max.seoul.7 <- max( seoul.7 )
max.seoul.7.name <- names( seoul.7[ seoul.7 == max.seoul.7 ] )
min.seoul.7 <- min( seoul.7 )
min.seoul.7.name <- names( seoul.7[ seoul.7 == min.seoul.7 ] )
cat( "서울 7월 사고 발생이 최고인 구 : ", max.seoul.7.name, " [ ", max.seoul.7, " ]", "\n", 
     "서울 7월 사고 발생이 최저인 구 : ", min.seoul.7.name, " [ ", min.seoul.7, " ]" )

seoul.8 <- subset( local.seoul, month == 8 )$발생건수
names( seoul.8 ) <- local.gu
print( "8월 구별 사고 건수" ); seoul.8
max.seoul.8 <- max( seoul.8 )
max.seoul.8.name <- names( seoul.8[ seoul.8 == max.seoul.8 ] )
min.seoul.8 <- min( seoul.8 )
min.seoul.8.name <- names( seoul.8[ seoul.8 == min.seoul.8 ] )
cat( "서울 8월 사고 발생이 최고인 구 : ", max.seoul.8.name, " [ ", max.seoul.8, " ]", "\n", 
     "서울 8월 사고 발생이 최저인 구 : ", min.seoul.8.name, " [ ", min.seoul.8, " ]" )

seoul.9 <- subset( local.seoul, month == 9 )$발생건수
names( seoul.9 ) <- local.gu
print( "9월 구별 사고 건수" ); seoul.9
max.seoul.9 <- max( seoul.9 )
max.seoul.9.name <- names( seoul.9[ seoul.9 == max.seoul.9 ] )
min.seoul.9 <- min( seoul.9 )
min.seoul.9.name <- names( seoul.9[ seoul.9 == min.seoul.9 ] )
cat( "서울 9월 사고 발생이 최고인 구 : ", max.seoul.9.name, " [ ", max.seoul.9, " ]", "\n", 
     "서울 9월 사고 발생이 최저인 구 : ", min.seoul.9.name, " [ ", min.seoul.9, " ]" )

seoul.10 <- subset( local.seoul, month == 10 )$발생건수
names( seoul.10 ) <- local.gu
print( "10월 구별 사고 건수" ); seoul.10
max.seoul.10 <- max( seoul.10 )
max.seoul.10.name <- names( seoul.10[ seoul.10 == max.seoul.10 ] )
min.seoul.10 <- min( seoul.10 )
min.seoul.10.name <- names( seoul.10[ seoul.10 == min.seoul.10 ] )
cat( "서울 10월 사고 발생이 최고인 구 : ", max.seoul.10.name, " [ ", max.seoul.10, " ]", "\n", 
     "서울 10월 사고 발생이 최저인 구 : ", min.seoul.10.name, " [ ", min.seoul.10, " ]" )

seoul.11 <- subset( local.seoul, month == 11 )$발생건수
names( seoul.11 ) <- local.gu
print( "11월 구별 사고 건수" ); seoul.11
max.seoul.11 <- max( seoul.11 )
max.seoul.11.name <- names( seoul.11[ seoul.11 == max.seoul.11 ] )
min.seoul.11 <- min( seoul.11 )
min.seoul.11.name <- names( seoul.11[ seoul.11 == min.seoul.11 ] )
cat( "서울 11월 사고 발생이 최고인 구 : ", max.seoul.11.name, " [ ", max.seoul.11, " ]", "\n", 
     "서울 11월 사고 발생이 최저인 구 : ", min.seoul.11.name, " [ ", min.seoul.11, " ]" )

seoul.12 <- subset( local.seoul, month == 12 )$발생건수
names( seoul.12 ) <- local.gu
print( "12월 구별 사고 건수" ); seoul.12
max.seoul.12 <- max( seoul.12 )
max.seoul.12.name <- names( seoul.12[ seoul.12 == max.seoul.12 ] )
min.seoul.12 <- min( seoul.12 )
min.seoul.12.name <- names( seoul.12[ seoul.12 == min.seoul.12 ] )
cat( "서울 12월 사고 발생이 최고인 구 : ", max.seoul.12.name, " [ ", max.seoul.12, " ]", "\n", 
     "서울 12월 사고 발생이 최저인 구 : ", min.seoul.12.name, " [ ", min.seoul.12, " ]" )

seoul.spring <- sum( c( seoul.3, seoul.4, seoul.5 ) )
seoul.summer <- sum( c( seoul.6, seoul.7, seoul.8 ) )
seoul.fall <- sum( c( seoul.9, seoul.10, seoul.11 ) )
seoul.winter <- sum( c( seoul.12, seoul.1, seoul.1 ) )

season.accident <- c( seoul.spring, seoul.summer, seoul.fall, seoul.winter )
season <- c( '봄', '여름', '가을', '겨울' )
names( season.accident ) <- season

max.season <- max( season.accident )
max.season.name <- names( season.accident[ season.accident == max.season ] )
min.season <- min( season.accident )
min.season.name <- names( season.accident[ season.accident == min.season ] )

cat( "서울 사고 발생 최고 계절 : ", max.season.name, " [ ", max.season, " ]", "\n", 
     "서울 사고 발생 최저 계절 : ", min.season.name, " [ ", min.season, " ]" )

par( mfrow = c( 4, 1 ) )

plot( local.gu.length,
      seoul.3,                  
      main = "서울시 봄 사고건수",
      type = "b",              
      lty = 1,                 
      lwd = 1,                 
      col = "red",
      ylim = c( 50, 350 ),
      xlab = "구",          
      ylab = "발생건수" )       
lines( local.gu.length, seoul.4, type = "b", col = "blue" )
lines( local.gu.length, seoul.5, type = "b", col = "black" )

plot( local.gu.length,
      seoul.6,                  
      main = "서울시 여름 사고건수",
      type = "b",              
      lty = 1,                 
      lwd = 1,                 
      col = "red",
      ylim = c( 50, 350 ),
      xlab = "구",          
      ylab = "발생건수" )       
lines( local.gu.length, seoul.7, type = "b", col = "blue" )
lines( local.gu.length, seoul.8, type = "b", col = "black" )

plot( local.gu.length,
      seoul.9,                  
      main = "서울시 가을 사고건수",
      type = "b",              
      lty = 1,                 
      lwd = 1,                 
      col = "red",
      ylim = c( 50, 350 ),
      xlab = "구",          
      ylab = "발생건수" )       
lines( local.gu.length, seoul.10, type = "b", col = "blue" )
lines( local.gu.length, seoul.11, type = "b", col = "black" )

plot( local.gu.length,
      seoul.12,                  
      main = "서울시 겨울 사고건수",
      type = "b",              
      lty = 1,                 
      lwd = 1,                 
      col = "red",
      ylim = c( 50, 350 ),
      xlab = "구",          
      ylab = "발생건수" )       
lines( local.gu.length, seoul.1, type = "b", col = "blue" )
lines( local.gu.length, seoul.2, type = "b", col = "black" )

par( mfrow = c( 1, 1 ) )
