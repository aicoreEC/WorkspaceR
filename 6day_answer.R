#
# day6_answer.R
#
# 6일차 다변량 자료 탐색 / EDA 실습
#

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
head( df )
tail( df )

# 3. 시도 이름 분리
local <- unique( df[ , 1 ] )
local

# 4. 서울 데이터 분리
local.seoul <- df[ df$시도 == local[ 1 ], ]
local.seoul

# 4. 서울 구별 이름 분리
local.gu <- unique( local.seoul[ , 2 ] )
local.gu

month <- unique( local.seoul[ , 3 ] )
month
month.index <- 1:12

# 5. 서울 월별 사고 건수 
seoul.accident <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
for ( i in 1:nrow( local.seoul ) ) {
      
}

