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
df <- read.table( file = "airquality.txt", header = T )
df

class( df )

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


#
# 2.3 단일 변수 범주형 자료 탐색
#

#
# 2.4 단일 변수 연속형 자료 탐색
#

