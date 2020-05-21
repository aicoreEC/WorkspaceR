# 
# 8일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 dplyr 패키지
#   2.2 ggplot 패키지
#   2.3 Markdown 사용법
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 dplyr패키지
#
# dplyr 패키지는 데이터 전처리 작업에 가장 많이 사용되는
# 패키지
# ( https://dplyr.tidyverse.org/ )
# ( https://www.tidyverse.org/packages/ )
#
install.packages( "tidyverse" )
 
setwd( "C:\\WorkspaceBigDaejeon\\WorkspaceR" )
exam <- read.csv( 'exam.csv', header = T )
exam

library( dplyr )

#
# filter() : dataset에 대해서 조건에 맞는 행 추출 함수
#
filter( exam, class == 1 )
filter( mtcars, cyl == 4 )

#
# %>% ( 파이프 연산자, pipe operator )
#   물길을 연결하는 수도관처럼 함수들을 연결하는 기능을
#   하는 연산자
#   입력 단축키 : ctrl + shift + m
#
exam %>% filter( class == 1 )

mtcars %>% 
    filter( cyl == 4 )

exam %>% 
    filter( class != 1 )

exam %>% 
    filter( korean > 50 )

exam %>% 
    filter( science <= 70 )

exam %>% 
    filter( class == 1 & korean >= 50 )

mtcars %>% 
    filter( cyl >= 6 & mpg > 20 )

exam %>% 
    filter( korean >= 90 | science >= 90 )

exam %>% 
    filter( class == 1 | class == 3 | class == 5 )

exam %>% 
    filter( class %in% c( 1, 3, 5 ) )

class1 <- exam %>% filter( class == 1 )
class1
class2 <- exam %>% filter( class == 2 )
class2 

mean( class1$korean )
mean( class2$korean )

#
# select() : dataset에 대해서 일부 변수만 추출 하는 함수
#
exam %>% 
    select( korean )

exam %>% 
    select( class, korean, science )

exam %>% 
    select( -korean )

# %>% 연산자를 이용하여 dplyr 함수 조합
exam %>% 
    filter( class == 1 ) %>% 
    select( korean )

exam %>% 
    select( id, korean ) %>% 
    head

iris %>% 
    filter( Species == "setosa" ) %>% 
    select( Petal.Length, Petal.Width ) %>% 
    tail

#
# arrange() : dataset에 대해서 원하는 순서로 정렬하는 함수
#
exam %>% 
    arrange( korean )

exam %>% 
    arrange( desc( korean ) )  # 내림차순

exam %>% 
    arrange( class, desc( korean ) )

mtcars %>% 
    filter( mpg >= 15.0 ) %>% 
    select( mpg, cyl ) %>% 
    arrange( cyl, desc( mpg ) ) %>% 
    head( 15 )

# dplyr 패키지의 함수들은 함수 인수로 dataset을 반복해
# 입력하지 않고 파이프 연산자를 이용하여 결과를 사용함
# 으로써 스크립트가 간결해지는 장점이 있다.

#
# mutate() : dataset에 대한 파생변수 추가 함수
#
exam %>% 
    mutate( total = korean + science ) %>% 
    head

exam %>% 
    mutate( total = korean + science ,
            average = ( korean + science ) / 2 ) %>% 
    head

# ifelse( science >= 60, 'pass', 'fail' ) : 삼항연산자
#             조건       참일때  거짓일때
#
exam %>% 
    mutate( grade = ifelse( science >= 60, 'pass', 'fail' ) ) %>% 
    head

exam <- exam %>% 
            mutate( total = korean + science,
                    average = ( korean + science ) / 2 )
exam %>% 
    mutate( grade = ifelse ( average >= 90 , 'A',
                        ifelse ( average >= 80, 'B',
                                 ifelse ( average >= 70, 'C',
                                          ifelse ( average >= 60, 'D',
                                                   'F' ) ) ) ) ) %>% 
    head


#
# group_by() : dataset에 대하여 그룹으로 나누는 함수
#
# summarise() : dataset에 대하여 요약한 값을 추출하는 함수
#
exam %>% 
    summarise( mean_korean = mean( korean ) )

exam %>% 
    group_by( class ) %>% 
    summarise( mean_korean = mean( korean ) )

mtcars %>% 
    group_by( cyl ) %>% 
    summarise( mean_mpg = mean( mpg ) )

exam %>% 
    group_by( class ) %>% 
    summarise( mean_korean = mean( korean ),
               mean_science = mean( science ),
               median_korean = median( korean ),
               median_science = median( science ), 
               n = n() )    # n() : 행의 개수 count

mtcars %>% 
    group_by( cyl, gear ) %>% 
    summarise( mean_mpg = mean( mpg ) ) %>% 
    head

#
# dplyr 패키지 함수의 조합
#
# 하고자 하는일 : mtcars dataset에서 cyl별로 분리한 후 gear가 4인
#                 data를 추출한 후 wt의 합계를 추가하고 wt의 평균을
#                 산출한 후 wt 평균순 으로 정렬하여 결과 추출
mtcars %>% 
    group_by( cyl ) %>% 
    filter( gear == '4' ) %>% 
    mutate( tot_wt = sum( wt ) ) %>% 
    summarise( mean_wt = mean( wt ) )  %>% 
    arrange( desc( mean_wt ) )

#
# distinct() : dataset에서 중복값 제거 함수
#
mtcars %>% 
    distinct( cyl )

mtcars %>% 
    distinct( gear )

#
# 2.2 ggplot 패키지( https://ggplot2.tidyverse.org/index.html )
# 
# ggplot : 보고서용 그래프와 같이 보다 미적인 그래프를 작성할 때 사용
#          하는 시각화 패키지
#          복잡하고 화려한 그래프를 작성할 수 있다는 장점은 있지만,
#          사용법이 어렵다는 단점이 있다.
#
# ggplot 패키지의 함수는 여러 개의 함수를 연결하여 사용한다.
#
# ggplot 패키지는 보통 하나의 ggplot()와 geom_xx() 함수들이 +로 연결
#        되어 하나의 그래프를 완성한다.
#
# ggplot()의 매개변수로 그래프를 작성할 때 사용할 dataset( data = xx )과
#       dataset 안에서 x축, y축으로 사용할 열 이름( aes( x = x1, y = x2 ) )
#       를 지정하고 +로 연결하여 어떤 형태의 그래프를 그릴지를
#       geom_xx()를 통해 저장한다.

library( ggplot2 )

month <- c( 1, 2, 3, 4, 5, 6 )
rain <- c( 55, 50, 45, 50, 60, 70 )
df <- data.frame( month, rain )
df

ggplot( df, aes( x = month, y = rain ) ) + # dataset, x, y
    geom_bar( stat = "identity",      # 막대 높이, y축값
              width = 0.7,            # 막대 폭
              fill = "steelblue" )    # 막대 색

# 막대 그래프 꾸미기
ggplot( df, aes( x = month, y = rain ) ) + 
    geom_bar( stat = "identity",      
              width = 0.7,            
              fill = "steelblue" ) +   
    ggtitle( "월별 강수량" ) + 
    theme( plot.title = element_text( size = 25,
                                      face = "bold",
                            colour = "steelblue" ) ) +
    labs( x = "월", y = "강수량" ) +
    coord_flip()
# ggtitle() : 그래프 제목
# labs() : x축, y축 레이블 지정
# theme() : 지정된 그래프에 대한 제목의 폰트크기, 색등 지정
# coord_flip() : 그래프를 가로 방향으로 출력


#
# 히스토그램
#
ggplot( iris, aes( x = Petal.Length ) ) +
    geom_histogram( binwidth = 0.5 )

# 그룹별 히스토그램
ggplot( iris, aes( x = Sepal.Length, 
                   fill = Species,
                   color = Species ) ) +
    geom_histogram( binwidth = 0.5, position = "dodge" ) +
    theme( legend.position = "top" )

#
# 산점도
#
ggplot( data = iris, aes( x = Petal.Length,
                          y = Petal.Width ) ) +
    geom_point()

# 그룹으로 구분되는 산점도
ggplot( data = iris, aes( x = Petal.Length, 
                          y = Petal.Width,
                          color = Species ) ) +
    geom_point( size = 3 ) +
    ggtitle( "꽃잎의 길이와 폭" ) +
    theme( plot.title = element_text( size = 25,
                                      face = "bold",
                            colour = "steelblue" ) )

#
# 상자수염그래프
#
ggplot( iris, aes( y = Petal.Length ) ) +
    geom_boxplot( fill = "steelblue" )

# 그룹별 상자수염그래프
ggplot( iris, aes( y = Petal.Length, fill = Species ) ) +
    geom_boxplot()



# 
# 선그래프
#
year <- 1937:1960
cnt <- as.vector( airmiles )
df <- data.frame( year, cnt )
head( df )

ggplot( df, aes( x = year, y = cnt ) ) +
    geom_line( col = "red" )

#
# 상관계수 시각화
#
# corrplot 패키지를 이용하면 상관계수를 이용한
# 시각화를 표현할 수 있다.
#
# https://www.rdocumentation.org/packages/
#                 corrplot/versions/0.2-0/topics/corrplot 
#
install.packages( "corrplot" )

library( corrplot )

m <- cor( mtcars )
m

corrplot( m, method = "color" )

#
# Treemap( 나무 지도 ) : 데이터가 갖는 계층 구조를 타일
#                        모양으로 표현하는 시각화 도구
#  http://www.cs.umd.edu/hcil/treemap/ )
#
install.packages( "treemap" )

library( treemap )

data( GNI2014 )
str( GNI2014 )

treemap( GNI2014, index = c( "continent", "iso3" ),
         vSize = "population",
         vColor = "GNI",
         type = "value",
         bg.labels = "yellow" )
# index : 계층 구조 표현
# vSize : 각 모양크기, vColor : 각 모양 색상
# type : 각 모양 색깔 방법, bg.labels : 전체 배경색

#
# bubble chart : bubble chart는 산점도의 한 종류
#                변수의 크기에 따라 bubble로 표현하는
#                시각화 도구
#
library( MASS )
str( UScrime )
head( UScrime )

radius <- sqrt( UScrime$Pop ) # 원의 반지름
symbols( UScrime$U2, UScrime$y, # x, y 좌표 위치
         circle = radius,       # 원의 반지름
         inches = 0.4,          # 원의 크기 조절값
         fg = "white", bg = "lightgray", # 원 테두리, 바탕색
         lwd = 1.5,             # 원의 테두리선 두께
         xlab = "unemployment 35~39 miles", # x축 설명
         ylab = "crime rate",               # y축 설명
         main = "UScrime Data" )            # 제목
text( UScrime$U2, UScrime$y,      # 문자 출력 x, y 위치
      1:nrow( UScrime ),          # 문자로 출력할 값
      cex = 0.8,                  # 글자 크기
      col = "brown" )             # 글자 색

# https://developers.google.com/chart/interactive/
#           docs/gallery/bubblechart#Configuration_Options

#
# 2.3 Markdown 사용법
#
# Markdown : 쉽게 문서를 작성하여 HTML, PDF, Word와 같은 
#            도구에 표현할 수 있도록 하는 문석 작성 언어
#            여러 Program Language나 Web에서 사용 가능
#
# * 데이터 분석 보고서를 신뢰할 수 있으려면 동일한 분석과정을
#   거쳤을때 동일한 분석 결과가 반복되어 나오도록 재현성을
#   갖추어야 한다. 
#














