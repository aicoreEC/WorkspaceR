# 
# 8일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 dplyr 패키지
#   2.2 ggplot 패키지
#   2.3 Word Cloud
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 dplyr패키지( https://dplyr.tidyverse.org/ )
#
# dplyr 패키지는 데이터 전처리 작업에 가장 많이 사용되는 패키지
#
install.packages( "dplyr" )

library( dplyr )

setwd( 'D:\\Workspace\\2020BigDataMaestro\\DaejeonWorkR' )
exam <- read.csv( 'exam.csv' )
exam

#
# filter() : dataset에 대해서 조건에 맞는 행 추출
#
filter( exam, class == 1 )
filter( mtcars, cyl == 4 )

# %>% : 파이프 연산자( pipe operator ), 물길을 연결하는 수도관처럼 함수들을 연결
#       하는 기능을 하는 연산자, ctrl + shift + m ( 단축키 )
#
exam %>% filter( class == 1 )
mtcars %>% filter( cyl == 4 )

exam %>% filter( class != 1 )

exam %>% filter( korean > 50 )

exam %>% filter( science <= 70 )

exam %>% filter( class == 1 & korean >= 50 )
mtcars %>% filter( cyl >= 6 & mpg > 20 )

exam %>% filter( korean >= 90 | science >= 90 )

exam %>% filter( class == 1 | class == 3 | class == 5 )

exam %>% filter( class %in% c( 1, 3, 5 ) )

class1 <- exam %>% filter( class == 1 )
class2 <- exam %>% filter( class == 2 )
mean( class1$korean )
mean( class2$korean )

#
# select() : dataset에 대해서 일부 변수만 추출
#
exam %>% select( korean )

exam %>% select( class, korean, science )

exam %>% select( -korean ) # 변수 제외

# %>%를 이용한 dplyr 함수 조합
exam %>% 
  filter( class == 1 ) %>% 
  select( korean )

exam %>% 
  select( id, korean ) %>% 
  head

iris %>% 
  filter( Species == 'setosa' ) %>% 
  select( Petal.Length, Petal.Width ) %>% 
  head

#
# arrange() : dataset에 대해서 원하는 순서로 정렬
#
exam %>% arrange( korean )

exam %>% arrange( desc( korean ) )

exam %>% arrange( class, desc( korean ) )

mtcars %>% 
  filter( mpg, cyl ) %>% 
  arrange( cyl, desc( mpg ) ) %>% 
  head

#
# mutate() : dataset에 대해서 파생변수 추가
#
# dplyr 패키지 함수들은 변수명 앞에 데이터프레임명을 반복해 입력하지 않기 때문에
# 스크립트가 간결해지는 장점이 있다.
exam %>% 
  mutate( total = korean + science ) %>% 
  head

exam %>% 
  mutate( total = korean + science,
          mean = ( korean + science ) / 2 ) %>% 
  head

exam %>% 
  mutate( grade = ifelse( science >= 60, 'pass', 'fail' ) ) %>% 
  head

exam %>% 
  mutate( total = korean + science ) %>% 
  arrange( desc( total ) )

#
# group_by(), summarise() : dataset에 대해서 평균이나 빈도처럼 요약한 값 추출
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
  summarise( mean_korea = mean( korean ),
             mean_science = mean( science ),
             median_korean = median( korean ),
             n = n() ) # n() : 행의 개수 count

mtcars %>% 
  group_by( cyl, gear ) %>% 
  summarise( mean_mpg = mean( mpg ) ) %>% 
  head


#
# dplyr 조합
#
mtcars %>% 
  group_by( cyl ) %>%                     # cyl별로 분리
  filter( gear == '4' ) %>%               # gear가 4인 data 추출
  mutate( tot_wt = sum( wt ) ) %>%        # wt의 합계 추가
  summarise( mean_wt = mean( wt ) ) %>%   # 평균 wt 산출
  arrange( desc( mean_wt ) )              # 평균 wt순 정렬
                                      

#
# distinct() : dataset에서 중복값 제거
#
mtcars %>% 
  distinct( cyl )

mtcars %>% 
  distinct( gear )

#
# 2.2 ggplot 패키지( https://ggplot2.tidyverse.org/index.html )
#
# ggplot : 보고서용 그래프와 같이 보다 미적인 그래프를 작성할 때 사용하는 시각화 패키지
#          복작하고 화려한 그래프를 작성할 수 있다는 장점이 있지만, 사용법이 어렵다는 단점이 있다.
# ggplot 명령문은 여러 개의 함수들을 연결하여 사용
#
# ggplot은 보통 하나의 ggplot()와 geom_xx() 함수들이 +로 연결되어 하나의 그래프를 완성
# ggplot() 함수의 매개변수로 그래프를 작성할 때 사용할 dataset( data = xx )과 
#                 dataset 안에서 x축, y축으로 사용할 열 이름( aes( x = x1, y = x2 ) )을 지정
#                 이 데이터를 이용하여 어떤 형태의 그래프를 그릴지를 geom_xx()를 통해 지정

#
# 막대 그래프
#
install.packages( "ggplot2" )

library( ggplot2 )
    
month <- c( 1, 2, 3, 4, 5, 6 )
rain <- c( 55, 50, 45, 50, 60, 70 )
df <- data.frame( month, rain )
df

ggplot( df, aes( x = month, y = rain ) ) +  # 그래프를 그릴 데이터 지정
  geom_bar( stat = "identity",              # 막대의 높이는 y축에 해당하는 열의 값
            width = 0.7,                    # 막대의 폭 지정
            fill = "steelblue" )            # 막대의 색 지정 

# 막대그래프 꾸미기
ggplot( df, aes( x = month, y = rain ) ) +
  geom_bar( stat = "identity",
            width = 0.7,
            fill = "steelblue" ) +
  ggtitle( "월별 강수량" ) +                # 그래프 제목 지정
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) ) + 
  labs( x = "월", y = "강수량" ) +          # 그래프 x, y축 레이블 지정
  coord_flip()                              # 그래프를 가로 방향으로 출력 
# theme : 지정된 그래프에 대한 제목의 폰트 크기, 색등 지정

#
# 히스토그램 
#
ggplot( iris, aes( x = Petal.Length ) ) +
  geom_histogram( binwidth = 0.5 )

# 그룹별 히스토그램
ggplot( iris, aes( x = Sepal.Width, fill = Species, color = Species ) ) +
  geom_histogram( binwidth = 0.5, position = "dodge" ) +
  theme( legend.position = "top" )

#
# 산점도
#
ggplot( data = iris, aes( x = Petal.Length, y = Petal.Width ) ) +
  geom_point()

# 그룹이 구분되는 산점도
ggplot( data = iris, aes( x = Petal.Length, y = Petal.Width, color = Species ) ) +
  geom_point( size = 3 ) +
  ggtitle( "꽃잎의 길이와 폭" ) +
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) )

#
# 상자그래프
#
ggplot( iris, aes( y = Petal.Length ) ) +
  geom_boxplot( fill = "yellow" )

# 그룹별 상자그래프
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
# corrplot 패키지를 이용하면 상관계수를 이용하여 그래프를 그릴 수가 있습니다.
# https://www.rdocumentation.org/packages/corrplot/versions/0.2-0/topics/corrplot
#
install.packages( "corrplot" )

library(corrplot)

m <- cor(mtcars)
m

corrplot( m, method="number" )

#
# 지도 
#
install.packages( "ggmap" )

library( ggmap )
register_google( key = 'AIzaSyB-O6sjEYVUzqvWCw67ojo4Q5x7uIjK6og' )

gc <- geocode( enc2utf8( "종로구" ) )    # 지점의 경도 위도
gc
cen <- as.numeric( gc )                  # 경도 위도를 숫자로
cen
map <- get_googlemap( center = cen )     # 지도 생성
ggmap( map )                             # 지도 화면에 보이기

gg_seoul <- get_googlemap( "seoul", maptype = "terrain" )
ggmap( gg_seoul )
#
# 2.3 word cloud
#
# word cloud : 텍스트 데이터를 분석하는 대표적이 방법
#              대상 데이터에서 단어( 주로 명사 )를 추출하고 단어들의 
#              출현 빈도수를 계산하여 시각화하는 기능
#              출현 빈도수가 높은 단어는 그만큼 중요하거나 관심도가 높다는 것을 의미
#              워드 클라우드는 단어의 출현 빈도수가 높을수록 큰 글씨로 표현
#
install.packages( "wordcloud" )
install.packages( "KoNLP" )

Sys.setenv( JAVA_HOME = "D:\\Java\\jdk1.8.0_181\\jre" )

library( )