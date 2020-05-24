#
# 8day_answer.R
#
# 8일차 ggplot 실습
#
#문1)
#R에서 제공하는 mtcars 데이터셋에서 gear(기어의 수)에 대해 ggplot으로
#막대그래프를 작성하시오. 단, 제목과 x축 레이블은 ‘기어의 수’, y축 레이블
#은 ‘빈도수’로 나타내시오.

library( ggplot2 )

tmp <- table( mtcars$gear )
df <- data.frame( tmp )
df

ggplot( df, aes( x = Var1, y = Freq ) ) + 
  geom_bar( stat = "identity", width = 0.7, fill = "steelblue" ) +
  ggtitle( "기어의 수" ) + 
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) ) +
  labs( x = "기어의수", y = "빈도수" ) 

#문2)
#R에서 제공하는 mtcars 데이터셋에서 cyl(실린더의 수)에 대해 막대 색이
#초록색인 막대그래프를 ggplot으로 작성하시오.

tmp <- table( mtcars$cyl )
df <- data.frame( tmp )
df
ggplot( df, aes( x = Var1,y = Freq ) ) + 
  geom_bar( stat = "identity", width = 0.7, fill = "green" ) +
  ggtitle( "실린더 수" ) + 
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) ) +
  labs( x = "실린더수", y = "빈도수" )

#문3) 
#R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 구간 간격이 5.0
#인 히스토그램을 ggplot으로 작성하시오.

ggplot( mtcars, aes( x = mpg ) ) +
  geom_histogram( binwidth = 5.0 )

#문4)
#R에서 제공하는 trees 데이터셋의 Girth(나무 둘레)에 대해 ggplot으로
#히스토그램을 작성하시오. 여기에서는 히스토그램의 제목, x축 레이블, y축
#레이블을 한글로 표시하시오. (구간 간격은 3.0, 막대의 색은 steelblue로 한다.)

ggplot( trees, aes( x = Girth ) ) +
  geom_histogram( binwidth = 3.0, fill = "steelblue" ) +
  ggtitle( "나무의 둘레" ) + 
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) ) +
  labs( x = "둘레", y = "빈도수" )

#문5)
#R에서 제공하는 mtcars 데이터셋에서 mpg(연비)를 x축으로 하고, wt(중
#량)를 y축으로 하는 산점도를 ggplot으로 작성하시오. (단, 점의 색은 gear의
#수에 따라 다르게 표시한다.)

ggplot( data = mtcars, aes( x = mpg, y = wt, color = gear ) ) +
  geom_point( size = 3 ) +
  ggtitle( "연비와 중량" ) +
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) )

#문6)
#R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 ggplot으로 상
#자그림을 작성하되, cyl(실린더 수)에 따라 그룹을 나누어 작성하시오.

ggplot( data = mtcars, aes( y = mpg, fill = factor( mtcars$cyl ) ) ) +
  geom_boxplot() +
  guides( fill = guide_legend( title = "실린더수" ) )

#문7) 
#다음은 2015년부터 2026년도까지의 예상 인구수 추계 자료이다. 연도를
#x축으로 하여 ggplot으로 선그래프를 작성하시오.
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
df <- data.frame( year, pop )

ggplot( data = df, aes( x = year,y = pop ) ) +
  geom_line( col = "red" )
