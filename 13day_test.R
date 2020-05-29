library(ggplot2)

str( sleep )
head( sleep )
tail( sleep )
sleep

ggplot(sleep, aes(ID, extra, fill=group))+
  geom_bar(stat='identity', position = 'dodge')

city <- c( '서울', '서울', '부산', '부산', '대구', '대구', '광주', '광주' )
tran <- c( 1, 2, 1, 2, 1, 2, 1, 2 )
value <- c( 22.3, 18.1, 21.9, 17.2, 20.3, 19.1, 30.1, 20.9 )
df <- data.frame( city = city, tran = tran, value = value )
df

ggplot(df, aes(tran, value, fill = city))+
  geom_bar(stat='identity', position = 'dodge')

str( mtcars )

ggplot( data = mtcars ) +
  geom_bar( aes( x = cyl, fill = as.factor( am ) ), position = "dodge" )


library( MASS )
str( Cars93 )
head( Cars93 )

ggplot(Cars93, aes(x=Passengers)) + 
  geom_histogram()

c <- c( 10, 20, 30 )
t <- c( 31.0, 49.0, 58.3 )
df <- data.frame( c = c, t = t )
str( df )
df

ggplot( df, aes( x = c, y = t ) ) +
  geom_line(  )


r1 <- c( "100", "200", "300" )
r2 <- c( "10", "20", "30" )
df <- data.frame( r1 = r1, r2 = r2 )
str( df )
df

r1.1 <- as.numeric( df[ , 1 ] )
str( r1.1 )



r <- 1:50
r
m <- matrix( r, nrow = 5 )
m

library( tidyverse )

str( Orange )
head( Orange )
tail( Orange )

Orange%>%
  filter(Tree==1)%>%
  ggplot(aes(age, circumference))+ 
  geom_line()

ggplot(Orange, aes(age, circumference, color = Tree))+
  geom_line()

ggplot(Orange, aes(age, circumference))+
  geom_line(aes(color = Tree))

r10 <- c( 10, 10, 10, 20, 20 )
r20 <- c( 1, 2, 3, 1, 2 )
r30 <- c( 25.2, 24.5, 30.3, 29.3, 31.9 )
df <- data.frame( speed = r10, city = r20, range= r30 )
df

str( df )

ggplot(df, aes( city, range ))+
  geom_line()

ggplot( data = df ) +
  geom_bar( aes( x = city,  fill = city ), position = "dodge" )
