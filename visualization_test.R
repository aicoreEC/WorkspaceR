install.packages( "DescTools" )

library(DescTools)

if(Sys.info()[["sysname"]] == "Darwin") { par(family="AppleGothic") }

tail(d.pizza)

Desc(d.pizza[,c("driver","temperature","count","weekday","wine_ordered")], plotit=TRUE)


Desc( c( mtcars$mpg ), plotit = TRUE )

library( ggplot2 )

data_bar = data.frame(xx = 1:10, yy = sample(1:3, 10, replace = T))
data_bar

ggplot(data=data_bar, aes(x=yy)) + 
  geom_bar()

ggplot(data=data_bar, aes(x=xx, y=yy)) +
  geom_bar(stat='identity')

ggplot(data=data_bar, aes(x=xx,y=yy)) + 
  geom_point(color = '#FFA500') 

ggplot(data=data_, aes(x=xx,y=yy))+ 
  geom_point(color = '#FFA500') 