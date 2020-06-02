install.packages( "DescTools" )

library(DescTools)

if(Sys.info()[["sysname"]] == "Darwin") { par(family="AppleGothic") }

tail(d.pizza)

Desc(d.pizza[,c("driver","temperature","count","weekday","wine_ordered")], plotit=TRUE)


Desc( c( mtcars$mpg ), plotit = TRUE )

