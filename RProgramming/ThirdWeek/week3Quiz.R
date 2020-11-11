# Quiz Week 3

library(datasets)
data(iris)
?iris

round(mean(iris[which(iris$Species == "virginica"),]$Sepal.Length))

apply(iris[1:4], 2, mean)

data(mtcars)
?mtcars

with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl),mean)


horse <- tapply(mtcars$hp, mtcars$cyl, mean)
horse
round(abs(horse[3]-horse[1]))


debug(ls)
# ls