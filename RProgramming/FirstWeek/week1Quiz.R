# Week 1 Quiz 

x <- 4
class(x)

x <- 4L
class(x)

x <- c(4, TRUE)
class(x)

x <- c(4, "a", TRUE)
class(x)

x <- c(1,3, 5)
y <- c(3, 2, 10)
cbind(x,y)

x <- list(2, "a", "b", TRUE)
x[[1]]
class(x[[1]])
class(x[1])

x <- 1:4
y <- 2:3
z <- x + y
class(z)

x <- 1:4
y <- 2
z <- x + y
class(z)

x <- c(3, 5, 1, 10, 12, 6)
x[x<=5]<-0
x <- c(3, 5, 1, 10, 12, 6)
x[x<6]<-0
x <- c(3, 5, 1, 10, 12, 6)
x[x %in% 1:5]<-0

library(dplyr)
hw1 <- read.csv("data/hw1_data.csv")
str(hw1)
head(hw1)

names(hw1)

hw1[c(1,2),]

dim(hw1)

tail(hw1,2)

hw1[47,"Ozone"]

sum(is.na(hw1$Ozone))

mean(hw1$Ozone, na.rm = TRUE)

subset1hw1 <- hw1 %>% filter(Ozone > 31 & Temp > 90)
subset1hw1
mean(subset1hw1$Solar.R)

subset2hw1 <- hw1 %>% filter(Month == 6)
subset2hw1
mean(subset2hw1$Temp)

subset3hw1 <- hw1 %>% filter(Month == 5)
subset3hw1
max(subset3hw1$Ozone, na.rm = TRUE)
