# Functions

add2 <- function(x, y){
  
  x + y
  
}

add2(7,8)

above10 <- function(vector){
  
  vector[vector > 10]
  
}

above10(c(1,18,13,2,11,3,15,12,14,8))

aboven <- function(vector, n){
  
  vector[vector > n]
  
}

aboven(c(1,18,13,2,11,3,15,12,14,8), 5)
aboven(c(1,18,13,2,11,3,15,12,14,8), 12)


columnmean <- function(dataframe, removeNA = TRUE){
  
  nc <- ncol(dataframe)
  
  means <- numeric(nc)
  
  for(i in 1:nc){
    
    means[i] <- mean(dataframe[,i], na.rm = removeNA)
    
  }
  
  return(means)
  
}

irisDF <-iris
str(irisDF)
columnmean(irisDF[1:4])




