fisrt <- function(){
  
  x <- rnorm(1000)
  mean(x)
  
}

second <- function(x){
  
  x + rnorm(length(x))
  
}