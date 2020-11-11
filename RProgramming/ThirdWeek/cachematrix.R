# Put comments here that give an overall description of what your functions do

# Write a short comment describing this function

# Functions to save the inverse of an array.
# Assumption: all arrays supplied to the function must be inverse. 


# Create a special object with matrix structure that can hold the inverse. 
makeCacheMatrix <- function(m = matrix()){
  
  # Initialize the inverse.
  i <- NULL
  
  # Method for setting the matrix. 
  set <- function(matrix){
    
    m <<- matrix
    
    i <<- NULL
    
  }
  
  # Method to get the matrix. 
  get <- function(){
    
    m
    
  }
  
  # Method for calculating the inverse of the matrix. 
  setInverse <- function(inverse){
    
    i <<- inverse
    
  }
  
  # Method to fetch the inverse of the matrix. 
  getInverse <- function(){
    
    i
    
  }
  
  # A list that will return the results of each of the functions.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Write a short comment describing this function

# Function to calculate the inverse of the array created with makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    
    message("getting cached data")
    
    return(m)
    
  } else {
    
    # Get the matrix of the created object.
    data <- x$get()
    
    # Calculate the inverse of the matrix. 
    m <- solve(data) %*% data
    
    # Set the inverse in the object's memory.
    x$setInverse(m)
    
    # Return a matrix that is the inverse of x.
    m
    
  }
  
}
