# Lexical Scoping

makeVector <- function(x = numeric()) {
  
  m <- NULL
  
  # Establecer el valor del vector. 
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  # Tomar el valor del vector. 
  get <- function(){
    
    x
    
    } 
  
  # Calcular la media. 
  setmean <- function(mean){
    
    m <<- mean
    
    }
  
  # Obtener el valor de la media. 
  getmean <- function() m
  
  # Guardar todos los valores calculados en una lista que devolverá la función. 
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}

# Función para calcular la media del vector creado con makeVector. 
cachemean <- function(x, ...) {
  
  m <- x$getmean()
  
  # Ver si la media ya está calculada. 
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
    
  } else {
    
    data <- x$get()
    
    m <- mean(data, ...)
    
    x$setmean(m)
    
    m
    
  }
  
}


# Funciones para guardar la inversa de una matriz.
# Supuesto: todas las matrices que se suministren a la función habrán de ser inversas. 


# Crear un objeto especial con estructura de matriz que pueda guardar la inversa. 
makeCacheMatrix <- function(m = matrix()){
  
  # Inicializar la inversa. 
  i <- NULL
  
  # Método para establecer la matriz. 
  set <- function(matrix){
    
    m <<- matrix
    
    i <<- NULL
    
  }
  
  # Método para obtener la matriz. 
  get <- function(){
    
    m
    
  }
  
  # Método para calcular la inversa de la matriz. 
  setInverse <- function(inverse){
    
    i <<- inverse
    
  }
  
  # Método para obtener la inversa de la matriz. 
  getInverse <- function(){
    
    i
    
  }
  
  # Lista que devolverá los resultados de cada una de las funciones. 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Función para calcular la inversa de la matriz creada con makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    
    message("getting cached data")
    
    return(m)
    
  } else {
    
    # Obtener la matriz del objeto creado. 
    data <- x$get()
    
    # Calcular la inversa de la matriz. 
    m <- solve(data) %*% data
    
    # Establecer la inversa en la memoria del objeto. 
    x$setInverse(m)
    
    # Devolver la inversa. 
    m
    
  }
  
}



