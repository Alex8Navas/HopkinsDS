# Rankall

rankall <- function(outcome, num = "best"){
  
  # Lectura de los datos de salida. 
  outDT <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Se pasa el nombre a minúsculas (recordar que R es Case Sensitive). 
  outcome <- tolower(outcome)
  
  # Se reduce a las columnas hospital, state, heart attack, heart failure & pneumonia.
  outDT <- outDT[, c(2,7,11,17,23)] # Estas posiciones se deducen del str de arriba. 
  
  # Se renombran las columnas. 
  colnames(outDT) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  # Pasar la variable de estudio a numeric antes de entrar en los condicionales. 
  outDT[, eval(outcome)] <- as.numeric(outDT[, eval(outcome)])
  
  # Verificar que la variable de inetrés se ha escrito de forma correcta. 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    
    stop("La variable de estudio introducida no es válida")
    
  } else if (is.numeric(num)) {
    
    # Separar el marco de datos por Estado. 
    byState <- with(outDT, split(outDT, state))
    
    # Declaración de una lista vacía. 
    ordered  <- list()
    
    # Bucle que recorra la lista creada con split. 
    for (i in seq_along(byState)){
      
      # Establecer el orden según la variable de interés. 
      byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                         byState[[i]][, "hospital"]), ]
      
      # Incluir en la lista el mejor hospital del Estado. 
      ordered[[i]]  <- c(byState[[i]][num, "hospital"], byState[[i]][, "state"][1])
      
    }
    
    # Unir los elementos de la lista ordered. Esto crea una matriz con los mejores hospitales por Estado. 
    resultado <- do.call(rbind, ordered)
    
    # Crear un marco de datos a partir de la matriz. 
    resultadoDF <- as.data.frame(resultado, row.names = resultado[, 2], stringsAsFactors = FALSE)
    
    names(resultadoDF) <- c("hospital", "state")
    
  } else if (!is.numeric(num)) {
    
    if (num == "best") {
      
      # Separar el marco de datos por Estado. Esto crea una lista cuyos elementos son marcos de datos de cada Estado.
      byState <- with(outDT, split(outDT, state))
      
      # Declaración de una lista vacía. 
      ordered  <- list()
      
      # Recorrer la lista creada a partir del marco de datos. 
      for (i in seq_along(byState)){
        
        # Establecer el orden según la variable de interés.
        byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                           byState[[i]][, "hospital"]), ]
        
        # Incluir en la lista el mejor hospital del Estado. 
        ordered[[i]]  <- c(byState[[i]][1, c("hospital", "state")])
        
      }
      
      # Unir los elementos de la lista ordered. Esto crea una matriz con los mejores hospitales por Estado. 
      resultado <- do.call(rbind, ordered)
      
      resultadoDF <- as.data.frame(resultado, stringsAsFactors = FALSE)
      
      rownames(resultadoDF) <- resultadoDF[, 2]
      
    } else if (num == "worst") {
      
      byState <- with(outDT, split(outDT, state))
      
      ordered  <- list()
      
      for (i in seq_along(byState)){
        
        # Es como el anterior pero estableciendo que sea decreciente. 
        byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                           byState[[i]][, "hospital"], 
                                           decreasing = TRUE), ]
        
        ordered[[i]]  <- c(byState[[i]][1, c("hospital", "state")])
      }
      
      resultado <- do.call(rbind, ordered)
      
      resultadoDF <- as.data.frame(resultado, stringsAsFactors = FALSE)
      
      rownames(resultadoDF) <- resultadoDF[, 2]
      
    } else {
      
      stop("El número introducido no es válido")
      
    }
    
  }
  
  # Esto nos dará un marco de datos con:
  # Si num es numeric la posición especificada del hospital por Estado. 
  # Si num es best el mejor hospital por Estado para la variable de interés.
  # Si num es worst el peor hospital por Estado para la variable de interés. 
  return(resultadoDF)
  
}