# Hospital Proyect

library(dplyr)

outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")

head(outcome)

dim(outcome)

names(outcome)

str(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11], main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", col = "steelblue",
      xlab = "Deaths")


best <- function(chosenState, outcome) {
  
  # Lectura de los datos de salida. 
  outDT <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Se pasa el nombre a minúsculas (recordar que R es Case Sensitive). 
  outcome <- tolower(outcome)
  
  # Se reduce a las columnas hospital, state, heart attack, heart failure & pneumonia.
  outDT <- outDT[, c(2,7,11,17,23)] # Estas posiciones se deducen del str de arriba. 
  
  # Se renombran las columnas. 
  colnames(outDT) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") 
  
  # Verificar que el nombre del Estado sea válido. 
  if (!chosenState %in% unique(outDT[["state"]])) {
    
    stop("Nombre de Estado incorrecto")
    
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
    # Verificar que la salida se encuentra en las variables que se quieren estudiar. 
    stop("Variable introducida incorrecta")
    
  } else {
    
    # Filtro por Estado. 
    outDTfState <- dplyr::filter(outDT, outDT$state == chosenState)
    
    # Cambiar la salida de la clase a variable numérica. 
    outDTfState[, outcome] <- as.numeric(outDTfState[,  eval(outcome)])
    
    # Eliminación de los valores perdidos. 
    outDTfState <- outDTfState[complete.cases(outDTfState),]
    
    # Ordenar la columna en orden ascendente (primero el hospital con menos casos).  
    outDTfState <- outDTfState %>% arrange(get(outcome))
    
    # Definir una lista con el mejor hospital y el marco de datos.
    resultado <- list(
      
      bestHospital = outDTfState[1,],
      top10Hospitals = head(outDTfState, 10)
      
      )
    
  }
  
  # Devolver la lista creada. 
  return(resultado)
  
}

best("TX", "heart attack")

# Comprobar el resultado: 
outDTfState[outDTfState$`heart attack` == min(outDTfState$`heart attack`),]

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack") # Nombre de Estado incorrecto.

best("NY", "hert attack") # Variable introducida incorrecta.



rankhospital <- function(chosenState, outcome, rank = "best"){
  
  # Lectura de los datos de salida. 
  outDT <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Se pasa el nombre a minúsculas (recordar que R es Case Sensitive). 
  outcome <- tolower(outcome)
  
  # Se reduce a las columnas hospital, state, heart attack, heart failure & pneumonia.
  outDT <- outDT[, c(2,7,11,17,23)] # Estas posiciones se deducen del str de arriba. 
  
  # Se renombran las columnas. 
  colnames(outDT) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  # Verificar la validez de las entradas (Estado & Variable):
  if (!chosenState %in% outDT[, "state"]) {
    
    stop("Nombre del Estado incorrecto")
    
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    
    stop("Tal variable no existe")
    
  } else if (is.numeric(rank)) {
    
    # Filtro por Estado. 
    outDTfState <- dplyr::filter(outDT, outDT$state == chosenState)
    
    # Cambiar la salida de la clase a variable numérica. 
    outDTfState[, outcome] <- as.numeric(outDTfState[,  eval(outcome)])
    
    # Eliminación de los valores perdidos. 
    outDTfState <- outDTfState[complete.cases(outDTfState),]
    
    # Ordenar el marco de datos por la variable de interés. 
    outDTfState <- outDTfState[order(outDTfState[, eval(outcome)], outDTfState[, "hospital"]), ]
    
    # Coger el valor especificado en el rango. 
    resultado <- outDTfState[, "hospital"][rank]
    
  } else if (!is.numeric(rank)){
    
    if (rank == "best") {
      
      output <- best(chosenState, outcome)
      
    } else if (rank == "worst") {
      
      # Filtro por Estado. 
      outDTfState <- dplyr::filter(outDT, outDT$state == chosenState)
      
      # Cambiar la salida de la clase a variable numérica. 
      outDTfState[, outcome] <- as.numeric(outDTfState[,  eval(outcome)])
      
      # Eliminación de los valores perdidos. 
      outDTfState <- outDTfState[complete.cases(outDTfState),]
      
      # Ordenar el marco de datos en orden ascendente según la variable de estudio. 
      outDTfState <- outDTfState %>% arrange(desc(get(outcome)))
      
      # Definir una lista con el mejor hospital y el marco de datos.
      resultado <- list(
        
        worstHospital = outDTfState[1,],
        last10Hospitals = head(outDTfState, 10)
        
      )
      
    } else {
      
      stop("Rango no válido")
      
    }
    
  }
  
  return(resultado)
  
}


rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)


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

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure", "best"), 10)

tail(rankall("heart failure"), 10)


# Programming Assignment 3: Quiz

best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")

rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
