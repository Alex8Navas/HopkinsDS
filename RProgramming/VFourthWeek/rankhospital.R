# Rankhospital

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