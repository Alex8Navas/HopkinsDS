# Best

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