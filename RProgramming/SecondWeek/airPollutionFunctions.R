# Funciones de la tarea sobre contaminación atmosférica.

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calcultate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result
  
  means <- c()
  
  for(i in id){
    
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
    
    # Datos de cada monitor. 
    data <- read.csv(path)
    
    # Variable de contaminación seleccionada. 
    pollutionVar <- data[pollutant]
    
    # Vector de las medias de cada monitor. 
    means <- c(means, pollutionVar[!is.na(pollutionVar)])
    
  }
  
  # Media del conjunto de medias. 
  mean(means)
  
}



complete <- function(directory, id = 1:332){
  
  ## 'director' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the from:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Definición de un marco de datos vacío. 
  results <- data.frame(id = numeric(0), nobs = numeric(0))
  
  for(monitor in id){
    
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    
    # Datos de cada monitor. 
    data <- read.csv(path)
    
    # Eliminación de los valores perdidos de la variable sulfato. 
    cleanData <- data[(!is.na(data$sulfate)), ]
    
    # Eliminación de los valores perdidos de la variable nitrato. 
    cleanData <- cleanData[(!is.na(cleanData$nitrate)), ]
    
    # Número de observaciones. 
    nobs <- nrow(cleanData)
    
    # Se añade como una fila al marco de datos el id del monitor y el número de mediciones sin NA. 
    results <- rbind(results, data.frame(id = monitor, nobs = nobs))
    
  }
  
  results
  
}


corr <- function(directory, threshold = 0){
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completely observed observations (on all
  ## variables) requi?red to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  # Se declara una variable que contendrá el valor de la correlación.  
  corResults <- numeric(0)
  
  # Localización de los archivos. 
  complete_cases <- complete(directory)
  
  # Número de monitores con tantos o más mediciones que las exigidas por el umbral. 
  complete_cases <- complete_cases[complete_cases$nobs >= threshold, ]
  
  
  if(nrow(complete_cases) > 0){
    
    for(monitor in complete_cases$id){
      
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      
      data <- read.csv(path)
      
      cleanData <- data[(!is.na(data$sulfate)), ]
      
      cleanData <- cleanData[(!is.na(cleanData$nitrate)), ]
      
      sulfateData <- cleanData["sulfate"]
      
      nitrateData <- cleanData["nitrate"]
      
      # Se añade al vector la correlación entre los valores sulfato y nitrato en el monitor.
      corResults <- c(corResults, cor(sulfateData, nitrateData))
      
    }
  }
  
  corResults
}



pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

