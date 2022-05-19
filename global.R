library(shiny)
library(dplyr)
#library(readr)


set.seed(123)

# Set Local / RStudio Connect directories
outputDir <- "./data/outputs/"
inputDir <- "./data/inputs/"

# Utility function to load csv files from dropbox
loadCSV <- function(filePath){
  f <- tryCatch({
    # CAREFUL: next line needs to be the last one so the return value is the uploaded file
    read.csv(filePath,stringsAsFactors = FALSE)
  },
  error = function(e){
    msg <- paste0("Error: no se ha podido leer el fichero ",filePath)
    message(msg)
    message(e$message)
    #Shiny alerts solo sirve si ya dentro de la app, sino consola
    shinyalert(title="Salga de la aplicación y hable con su administrador",
               text=paste0("No se ha cargado el fichero ",filePath," necesario el funcionamiento de la aplicación."),
               type="error")
    return(NULL)
  })
}

# Load files -global variables
filePathCd <- file.path(inputDir,"users.csv")
filePathCf <- file.path(inputDir,"conf.csv")
filePathEV <- file.path(outputDir,"validations.csv")

conf <- loadCSV(filePathCf)
credentials <- loadCSV(filePathCd)

if( !is.null(conf)){
  articles <- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
  summaries <- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)])) 
}else{
  shinyalert(title="Salta de la aplicación", text="Se necesita el fichero de configuración para continuar",
             closeOnClickOutside = TRUE, type="error")
}

expertsValidations <- loadCSV(filePathEV)
###### --> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
expertsValidationsCurrExp <- as.data.frame(expertsValidations %>% filter(idExp == conf$id[nrow(conf)]))
# print(nrow(expertsValidations))
# print(nrow(expertsValidationsCurrExp))
print(expertsValidationsCurrExp)
newValid <- 0

##### --> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
filePathAg <- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".csv"))

if(file.exists(filePathAg)){
  if(nrow(conf)==1 && conf$init==0){
    agreemExists <- 0
  }else{
    agreements <- loadCSV(filePathAg)
    agreemExists <- 1
  }
}else{
  agreemExists <- 0
  print("need to create agreements")
}


# Save files when app stops
onStop(function(){
  # Save credentials 
  write.csv(credentials,file=filePathCd,row.names = FALSE)#, row.names = FALSE)
  # Save conf
  write.csv(x=conf,file=filePathCf,row.names = FALSE)
  # Save validations
  if(newValid > 0){
    expertsValidations <<- merge(expertsValidations,expertsValidationsCurrExp,by="idExp") #--> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
    write.csv(x=expertsValidations,file=filePathEV,row.names = FALSE) 
  }
  # Save agreements if created (generated when the first validation experiment starts)
  if (agreemExists == 1){
    write.csv(x=agreements,file=filePathAg,row.names = FALSE)
  }
})


# Functions Agreements

numCols <- function(x){
  factorial(x) / (2 * factorial(x-2))
}

