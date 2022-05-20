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

conf <- loadCSV(filePathCf)
credentials <- loadCSV(filePathCd)

if( !is.null(conf)){
  articles <- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
  summaries <- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)])) 
}else{
  shinyalert(title="Salta de la aplicación", text="Se necesita el fichero de configuración para continuar",
             closeOnClickOutside = TRUE, type="error")
}

##### --> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
filePathAg <- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".csv"))
if(file.exists(filePathAg)){
  # if(nrow(conf)==1 && conf$init==0){
  #   agreemExists <- 0
  # }else{
    agreements <- loadCSV(filePathAg)
    agreemExists <- 1
  # }
}else{
  agreemExists <- 0
  print("need to create agreements")
}

expValidExists <- 0
filePathEV <- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".csv"))
#filePathEV <- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".RDS"))
if(file.exists(filePathEV)){
    expertsValidations <- loadCSV(filePathEV)
    expValidExists <- 1
}else{
  expValidExists <- 0
  print("need to create expertsValidationsFile")
}



# Save files when app stops
onStop(function(){
  message("Saliendo de la aplicación")
  # Save credentials 
  write.csv(credentials,file=filePathCd,row.names = FALSE)#, row.names = FALSE)
  # Save conf
  write.csv(x=conf,file=filePathCf,row.names = FALSE)
  # Save validations
  if(expValidExists == 1){
    #expertsValidations <- merge(expertsValidations,expertsValidationsCurrExp,by="idExp",all=TRUE) #--> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
    print(filePathEV)
    write.csv(x=expertsValidations,file=filePathEV,row.names = FALSE)
    #save(expertsValidations,file=filePathEV)
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

calcPairs <- function(x) {
  a <- factorial(x) / (2 * factorial(x - 2))
  if(is.nan(a))
    0
  else
    a
}

