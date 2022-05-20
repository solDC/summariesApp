library(shiny)
library(dplyr)
#library(readr)
library(rstan)


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
    return(NULL)
  })
}

# Load files -global variables
filePathCd <- file.path(inputDir,"users.csv")
filePathCf <- file.path(inputDir,"conf.csv")

conf <- loadCSV(filePathCf)
credentials <- loadCSV(filePathCd)
print(conf)
print(credentials)

filePathAV <- file.path(inputDir,paste0("articlesToValidate-",conf$id[nrow(conf)],".rds"))
if( !is.null(conf)){
  if(file.exists(filePathAV)){
    articlesToValidate <- readRDS(filePathAV)
    articlesToValidateExists <- 1
  }else{
    articlesToValidateExists <<- 0
    articles <- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
    summaries <- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)])) 
  }
}else{
  message("Salta de la aplicación. Se necesita el fichero de configuración para continuar.")
}

##### --> --> --> SOL CHEQUEAR DESPUES DE GUARDAR VALIDACIONES USUARIOS
filePathAg <- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".rds"))
if(file.exists(filePathAg)){
  # if(nrow(conf)==1 && conf$init==0){
  #   agreemExists <- 0
  # }else{
    #agreements <- loadCSV(filePathAg)
  agreements <- readRDS(filePathAg)
  agreemExists <- 1
  # }
}else{
  agreemExists <- 0
  message("need to create agreements")
}

expValidExists <- 0
filePathEV <- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".rds"))
if(file.exists(filePathEV)){
    #expertsValidations <- loadCSV(filePathEV)
  expertsValidations <- readRDS(file=filePathEV)
  expValidExists <- 1
}else{
  expValidExists <- 0
  message("Need to create expertsValidationsFile")
}

# Save files when app stops
onStop(function(){
  message("Saliendo de la aplicación")
  # Save credentials 
  write.csv(credentials,file=filePathCd,row.names = FALSE)#, row.names = FALSE)
  # Save conf
  write.csv(x=conf,file=filePathCf,row.names = FALSE)
  # Save articles To Validate
  if(articlesToValidateExists == 1){
    saveRDS(articlesToValidate,file=filePathAV)
  }
  # Save validations
  if(expValidExists == 1){
    print(filePathEV)
    saveRDS(expertsValidations,file=filePathEV)
  }
  # Save agreements if created (generated when the first validation experiment starts)
  if (agreemExists == 1){
    saveRDS(agreements,file=filePathAg)
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

