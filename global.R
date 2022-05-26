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
message("Cargados credentials y conf")

if( !is.null(conf)){
    articles <- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
    numArticlesG <- nrow(articles)
    summaries <- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)])) 
}else{
  message("Salta de la aplicación. Se necesita el fichero de configuración para continuar.")
}

filePathAV <- file.path(outputDir,paste0("articlesValidate-",conf$id[nrow(conf)],".rds"))
print(filePathAV)
if(file.exists(filePathAV)){
  articlesToValidate <- readRDS(filePathAV)
  articlesToValidateExists <- 1
  rm(articles)
  rm(summaries)
}else{
  articlesToValidateExists <- 0
  articlesToValidate <- NULL
  message("need to create articlesToValidate")
}

filePathAg <- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".rds"))
print(filePathAg)
if(file.exists(filePathAg)){
  agreements <- readRDS(filePathAg)
  agreemExists <- 1
}else{
  agreemExists <- 0
  agreements <- NULL
  message("need to create agreements")
}

filePathEV <- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".rds"))
print(filePathEV)
if(file.exists(filePathEV)){
  expertsValidations <- readRDS(file=filePathEV)
  expValidExists <- 1
}else{
  expValidExists <- 0
  expertsValidations <- NULL
  message("Need to create expertsValidationsFile")
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

