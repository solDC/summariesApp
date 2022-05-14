library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr) 
library(tidyr) #spread
#library(readr) #read txt more efficient
library(rdrop2)
#library(httr)
library(shinyalert)

# Done once to create Dropbox authentification tokens
token<-drop_auth()
saveRDS(token, "droptoken.rds")

#Dropbox auth
token <- readRDS("droptoken.rds")

# Set Dropbox directories
outputDir <- "summariesApp/responses/"
inputDir <- "summariesApp/data/"

# Utility function to load csv files from dropbox
loadCSV <- function(path,fileName){
  f <- tryCatch({
    filePath <- paste0(path,fileName) #,".csv")
    print(filePath)
    # WARNING: next line needs to be the last one so the return value is the uploaded file
    drop_read_csv(file=filePath,dest = tempdir(),dtoken=token)
  },
  error = function(e){
    msg <- paste0("Error: no se ha podido leer el fichero ",filePath)
    message(msg)
    message(e$message)
    # shinyalert(title="Salga de la aplicación y hable con su administrador",
    #            text=paste0("No se ha cargado el fichero ",filePath," necesario el funcionamiento de la aplicación."),
    #            type="error")
    return(NULL)
  })
}

# Load files --> only one time when app loads
conf <- loadCSV(inputDir,"conf.csv")
credentials <- loadCSV(inputDir,"users.csv")

# Load Articles and Summaries
# Articles and Summaries are related by its position in the file
# I will only kept the ones the users need to validate (random sample depending on sample size defined by admin)
set.seed(123)
if(is.null(conf)){
  message("Error al leer el fichero configuración, sin contenido")
} else{
  articles <- loadCSV(inputDir,conf$fileArticles)
  numArticles <- nrow(articles)
  summaries <- loadCSV(inputDir,conf$fileSummaries)  
  sampleSize <- round(conf$sampleSize*numArticles/100,0)
  samplePositions <- sort(sample(1:numArticles,sampleSize,replace=F))
  articles <- articles[samplePositions,]
  articles$position <- samplePositions
  summaries <- as.data.frame(summaries[samplePositions,])
  summaries$position <- samplePositions
  colnames(summaries) <- c("summary","articlesPosition")
}

# Data structure of user validation responses
names <- c("usernameId","position","question1","question2","question3","timeStamp","summariesNameFile","articlesNameFile","articleTitle")
numColEV <- length(names)
expertsValidationsColNames <- vector("character",numColEV)
expertsValidationsColNames <- names

# Load experts validations (each file with anwers)
filesInfo <- drop_dir(outputDir)
if(dim(filesInfo)[1] >= 1){ 
  filePaths <- filesInfo$path_display
  expertsValidations <- lapply(filePaths, drop_read_csv, stringsAsFactors=FALSE)
  expertsValidations <- do.call(rbind,expertsValidations)
}
  