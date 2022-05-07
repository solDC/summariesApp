library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr) 
#library(readr) #read txt more efficient
library(rdrop2)
#library(httr)
library(shinyalert)

# Done once to create Dropbox authentification tokens
# token<-drop_auth()
# saveRDS(token, "droptoken.rds")

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
# I will only kept the ones the user will validate
set.seed(123)
if(is.null(conf)){
  message("Error al leer el fichero configuración, sin contenido")
} else{
  articles <- loadCSV(inputDir,conf$fileArticles)
  numArticles <- nrow(articles)
  #print(paste0("filas de articulos: ",numArticles))
  summaries <- loadCSV(inputDir,conf$fileSummaries)  
  numTestSample <- conf$sampleSize*numArticles/100
  sampleSize <- round(numTestSample,0)
  samplePositions <- sample(1:numArticles,sampleSize,replace=F)
  samplePositions <- sort(samplePositions)
  articles <- articles[samplePositions,]
  summaries <- as.data.frame(summaries[samplePositions,])
  # summaries$position <- samplePositions
  # colnames(summaries) <- c("summary","articlesPosition")
}

# Data structure of user validation responses
names <- c("usernameId","position","question1","question2","question3","timeStamp","summariesNameFile","articlesNameFile","articleTitle")
numColEV <- length(names)
expertsValidationsColNames <- vector("character",numColEV)
expertsValidationsColNames <- names

# Table used in the administrator dashboard --->   MOVER ESTO A SERVER EN LA PARTE DEL SI EL LOGADO ES EL ADMIN
if(is.null(articles)){
  message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
} else{
  adminArticles <- articles %>% select(title) #,summary) El resumen objetivo ya no sirve
  #adminArticles$row_num <- seq.int(nrow(adminArticles)) Ya no genero una secuencia porque tengo el sample size
  adminArticles$row_num <- samplePositions
  adminArticles <- adminArticles[,c(ncol(adminArticles),1:(ncol(adminArticles)-1))] #dejar ron_num como primera columna del dataset
  if(is.null(summaries)){
    message("El fichero con los resúmenes no se ha cargado.")
  }
  else{
    adminArticles <- cbind(adminArticles,summaries)
    names(adminArticles)[3] <- "generatedSummary" #Era el 4 campo pero me cargo objective summary
    #names(adminArticles)[3] <- "objectiveSummary"
    #print(adminArticles)
  }
}
