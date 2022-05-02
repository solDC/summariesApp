#summariesApp
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr) #slice_sample
#library(readr) #read txt more efficient
#library(reactlog) #reactive graph
library(rdrop2)
#library(httr)
library(shinyalert)
#library(log4r)
#library(shinylogs)

#reactiveConsole(TRUE)

# source("logSetup.R")
# log4r_info(paste0("Summaries App starts",Sys.time()))

# Done once to create Dropbox authentification tokens
#token<-drop_auth()
#saveRDS(token, "droptoken.rds")

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
    # OJO la siguiente linea tiene que ser la última para que devuelva el fichero, sino se rompe todo
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

#Load files --> only one time when app loads
conf <- loadCSV(inputDir,"conf.csv")
credentials <- loadCSV(inputDir,"users.csv")

set.seed(123)
if(is.null(conf)){
  message("Error al leer el fichero configuración, sin contenido")
} else{
  articles <- loadCSV(inputDir,conf$fileArticles)
  print(paste0("filas de articulos: ",nrow(articles)))
  summaries <- loadCSV(inputDir,conf$fileSummaries) #Articles and summaries are related by its position in the file 
  sampleSize <- round(conf$sampleSize*nrow(articles)/100,0)
  samplePositions <- sample(1:nrow(articles),sampleSize,replace=F)
  articles <- articles[samplePositions,]
  summaries <- as.data.frame(summaries[samplePositions,])
}

# Data structure of user validation responses
names <- c("usernameId","position","question1","question2","question3","timeStamp","summariesNameFile","articlesNameFile","articleTitle")
num<- length(names)
expertsValidationsColNames <- vector("character",8)
expertsValidationsColNames <- names

# Table used in the administrator dashboard
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
