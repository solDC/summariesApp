#summariesApp
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(readr) #read txt more efficient
#library(reactlog) #reactive graph
library(rdrop2)
#library(httr)
library(shinyalert)
#library(log4r)
#library(shinylogs)

#reactiveConsole(TRUE)

# source("logSetup.R")
# log4r_info("Summaries App starts")

# Done once to create Dropbox authentification tokens
#token<-drop_auth()
#saveRDS(token, "droptoken.rds")

#Dropbox auth
token <- readRDS("droptoken.rds")

set.seed(123)

# Set Dropbox directories
outputDir <- "summariesApp/responses/"
inputDir <- "summariesApp/data/"

# Utility function to load csv files from dropbox
loadCSV <- function(path,fileName){
  f <- tryCatch({
    filePath <- paste0(path,fileName) #,".csv")
    drop_read_csv(file=filePath,dest = tempdir(),dtoken=token)
  },
  error = function(e){
    msg <- paste0("Error: no se ha podido leer el fichero ",filePath)
    message(msg)
    message(e)
    shinyalert(title="Salga de la aplicación y hable con su administrador",
               text=paste0("No se ha cargado el fichero ",filePath," necesario el funcionamiento de la aplicación."),
               type="error")
    return(NULL)
  },
  warning = function(w){
    msg <- paste0("Warning causado por el fichero ",filePath)
    message(msg)
    message(w)
  }  #, finally = {}
  )
}

#Load files --> only one time when app loads
conf <- loadCSV(inputDir,"conf.csv")
credentials <- loadCSV(inputDir,"users.csv")
articles <- loadCSV(inputDir,"articles.csv")
#summaries <- loadCSV(inputDir,"summaries") #Articles and summaries are related by its position in the file 
summaries <- loadCSV(inputDir,conf$fileSummaries) #Articles and summaries are related by its position in the file 


# # Table used in the administrator dashboard
# tryCatch({
#   if(is.null(articles)){
#     message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
#   }
#   else{
#     adminArticles <- articles %>% select(title,summary)
#     adminArticles$row_num <- seq.int(nrow(adminArticles)) 
#     adminArticles <- adminArticles[,c(ncol(adminArticles),1:(ncol(adminArticles)-1))] #dejar ron_num como primera columna del dataset
#     if(is.null(summaries)){
#       message("El fichero con los resúmenes no se ha cargado.")
#     }
#     else{
#       adminArticles <- cbind(adminArticles,summaries)
#       names(adminArticles)[4] <- "generatedSummary"
#       names(adminArticles)[3] <- "objectiveSummary"  
#     }
#   }
# })





#runApp(list(ui = ui, server = server), launch.browser = TRUE) #