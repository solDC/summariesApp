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
    msg <- paste0("Error. Problema con el fichero ",filePath)
    message(msg)
    message(e)
    return(NULL)
  },
  warning = function(w){
    msg <- paste0("Warning causado por el fichero ",filePath)
    message(msg)
    message(w)
  }  #, finally = {}
  )
}

conf <- loadCSV(inputDir,"conf.csv")

#Load files --> only one time when app loads
credentials <- loadCSV(inputDir,"users.csv")
articles <- loadCSV(inputDir,"articles.csv")
#summaries <- loadCSV(inputDir,"summaries") #Articles and summaries are related by its position in the file 
summaries <- loadCSV(inputDir,conf$fileSummaries) #Articles and summaries are related by its position in the file 

# Table used in the administrator dashboard
tryCatch({
  if(is.null(articles)){
    message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
  }
  else{
    adminArticles <- articles %>% select(title,summary)
    adminArticles$row_num <- seq.int(nrow(adminArticles)) 
    adminArticles <- adminArticles[,c(ncol(adminArticles),1:(ncol(adminArticles)-1))] #dejar ron_num como primera columna del dataset
    if(is.null(summaries)){
      message("El fichero con los resúmenes no se ha cargado.")
    }
    else{
      adminArticles <- cbind(adminArticles,summaries)
      names(adminArticles)[4] <- "generatedSummary"
      names(adminArticles)[3] <- "objectiveSummary"  
    }
  }
})

############
# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),#,icon=icon("far fa-sign-in",lib= "font-awesome")
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     #a("Create new account"),
                     br(),
                     tags$code("Username: admin  Password: adminpass"),
                     br(),
                     tags$code("Username: user1  Password: pass1"),
                     br(),
                     tags$code("Username: user2  Password: pass2")
                   ))
)




#runApp(list(ui = ui, server = server), launch.browser = TRUE) #