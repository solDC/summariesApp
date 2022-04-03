library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(readr) #read txt more efficient
library(reactlog)

reactiveConsole(TRUE)

#setwd("~/shinyapp/summariesApp")
typeErrors <- list( "No contiene errores" = 1,
                    "No transmite el objetivo principal del texto" = 2,
                    "Oración mal formada" = 3,
                    "Contiene información inconsistente con el artículo" = 4,
                    "Contiene alguna información que no puede ser inferida del artículo" = 5)

#Load files --> only one time when app loads
#TODO: para todos los ficheros: 
#       función que chequee si existe el fichero o 
#       admin: que suba el fichero/diga donde está
#       usuario normal: mensaje de error en caso contrario

credentials <- read.csv(file="data/users.csv")

articles <- (read.csv("data/articles.csv")) #test dataset XL-Sum
#names(articles)[2] <- "idArticle"


#Articles and summaries are related by its position in the file 
summaries <- as.data.frame(read_lines("data/resumenesTEST.txt"))
names(summaries)[1] <- "summary"





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