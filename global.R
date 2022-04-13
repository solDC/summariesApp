library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(readr) #read txt more efficient
library(reactlog)
library(rdrop2)

#reactiveConsole(TRUE)

# Done once to create Dropbox authentification tokens
# token<-drop_auth()
# saveRDS(token, "droptoken.rds")

# Dropbox auth
token <- readRDS("droptoken.rds")
outputDir <- "summariesApp/responses"

typeErrors <- list( "No contiene errores" = 1,
                    "No transmite el objetivo principal del texto" = 2,
                    "Contiene información inconsistente con el artículo" = 3,
                    "Contiene alguna información que no puede ser inferida del artículo" = 4)

#Load files --> only one time when app loads
#TODO: para todos los ficheros: 
#       función que chequee si existe el fichero o 
#       admin: que suba el fichero/diga donde está
#       usuario normal: mensaje de error en caso contrario

credentials <- drop_read_csv(file="summariesApp/data/users.csv",dest = tempdir(),dtoken=token) #read.csv(file="data/users.csv")

articles <- drop_read_csv(file="summariesApp/data/articles.csv",dest = tempdir(),dtoken=token)#read.csv("data/articles.csv") #test dataset XL-Sum

#Articles and summaries are related by its position in the file 
#summaries <- as.data.frame(read_lines("data/resumenesTEST.txt"))
summaries <- drop_read_csv(file="summariesApp/data/summaries.csv",dest = tempdir(),dtoken=token)


# Información base para tabla administrador
adminArticles <- articles %>% select(title,summary)
adminArticles$row_num <- seq.int(nrow(adminArticles)) 
adminArticles <- adminArticles[,c(ncol(adminArticles),1:(ncol(adminArticles)-1))] #dejar ron_num como primera columna del dataset
adminArticles <- cbind(adminArticles,summaries)
names(adminArticles)[4] <- "generatedSummary"
names(adminArticles)[3] <- "objectiveSummary"

#numCurrentValidations <- 0 ---> para actualizar la info del userAdmin si hay más validaciones (leer expertsValidations)

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

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

# 
# loadData <- function() {
#   # Read all the files into a list
#   filesInfo <- drop_dir(outputDir)
#   filePaths <- filesInfo$path_display
#   data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
#   # Concatenate all data together into one data.frame
#   data <- do.call(rbind, data)
#   data
# }


#runApp(list(ui = ui, server = server), launch.browser = TRUE) #