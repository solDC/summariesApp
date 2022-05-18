library(shiny)
library(rdrop2)
library(httpuv)

# Done once to create Dropbox authentification tokens
token<-drop_auth()
saveRDS(token, "droptoken.rds")
# 
# #Dropbox auth
# token <- readRDS("droptoken.rds")

# dropbox_endpoint <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize",
#                                          access = "https://api.dropbox.com/oauth2/token")
# 
# dropbox_app <- httr::oauth_app(appname="summariesApp", key = "ao4u5xpe7wtfg2k",
#                                secret = "z760md21t5v78xi")#, redirect_uri = "https://soldc.shinyapps.io/summariesApp/")
# dropbox_token <- httr::oauth2.0_token(endpoint=dropbox_endpoint, app=dropbox_app,
#                                       cache = TRUE,
#                                       query_authorize_extra = list(token_access_type = "offline"))

token <- readRDS("droptoken.rds")

# Set Dropbox directories
outputDir <- "summariesApp/responses/"
inputDir <- "summariesApp/data/"

set.seed(123)

# Utility function to load csv files from dropbox
loadCSV <- function(filePath){
  f <- tryCatch({
    # WARNING: next line needs to be the last one so the return value is the uploaded file
    drop_read_csv(file=filePath,dest = tempdir(),dtoken=token)
  },
  error = function(e){
    msg <- paste0("Error: no se ha podido leer el fichero ",filePath)
    message(msg)
    message(e$message)
    #Shiny alerts solo sirve si dentro de la app, sino consola
    shinyalert(title="Salga de la aplicación y hable con su administrador",
               text=paste0("No se ha cargado el fichero ",filePath," necesario el funcionamiento de la aplicación."),
               type="error")
    return(NULL)
  })
}

conf <- loadCSV(paste0(inputDir,"conf.csv"))
credentials <- loadCSV(paste0(inputDir,"users.csv"))

if( !is.null(conf)){
  articles <- loadCSV(paste0(inputDir,conf$fileArticles))
  summaries <- loadCSV(paste0(inputDir,conf$fileSummaries)) 
}else{
  shinyalert(title="Salta de la aplicación", text="Se necesita el fichero de configuración para continuar",
             closeOnClickOutside = TRUE, type="error")
}


onStop(function(){
  # Save credentials 
  filePathCd <- file.path(tempdir(),"users.csv")
  write.table(credentials,file=filePathCd,append = FALSE,sep=',',col.names = TRUE, row.names = FALSE)
  drop_upload(filePathCd,inputDir,mode = "overwrite")
  # Save conf
  filePathCf <- file.path(tempdir(),"conf.csv")
  write.table(conf,file=filePathCf,append = FALSE,sep=',',row.names = FALSE) 
  drop_upload(filePathCf,inputDir)
})



# Main login screen
#####
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