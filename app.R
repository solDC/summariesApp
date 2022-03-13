library(shiny)
library(shinyauthr)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium) #para encriptar contraseñas
library(tidyverse)
library(dplyr)
library(digest)

source("~/shinyapp/summariesApp/aux.R")

# #global variables
# fields <- c("user","title","error")
# responsesDir <- file.path("~/shinyapp/summariesApp/data")
# epochTime <- function() {
#   as.integer(Sys.time())
# }


credentials <- read.csv(file="data/users.csv")

#Ahora el texto y los resumenes están en un solo fichero, hay que ver como vienen
text <- read.csv("data/summaries.csv")
#text$title <- as.factor(text$title)
str(text)


#Titulos de textos que hay que validar porque tienen menos de 3 validacions --> 
# esto tiene que cambiar porque 3 es variable parametrica y sino hay acuerdo necesitamos más checks de expertos
numChoicesSummVal <- text %>% filter(numValid < 3) %>%  count()
choicesSummVal <- text %>% filter(numValid < 3)  %>% select(title) 


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


#UI
header <- dashboardHeader( title = "Validación de resúmenes", titleWidth = 250, uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(width = 250,uiOutput("sidebarpanel")) 

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))


ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            pasverify
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  typeUser <- reactive ({
    req(USER$login)
    Username <- isolate(input$userName)
    credentials["permission"][which(credentials$username_id==Username),]
  }) 
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("far fa-sign-out",lib = "font-awesome"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })

  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE){
      sidebarMenu(
        if(typeUser() != "admin"){ #== "expert" comparo contra admin en caso de crear el super-expert
          #menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt",lib = "font-awesome")),
          menuItem("Validar resumen", tabName = "validate", icon = icon("th",lib = "font-awesome"))
          }
        else{
          menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt",lib = "font-awesome"))
          }
      )
    }
  })

  output$body <- renderUI({
    if (USER$login == TRUE) {
      tabItems(
        # comparaba contra expert pero tal vez cree usuario de tipo super-expert para cuando no hay acuerdo entre experts normales
        if(typeUser() != "admin"){
          tabItem(tabName = "validate", class = "active",
                  fluidRow(
                    box(width=4, title="Título", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                        selectInput("selectTitle",label=("Seleccione el artículo a validar"),choices = choicesSummVal))
                  ),
                  
                  fluidRow(
                    box(width=12, title="Artículo y resumen",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        tableOutput('contents'),
                    # selectInput("selectError", label = h3("Select type of error"), 
                    #             choices = list("No error" = 0, "Error 1" = 1, "Error 2" = 2, "Error 3" = 3), 
                    #             selected = 0),
                    br())
                  ), 
                  
                  fluidRow(
                    box(width=12, title="Validación del resumen",status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      #p("Una vez leído el artículo y el resumen, indique si las siguientes afirmaciones son Verdaderas o Falsas"),
                      br(),
                      radioButtons("question1", label = ("El resumen trasmite el contenido del texto."),
                                 choices = list("Verdadero" = 1, "Falso" = 2), 
                                 selected = 2),
                      conditionalPanel(
                        condition = "input.question1 == 1",
                        radioButtons("question2", label = ("El resumen contiene informatión que no es coherente con el artículo."),
                                     choices = list("Verdadero" = 1, "Falso" = 2),  
                                     selected = 2),
                        # conditionalPanel(
                        #condition = "input.question2 == 1",
                        radioButtons("question3", label = ("El resumen contiene información que no puede deducirse del artículo."),
                                     choices = list("Verdadero" = 1, "Falso" = 2), 
                                     selected = 2)
                        #)
                      ),
                      actionButton('validateButton',"Validar Resumen"))
                  )
          )          
        }
        else{
          tabItem(tabName ="dashboard", class = "active",
                  fluidRow(
                    box(width = 12, dataTableOutput('results'))
                  ))          
        }
    )
      
  }
  else {
    loginpage
  }
  })  

  output$results <-  DT::renderDataTable({
    datatable(text, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })


  #Filter the text from the title input to show the expert body and summary
  filteredText<- reactive ({
    text %>% select(title,body,summary) %>% filter(title %in% input$selectTitle) %>% select(body,summary)
    
  })
  
  output$contents <- renderTable(
    filteredText()
  )
  
  observeEvent(input$question1,{
    
  })
  
}



runApp(list(ui = ui, server = server), launch.browser = TRUE)