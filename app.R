library(shiny)
library(shinyauthr)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium) #para encriptar contraseñas
library(tidyverse)
library(dplyr)
#library(digest)

#source("~/shinyapp/summariesApp/aux.R")

# #global variables
# fields <- c("user","title","error")
# responsesDir <- file.path("~/shinyapp/summariesApp/data")
# epochTime <- function() {
#   as.integer(Sys.time())
# }

setwd("~/shinyApp/summariesApp/")
credentials <- read.csv(file="data/users.csv")

articles <- read.csv("data/articles.csv") #test dataset XL-Sum
summaries <- read.csv("data/summaries2.csv")


#Titulos de textos que hay que validar porque tienen menos de 3 validacions --> 
# esto tiene que cambiar porque 3 es variable parametrica y sino hay acuerdo necesitamos más checks de expertos
# numChoicesSummVal <- text %>% filter(numValid < 3) %>%  count()
# choicesSummVal <- text %>% filter(numValid < 3)  %>% select(title) 


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
      #comparo contra admin en caso de crear el super-expert
      if(typeUser() != "admin"){ 
        sidebarMenu(
          menuItem("Information", tabName = "information", icon = icon("fas fa-info")),
          menuItem("Validar resumen", tabName = "validate", icon = icon("th",lib = "font-awesome"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt",lib = "font-awesome"))
        )
      }
    }
  })

  output$body <- renderUI({
    if (USER$login == TRUE) {
      if(typeUser() != "admin"){
        
        tabItems(
          
          #comparaba contra expert pero tal vez cree usuario de tipo super-expert para cuando no hay acuerdo entre experts normales
          tabItem(tabName = "information", 
                    h4("Acá voy a poner un texto donde se explica el objetivo de la herramienta, el dataset utilizado y el problema con el resumen objectivo del dataset que 
                         a veces viene con datos que no se pueden inferir del texto y no sé si hace falta algo más.")),
          
          tabItem(tabName = "validate", class = "active",
                  fluidRow(
                    box(
                      width=12, title="Título", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      selectInput("selectTitle",
                                    label=("Seleccione el artículo a validar"),
                                    choices = articles$title)) #choices = choicesSummVal
                  ),
                  
                  fluidRow(
                    box(
                      width=12, title="Texto del artículo",status = "primary", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                      uiOutput('articleURL'),
                      br(),
                      textOutput('text'))
                  ), 
                  
                  fluidRow(
                    box(
                      width=6, title="Resumen objetivo",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                      textOutput('objectiveSummary')),
                    box(
                      width=6, title="Resumen generado automáticamente",status = "danger", solidHeader = TRUE, collapsible = TRUE,
                      textOutput('generatedSummary'))
                  ), 
                  
                  fluidRow(
                    box(
                      width=12, title="Validación del resumen", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      h4("Una vez leído el artículo, el resumen objetivo y el resumen generado automáticamente, por favor indique si las siguientes afirmaciones son verdaderas o falsas."),
                      br(),
                      radioButtons("question1", label = ("El resumen trasmite la idea general del texto y es comparable al resumen realizado por un humano."),
                                 choices = list("Verdadero" = 1, "Falso" = 2), 
                                 selected = 2),
                        #condition = "input.question1 == 1",
                        radioButtons("question2", label = ("El resumen contiene algún error."),
                                     choices = list("Verdadero" = 1, "Falso" = 2),  
                                     selected = 2),
                        conditionalPanel(
                        condition = "input.question2 == 1",
                        selectInput("selectError", label = ("Select type of error"), 
                                    choices = list( "Entiende mal todo el texto " = 1, "Entiende mal cierta parte del texto " = 2, 
                                                    "Agrega información que no está en el texto" = 3, 
                                                    "La sintaxis de la oración introduce errores semánticos" = 4, "Otro" = 5), 
                                    selected = 1),
                        conditionalPanel(
                          condition = "input.selectError == 5",
                          textInput("text", label = ("Descripción del error que contiene el resumen"), value = "Explique el error...")
                        )
                      ),
                      actionButton('validateButton',"Validar Resumen"))
                  ) #última fluidRow
           ) #tabItem validate  
        )# tabItems
        }#fin if si el usuario es el expert
        else{
          tabItems(
            tabItem(tabName ="dashboard", class = "active",
                    fluidRow(
                      box(width = 12, dataTableOutput('results'))
                    ))   
          )
        } #fin if si el usuario es el administrador
  }
  else {
    loginpage
  }
  })  

  output$results <-  DT::renderDataTable({
    datatable(text, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })

  selectedArticleData <- reactive ({
    articles %>% select(id,title,summary,url) %>% filter(title %in% input$selectTitle)
  })

  filteredText<- reactive ({
    unlist(articles %>% select(title,text) %>% filter(title %in% input$selectTitle) %>% select(text))
  })
  
  filteredObjectiveSummary<- reactive ({
    unlist(articles %>% select(title,summary) %>% filter(title %in% input$selectTitle) %>% select(summary))
  })

  filteredGeneratedSummary<- reactive ({
    unlist(summaries %>% select(title,summary) %>% filter(title %in% input$selectTitle) %>% select(summary))
  })
  
  output$text <- renderText(filteredText())
  output$objectiveSummary <- renderText(filteredObjectiveSummary())
  output$generatedSummary <- renderText(filteredGeneratedSummary())
  output$articleURL <- renderUI(HTML(paste0("<p><b>Para una mejor experiencia de lectura, visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))) 
}



runApp(list(ui = ui, server = server), launch.browser = TRUE)