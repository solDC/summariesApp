library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinyjs)
library(sodium) #encrypt passwords
library(tidyverse)
library(dplyr)
library(readr) #read txt more efficient
library(reactlog)
library(shinyalert)

reactiveConsole(TRUE)

#source("~/shinyapp/summariesApp/aux.R")

setwd("~/shinyApp/summariesApp/")

typeErrors <- list( "No contiene errores" = 1,
                    "Contiene información inconsistente con el artículo" = 2,
                    "Contiene alguna información que no puede ser inferida del artículo" = 3,
                    "No transmite el objetivo principal del texto"= 4)

#Load files --> only one time when app loads
#TODO: para todos los ficheros: función que chequee si existe el fichero o mensaje de error en caso contrario

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


############ 
#UI
header <- dashboardHeader( title = "Validación de resúmenes", titleWidth = 250, uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(width = 250,uiOutput("sidebarpanel")) 

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui<-dashboardPage(header, sidebar, body, skin = "blue")


############
#Server
server <- function(input, output, session) {

  #LOGIN
  ######
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
            } 
            else {
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
  

  ######
  # Type of user ----QUITE ISOLATE
  typeUser <- reactive ({
    req(USER$login)
    Username <- input$userName  #isolate(input$userName)
    credentials["permission"][which(credentials$username_id==Username),]
  })

  ######
  # Logout button Render UI
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("far fa-sign-out",lib = "font-awesome"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })

  ######
  # Sidebar panel Render UI
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE){
      if(typeUser() == "expert"){ 
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

  ######
  # Generate list of articles pending validation ---me falta agregarle condicion de ver los que no tienen acuerdo

  # Reactive Values to store all validations perform by the user (previous and new)
  positions <- data.frame(matrix(ncol=1,nrow=0))
  colnames(positions) <- c("position")
  VALIDATIONS <- reactiveValues(positions = positions) #VALIDATIONS$positions
  
  userTitles <- data.frame(matrix(ncol=1,nrow=0))
  colnames(userTitles) <- c("title")
  TITLES <- reactiveValues(userTitles = userTitles) #TITLES$userTitles
  
  expertsValidations <- read.csv(file="data/expertsValidations.csv",header=TRUE)
  
  observeEvent(input$login,{
    req(USER$login)
    if (USER$login == TRUE) {
      #filter positions of validated articles by logged user 
      validatedTitles <- expertsValidations %>% filter(username_id == input$userName) %>% select(position)
      VALIDATIONS$positions <- as.data.frame(validatedTitles)  
      if (length(VALIDATIONS$positions > 0)){
        TITLES$userTitles <- articles[-VALIDATIONS$positions$position,3]
      }
      else{
        TITLES$userTitles <- articles$title     
      }
    }
    print(head(VALIDATIONS$positions))
  })


  #No me funciona:
  #userTitles <- articles  %>% slice(1:nrow(summaries)) %>%  filter(username = Username()) %>%select(title)
  #updateSelectizeInput(session,'selectTitle',choices = userTitles(), server=TRUE)
  
  
  ######
  # Body Render UI depending on logged user
  output$body <- renderUI({
    if (USER$login == TRUE) {
      if(typeUser() == "expert"){
        tabItems(
          tabItem(tabName = "information", 
                    h4("Acá voy a poner un texto donde se explica el objetivo de la herramienta, el dataset utilizado y el problema con el resumen objectivo del dataset que 
                         a veces viene con datos que no se pueden inferir del texto y no sé si hace falta algo más.")),
          tabItem(tabName = "validate", class = "active",
                  fluidRow(
                    box(
                      width=12, title="Título", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      selectInput("selectTitle",
                                    label=("Seleccione el artículo a validar"),
                                    choices = TITLES$userTitles)) # *********************************
                      #selectizeInput('selectTitle', label=("Seleccione el artículo a validar"),choices = articlesTitles )) #
                  ),
                  fluidRow(
                    box(
                      width=12, title="Texto", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      uiOutput('articleURL'))
                  ),
                  fluidRow(
                    box(
                      width=6, title="Resumen objetivo",status = "primary", solidHeader = TRUE,
                      textOutput('objectiveSummary')),
                    box(
                      width=6, title="Resumen generado",status = "danger", solidHeader = TRUE,  
                      textOutput('generatedSummary'))
                  ), 
                  fluidRow(
                    box(
                      width=12, title="Validación del resumen generado", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      h5("Una vez leído el artículo, el resumen objetivo y el resumen generado automáticamente, por favor conteste 
                       las siguientes preguntas:"),
                      br(),
                      radioButtons("question1", label = ("El resumen generado: ¿trasmite la idea general del artículo 
                                                         y es comparable al resumen objetivo?"),
                                 choices = list("Verdadero" = 1, "Falso" = 2)), 
                                 #selected = character(0)),
                      selectInput("selectError", label = ("Seleccione, si existe, el tipo de error en el resumen generado:"), 
                                  choices = typeErrors, 
                                  selected = character(0)),
                      actionButton('validateButton',"Validar Resumen",class="btn-primary"),
                      br())
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
        } #fin if cuando el usuario es el administrador
  }
  else {
    loginpage
  }
  })  
  
  ######
  #User Admin
  output$results <-  DT::renderDataTable({
    datatable(adminArticles, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })

  ######
  #User Expert 

  filteredText<- reactive ({
    unlist(articles %>% select(title,text) %>% filter(title %in% input$selectTitle) %>% select(text))
  })
  
  filteredObjectiveSummary<- reactive ({
    unlist(articles %>% select(title,summary) %>% filter(title %in% input$selectTitle) %>% select(summary))
  })

  filteredGeneratedSummary<- reactive ({
    summaries[[1]][which(articles$title == input$selectTitle)]
  })
  
    selectedArticleData <- reactive ({
      articles %>% select(id,title,summary,url) %>% filter(title %in% input$selectTitle)
    })
  
  output$text <- renderText(filteredText())
  
  output$articleURL <- renderUI(HTML(paste0("<p><b>Para leer el artículo visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))) 
  
  output$objectiveSummary <- renderText(filteredObjectiveSummary())
  
  output$generatedSummary <- renderText(filteredGeneratedSummary())
  
  observeEvent(input$validateButton,{
    #save validation
    validation <- data.frame(matrix(ncol=5,nrow=1)) #esto vale para validar un resumen
    colnames(validation) <- colnames(expertsValidations) #esto vale para guardar info de validación de un resumen
    validation$username_id <- input$userName
    validation$title <- input$selectTitle
    validation$position <- which(articles$title == input$selectTitle) 
    validation$summaryOK <- input$question1
    validation$error <- input$selectError
    print(validation)
    write.table(validation,file="data/expertsValidations.csv",append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    shinyalert(title="Validation stored",type="success") 
    
    #update select input
    position <- validation$position
    VALIDATIONS$positions <- append(VALIDATIONS$positions$position,position)
    VALIDATIONS$positions <- as.data.frame(VALIDATIONS$positions)
    colnames(VALIDATIONS$positions) <- c("position")
    TITLES$userTitles <- articles[-VALIDATIONS$positions$position,3] # ver de optimizar
    updateSelectInput(session,"selectTitle",choices=TITLES$userTitles)
    
    #compute agreeement
    
  })
  
  }

runApp(list(ui = ui, server = server), launch.browser = TRUE) #