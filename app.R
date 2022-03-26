library(shiny)
library(shinyauthr)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium) #para encriptar contraseñas
library(tidyverse)
library(dplyr)
#library(digest)
library(readr) #read txt more efficient

#source("~/shinyapp/summariesApp/aux.R")

setwd("~/shinyApp/summariesApp/")
typeErrors <- list( "No hay errores" = 1,"Entiende mal todo o cierta parte del texto" = 2,
                    "Agrega información que no está en el texto" = 3,
                    "La sintaxis de la oración introduce errores semánticos" = 4, "Otro" = 5)
credentials <- read.csv(file="data/users.csv")
articles <- (read.csv("data/articles.csv")) #test dataset XL-Sum
#names(articles)[2] <- "idArticle"
#summaries <- read.delim("data/resumenesTEST.txt",header=FALSE) #read.csv("data/summaries2.csv")
summaries <- as.data.frame(read_lines("data/resumenesTEST.txt"))
typeof(summaries)
names(summaries)[1] <- "summary"

# queria chequear si tenian la misma longitud para asingarlo uno a uno
# nArticles <- count(articles) 
# nSummaries <- count(summaries)
# print(nSummaries == nArticles)
# asumo que los primeros nSummaries se corresponder articulos:
# idArticles <- articles  %>% slice(1:nrow(summaries)) %>%  select(id)
# head(idArticles)
# names(idArticles) <- "idArticle" #cambiar nombre de la columna
# summaries <-cbind(summaries,idArticles)
# summaries <- summaries[,c(ncol(summaries),1:(ncol(summaries)-1))] #dejar idArticle como primera columna del dataset
articlesTitles <- articles  %>% slice(1:nrow(summaries)) %>%  select(title)
summaries <-cbind(summaries,articlesTitles)
summaries <- summaries[,c(ncol(summaries),1:(ncol(summaries)-1))] #dejar idArticle como primera columna del dataset


#hacer FUNCION QUE CHEQUEE QUE existe fichero de validación y si existe haga lo siguiente:
expertsValidations <- read.csv(file="data/expertsValidations.csv")
validation <- data.frame(matrix(ncol=4,nrow=1))
colnames(validation) <- colnames(expertsValidations)

#Titulos de textos que hay que validar porque tienen menos de 3 validacions --> 
# esto tiene que cambiar porque 3 es variable parametrica y sino hay acuerdo necesitamos más checks de expertos
# numChoicesSummVal <- text %>% filter(numValid < 3) %>%  count()
# choicesSummVal <- text %>% filter(numValid < 3)  %>% select(title) 
adminArticles <- articles %>% select(id, title) #habrá que añadir lo que salga de la validación con el nivel de acuerdo etc

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

  ##### LOGIN
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

  #updateSelectizeInput(session, 'selectTitle', choices = articlesTitles$title, server = TRUE)
  #PRUEBA AGREGAR ACÁ TITULOS PENDIENTES DE VALIDAR AL USUARIO
  
  # pendingTitles <- reactive({
  #   req(USER$login)
  #   Username <- isolate(input$userName)
  #   validatedSummaries <- expertsValidations %>% filter(username_id == input$userName) %>% select(title)
  #   pending <- setdiff(articlesTitles,validatedSummaries)
  #   
  # })
  
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
                      width=12, title="Título", status = "primary", #solidHeader = TRUE, collapsible = FALSE,
                      # selectInput("selectTitle",
                      #               label=("Seleccione el artículo a validar"),
                      #               choices = articlesTitles)) # *********************************
                      selectizeInput("selectTitle", label=("Seleccione el artículo a validar"),choices = articlesTitles)) #
                  ),
                  
                  # fluidRow(
                  #   box(
                  #     width=12, title="Texto del artículo",status = "primary", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                  #     uiOutput('articleURL'),
                  #     br(),
                  #     textOutput('text'))
                  # ), 
                  
                  fluidRow(
                    tabBox(
                      # Title can include an icon
                      title = "Texto del artículo", width =12, side="right",
                      tabPanel("Iframe",
                               htmlOutput('textFrame')
                               ),
                      tabPanel("Texto plano", 
                               textOutput('text')
                      ),
                      tabPanel("Sitio web", 
                               uiOutput('articleURL')
                               )
                    )
                  ),
                  
                  fluidRow(
                    box(
                      width=6, title="Resumen objetivo",status = "primary", 
                      textOutput('objectiveSummary')),
                    box(
                      width=6, title="Resumen generado automáticamente",status = "danger",  
                      textOutput('generatedSummary'))
                  ), 
                  
                  fluidRow(
                    box(
                      width=12, title="Validación del resumen", status = "primary", #solidHeader = TRUE, collapsible = FALSE,
                      #h4("Una vez leído el artículo, el resumen objetivo y el resumen generado automáticamente, por favor indique si las siguientes afirmaciones son verdaderas o falsas."),
                      #br(),
                      radioButtons("question1", label = ("El resumen trasmite la idea general del texto y es comparable al resumen realizado por un humano."),
                                 choices = list("Verdadero" = 1, "Falso" = 2), 
                                 selected = 1),
            
                      selectInput("selectError", label = ("Seleccione si existe el tipo de error:"), 
                                  choices = typeErrors, 
                                  selected = 1),
                      conditionalPanel(
                        condition = "input.selectError == 5",
                        textInput("errorDescription", label = ("Descripción del error que contiene el resumen"), value = "Explique el error...")
                      ),
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
  
  output$results <-  DT::renderDataTable({
    datatable(adminArticles, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })


  filteredText<- reactive ({
    unlist(articles %>% select(title,text) %>% filter(title %in% input$selectTitle) %>% select(text))
  })
  
  filteredObjectiveSummary<- reactive ({
    unlist(articles %>% select(title,summary) %>% filter(title %in% input$selectTitle) %>% select(summary))
  })

  filteredIdArticle <- reactive({
    #articles %>% select(title,id) %>% filter(title %in% input$selectTitle) %>% select(id)
    articles[articles$title == input$selectTitle,1]
  })

  filteredGeneratedSummary<- reactive ({
    #summaries %>% select(idArticle,summary) %>% filter(idArticle == filteredIdArticle) %>% select(summary)
    #summaries[summaries$idArticle == filteredIdArticle,2] funciona en la consola pero no acá
    unlist(summaries %>% filter(title %in% input$selectTitle) %>% select(summary))
  })
  

    selectedArticleData <- reactive ({
      articles %>% select(id,title,summary,url) %>% filter(title %in% input$selectTitle)
    })
  
  output$text <- renderText(filteredText())
  
  output$textFrame <- renderUI({
    test <- tags$iframe(src = selectedArticleData()$url, width = '100%')
    print(test)
    test
  })
  
  output$objectiveSummary <- renderText(filteredObjectiveSummary())
  
  output$generatedSummary <- renderText(filteredGeneratedSummary())
  
  output$articleURL <- renderUI(HTML(paste0("<p><b>Para leerlo en la página del site visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))) 

  observeEvent(input$validateButton,{
    # req(USER$login)
    # Username <- isolate(input$userName)
    validation$username_id <- isolate(input$userName)
    validation$title <- isolate(input$selectTitle)
    validation$summaryOK <- isolate(input$question1)
    if(input$selectError == 5){
      validation$error <- isolate(input$errorDescription)
    }
    else{
      validation$error <- isolate(input$selectError)
    }
    write.table(validation,file="data/expertsValidations.csv",append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    
  })
  
  }



runApp(list(ui = ui, server = server), launch.browser = TRUE)