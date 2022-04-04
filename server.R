library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinyjs)
library(sodium) #encrypt passwords
library(tidyverse)
library(dplyr)
library(tidyr)
library(reactlog)
library(shinyalert)
library(DT)
library(krippendorffsalpha)

reactiveConsole(TRUE)


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
    tags$li(a(icon("fa-sign-out",lib = "font-awesome"), "Logout", 
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

  # Reactive Value to store the position of all validated titles perform by the user (previous and new)
  positions <- data.frame(matrix(ncol=1,nrow=0))
  colnames(positions) <- c("position")
  VALIDATIONS <- reactiveValues(positions = positions) #VALIDATIONS$positions
  
  # Reactive Value to store the titles that needs validation from the user
  userTitles <- data.frame(matrix(ncol=1,nrow=0))
  colnames(userTitles) <- c("title")
  TITLES <- reactiveValues(userTitles = userTitles) #TITLES$userTitles
  
  #I read it each time a user logs so the articles he read are updated
  #if not it he disconnects and log on again he would see all the 
  #articles he validated in its previous session
  #TODO: chequear que el fichero existe
  #TODO: condiciones de carrera lectura en simultaneo ¿? existe o solo escritura
  expertsValidations <- read.csv(file="data/expertsValidations.csv",header=TRUE)
  
  #Generate list of titles to validate if user is expert
  observeEvent(input$login,{
    req(USER$login)
    if (USER$login == TRUE) {
      if(typeUser() == "expert"){
        #filter positions of validated articles by logged user 
        validatedTitles <- expertsValidations %>% filter(username_id == input$userName) %>% select(position)
        VALIDATIONS$positions <- as.data.frame(validatedTitles)  
        if (length(VALIDATIONS$positions > 0)){
          TITLES$userTitles <- articles[-VALIDATIONS$positions$position,3]
        }
        else{
          TITLES$userTitles <- articles$title     
        }
      } #if type user
    }#if login true
    #print(head(VALIDATIONS$positions))
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
                      infoBoxOutput("agreementBox"),
                      infoBoxOutput("validatedBox"),
                      infoBoxOutput("pendingBox")
                    ),
                    fluidRow(
                      box(width = 12, dataTableOutput('results'))
                    )#fluidRow
            )#tabItem dashboard   
          )#tabItems
        } #fin if cuando el usuario es el administrador
  }
  else {
    loginpage
  }
  })  
  
  
  ######
  #User Expert 
  
  selectedArticleData <- reactive ({
    articles %>% select(title,summary,url) %>% filter(title %in% input$selectTitle)
  })
  
  filteredGeneratedSummary<- reactive ({
    summaries[[1]][which(articles$title == input$selectTitle)]
  })
  
  output$articleURL <- renderUI({
    # tags$a(
    #   href = selectedArticleData()$url,
    #   "Haga click aquí para leer el artículo",
    #    target="_blank")
    HTML(paste0("<p><b>Para leer el artículo visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))
  })
  
  output$objectiveSummary <- renderText(selectedArticleData()$summary)
  
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
    numCurrentValidations <<- numCurrentValidations + 1
    
    #update select input
    position <- validation$position
    VALIDATIONS$positions <- append(VALIDATIONS$positions$position,position)
    VALIDATIONS$positions <- as.data.frame(VALIDATIONS$positions)
    colnames(VALIDATIONS$positions) <- c("position")
    TITLES$userTitles <- articles[-VALIDATIONS$positions$position,3] # ver de optimizar
    updateSelectInput(session,"selectTitle",choices=TITLES$userTitles)
    
    #compute agreeement --> no, se computa a nivel global, no individual
    
  })
  
  ######
  #User Admin
  
  validationsbyCoder <- expertsValidations %>% select(position,error,username_id) %>%  spread(username_id, error)
  tableAdmin <- left_join(adminArticles,validationsbyCoder,by=c("row_num"="position"))  
  set.seed(123)
  # krippTable <- data.matrix(tableAdmin[,-c(1:4)]) #all articles
  # colnames(krippTable) <- NULL
  # krippAgreement <- krippendorffs.alpha(krippTable, level = "nominal", control = list(bootit = 100, parallel = FALSE),verbose = TRUE) #validationbyCoder
  # print(krippAgreement$alpha.hat)
  # summary(krippAgreement)
  
  validationsbyCoderKripp <- data.matrix(validationsbyCoder[,-c(1)])
  colnames(validationsbyCoderKripp) <- NULL
  # validationsbyCoderKripp <- cbind(validationsbyCoderKripp,validationsbyCoderKripp[,1])
  krippAgreementValidated <- krippendorffs.alpha(validationsbyCoderKripp, level = "nominal", control = list(bootit = 100, parallel = FALSE),verbose = TRUE) #validationbyCoder
  print(krippAgreementValidated$alpha.hat)
  #summary(krippAgreementValidated)
  
  #sum(is.na(x))
  
  output$results <-  DT::renderDataTable({
    #no sé si leer, por si se conecta otro usuario......
    # validationsbyCoder <- expertsValidations %>% select(position,error,username_id) %>% arrange(position)
    # spreadTest <- validationsbyCoder %>%  spread(username_id, error)
    # validationsbyCoder <- expertsValidations %>% select(position,error,username_id) %>%  spread(username_id, error)
    # tableAdmin <- left_join(adminArticles,validationsbyCoder,by=c("row_num"="position"))
    #nrow(tableAdmin) #ok, todas las filas
    datatable(tableAdmin[-c(1,2),], options = list(autoWidth = TRUE,
                                            searching = FALSE))
  })
  
  output$agreementBox <- renderInfoBox({
    infoBox(
      "Krippendorff's Alpha", 
      round(krippAgreementValidated$alpha.hat,2), 
      icon = icon("fal fa-handshake"),
      color = "purple"
    )
  })
  
  output$validatedBox <- renderInfoBox({
    infoBox(
      "Validated Articles", 
      nrow(validationsbyCoder), 
      icon = icon("fal fa-check"),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  output$pendingBox <- renderInfoBox({
    infoBox(
      "Pending Articles", 
      nrow(articles) - nrow(validationsbyCoder), 
      icon = icon("fal fa-edit"), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  }
