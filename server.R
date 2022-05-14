library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinyjs)
library(sodium) #encrypt passwords
library(tidyverse)
library(dplyr)
library(tidyr)
library(shinyalert)
library(DT)
library(krippendorffsalpha)
library(RColorBrewer)
library(ggplot2)
#library(rdrop2)


calcPairs <- function(x) {
  a <- factorial(x) / (2 * factorial(x - 2))
  if(is.nan(a))
    0
  else
    a
}


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

#####

server <- function(input, output, session) {
  
  # Calculate individual level of agreement for question 1, it will be used to validate only articles-summaries with no agreement
  #####
  AGREEM <- reactiveValues(n = 0)
  VALIDATIONS <- reactiveValues(table= data.frame(matrix(ncol=0,nrow=0)))
  
  observe({
    if(dim(expertsValidations)[1] > 0){
      expertsValidations$questions <- with(expertsValidations,question1*100+question2*10+question3)
      validations <- expertsValidations %>%
        filter(summariesNameFile==conf$fileSummaries && articlesNameFile==conf$fileArticles) %>%
        select(position,questions,usernameId) %>%
        distinct(position,usernameId, .keep_all = TRUE) %>%
        spread(usernameId,questions)
      numNA <- apply(X=is.na(validations), MARGIN=1,FUN=sum)
      numUsers <- ncol(validations)-1
      validations$numResp <- numUsers-numNA
      df <- validations[2:(numUsers+1)]
      comb <- combn(df,2,simplify = FALSE)
      if (length(validations$numResp) > 0 ){
        validations$possPairs <- sapply(validations$numResp,calcPairs)
        n <- length(validations)+1
        for(val in comb){
          validations[n] <- (val[1]==val[2])
          n <- n+1
        }
        validations$agreemCount <- rowSums(validations[,(n-length(comb)):(n-1)],na.rm = TRUE)
        validations$agreemPerc <- with(validations,
                                       ifelse(validations$possPairs>=conf$minNumValid,
                                              round(validations$agreemCount / validations$possPairs * 100,2),
                                              0))
        validations$agreedAnswer <- strtoi(apply(df,1,function(x) names(which.max(table(x)))))
        i=1
        for(val in validations$agreemPerc){
          if ((val < conf$minLevelAgreem))
            validations$agreedAnswer[i] <- 0
          i <- i+1
        }
        if(sum(validations$agreedAnswer)>0){
          AGREEM$n <- 1
        }
      }
      VALIDATIONS$table <- validations
    } #outter if
})
  
  #LOGIN: SHOW APPROPIATE INTERFACE
  ######
  login <- FALSE
  USER <- reactiveValues(login = login)
  name <- NULL
  USERNAME <- reactiveValues(name = name) #USERNAME$name

  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(!is.null(credentials)){
            if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              Password <- NULL # to avoid keeping the password available
              if(pasverify) {
                USER$login <- TRUE
                USERNAME$name <- Username ##new
                #Password <- NULL # to avoid keeping the password available ---> lo moví arriba
              }
              else {
                shinyjs::toggle(id = "no password match", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            }
            else {
              shinyjs::toggle(id = "no username match", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
          else{
            shinyalert(title="Salga de la aplicación",
                       text="No es posible realizar la autenticación
                       (no se ha cargado el fichero credentials).",
                       type="error")
          } # end else if(!is.null(credentials))
        } # end if(input$login > 0)
      } # end if(!is.null(input$login))
    } # end if(USER$login == FALSE)
  }) # end observe

  ######
  # Logout button Render UI
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fas fa-sign-out"), "Logout",
              href="javascript:window.location.reload(true)"),
            class = "dropdown",
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })

  ######
  # Type of user
  typeUser <- reactive ({
    req(USER$login)
    # Don't need to check if credential is loaded, can't reach this point if it wasn't loaded.
    credentials["permission"][which(credentials$username_id==USERNAME$name),]
  })

  ######
  # Sidebar panel Render UI
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE){
      if(typeUser() == "expert"){
        sidebarMenu(
          id="tabsExpert",
          menuItem("Informacion", tabName = "information", icon = icon("fas fa-info")),
          menuItem("Validar resumen", tabName = "validate", icon = icon("th",lib = "font-awesome"))
        )
      }
      else{
        sidebarMenu(
          id="tabsAdmin",
          menuItem('Configurar "Validar Resúmenes"', tabName = "manageEvalSummaries", icon = icon("fal fa-cog")),
          menuItem('Dashboard "Validar Resúmenes"', tabName = "dashboadEvalSummaries", icon = icon("tachometer-alt",lib = "font-awesome")),
          menuItem('Gestionar Usuarios', tabName = "users", icon = icon("fal fa-user")),
          menuItem('Guardar workspace', tabName = "manageData", icon = icon("fal fa-database"))
        )
      }
    }
  })

  ######
  # LOAD EXPERTS VALIDATIONS (file with responses) AND GENERATE TITLES TO VALIDATE
  #Reactive values
  ######
  # Reactive Value to store what should be the name of the validations file depending on user and if the file was loaded (control variable)
  #VALID_LOADED <- reactiveValues(loaded = FALSE) #VALID_LOADED$loaded
  FILENAMEEV <- reactiveValues(name = "") #FILENAMEEV$name

  # Reactive Value to store the user's previous validations stored in the file
  df <- data.frame(matrix(ncol=numColEV,nrow=0))
  colnames(df) <- expertsValidationsColNames
  EXPERT_VALIDATION <- reactiveValues(df = df) #EXPERT_VALIDATION$df

  # Reactive Value to store the position of all validated titles perform by the user (previous and new)
  positions <- data.frame(matrix(ncol=1,nrow=0))
  colnames(positions) <- c("position")
  VALIDATIONS <- reactiveValues(positions = positions) #VALIDATIONS$positions

  # Reactive Value to store the titles that need validation from the user
  userTitles <- data.frame(matrix(ncol=1,nrow=0))
  colnames(userTitles) <- c("title")
  TITLES <- reactiveValues(userTitles = userTitles) #TITLES$userTitles

  # Reactive Value to store the articles which summaries the user needs to validate
  # dfA <- data.frame(matrix(ncol=length(articles),nrow=0))
  # colnames(dfA) <-colnames(articles)
  ARTICLES <- reactiveValues(df = articles) #ARTICLES$df
  
  ADMIN_ARTICLES <- reactiveValues(df = data.frame(matrix(ncol=0,nrow=0))) #ADMIN_ARTICLES$df

  ######
  observeEvent(input$login,{
    if (USER$login == TRUE) {
      if(typeUser() == "expert"){
        # FILENAMEEV$name <- paste0("validations-",USERNAME$name,".csv")
        # x <- drop_search(FILENAMEEV$name)
        # if(x$start == 0){
        #   msg <- paste0("El fichero ",FILENAMEEV$name, " con las validaciones de los usuarios no existe y se va a crear uno vacío.")
        #   message(msg)
        #   expertValid <- data.frame(matrix(ncol=numColEV,nrow=0))
        #   colnames(expertValid) <- expertsValidationsColNames
        #   filePath <- file.path(tempdir(),FILENAMEEV$name)
        #   write.table(expertValid,file=filePath,sep=',',row.names = FALSE)
        #   drop_upload(filePath,outputDir)
        #   message(paste0("Creado fichero",FILENAMEEV$name))
        # }
        #EXPERT_VALIDATION$df <- loadCSV(outputDir,FILENAMEEV$name)
        #filter positions of validated articles by logged user
        validatedTitlesPos <- EXPERT_VALIDATION$df %>% 
          filter(summariesNameFile==conf$fileSummaries && articlesNameFile==conf$fileArticles) %>%
          select(position) #filter(usernameId == input$userName) %>% select(position)
        VALIDATIONS$positions <- as.data.frame(validatedTitlesPos)
        # If there's agreement in certain summaries-articles, there's no need of validating them any more
        if(AGREEM$n==1){
          posDiscard <- VALIDATIONS$table %>% filter(agreemPerc > conf$minLevelAgreem) %>%  select(position)
          #posDiscard <- AGREEM$table() %>% filter(agreemPerc > conf$minLevelAgreem) %>%  select(position)
          print(posDiscard$position)
          ARTICLES$df <- subset(articles,!(position %in% posDiscard$position))
        }
        # Randomize titles to validate so different users validate articles in different order
        if (length(VALIDATIONS$positions) >= 1){
          TITLES$userTitles <- sample(ARTICLES$df[!(row.names(ARTICLES$df) %in% VALIDATIONS$positions$position),3])
        }
        else{
          TITLES$userTitles <- sample(ARTICLES$df$title)
        }
      }
      else{#user admin
        if(is.null(articles)){
          message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
        } 
        else{
          if(is.null(summaries)){
            message("El fichero con los resúmenes no se ha cargado.")
          }
          else{
            print("crea admin articles")
            adminArticles <- cbind(articles,summaries$summary)
            names(adminArticles)[length(adminArticles)] <- "generatedSummary" 
            #agregar nivel de cuerdo
            if(AGREEM$n == 1){
              dfa <- validations %>%  select(position,agreemPerc,agreedAnswer)
              #dfa <- AGREEM$table() %>%  select(position,agreemPerc,agreedAnswer)
              #adminArticles <- adminArticles %>% left_join(dfa,by="position")
              ADMIN_ARTICLES$df <- adminArticles %>% left_join(dfa,by="position")
            }
          }
        }
      } #end if user admin
    }#if login true
  })

  # Body Render UI depending on logged user
  ######
  output$body <- renderUI({
    if (USER$login == TRUE) {
      req(conf)
      if(typeUser() == "expert"){
        tabItems(
          tabItem(tabName = "information",
                    h4("Acá voy a poner un texto donde se explica el objetivo de la herramienta, el dataset utilizado y el problema con el resumen objectivo del dataset que
                         a veces viene con datos que no se pueden inferir del texto y no sé si hace falta algo más.")
                  ),
          tabItem(tabName = "validate", class = "active",
                  fluidRow(
                    infoBoxOutput("expertGreetingBox"),
                    infoBoxOutput("expertValidatedBox"),
                    infoBoxOutput("expertPendingBox")
                  ),
                  fluidRow(
                    box(
                      width=12, title="1- TITULO: seleccione el artículo a validar", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      selectInput("selectTitle",
                                    label=("Seleccione el artículo a validar"),
                                    choices = TITLES$userTitles))
                  ),
                  fluidRow(
                    box(
                      width=12, title="2- TEXTO: lea la noticia para poder validar el resumen",
                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                      uiOutput('articleURL'),
                      br(),
                      textOutput('text'))
                  ),
                  fluidRow(
                    box(
                      width=12, title="3- RESUMEN: lea el resumen generado por el modelo",status = "primary", solidHeader = TRUE,
                      textOutput('generatedSummary'))
                  ),
                  fluidRow(
                    box(
                      width=12, title="4- VALIDACIÓN: conteste las preguntas sobre el resumen",
                      status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      radioButtons("question1", label = ("El resumen generado: ¿trasmite la idea general del artículo?"),
                                 choices = list("Verdadero" = 1, "Falso" = 2),
                                 selected = 2), #character(0)),
                      conditionalPanel(
                        condition = "input.question1 == 1",
                        radioButtons("question2", label = ("2- El resumen, ¿contiene información inconsistente con el artículo?"),
                                     choices = list("Verdadero" = 1, "Falso" = 2),
                                     selected = 2), #character(0)),
                        radioButtons("question3", label = ("3- El resumen, ¿contiene alguna información que no puede ser inferida del artículo?"),
                                     choices = list("Verdadero" = 1, "Falso" = 2),
                                     selected = 2) #character(0))
                      ), #conditional panel
                      actionButton('validateButton',"Validar Resumen",class="btn-primary"),
                      br())
                  ) #última fluidRow
           ) #tabItem validate
        )# tabItems
        }#fin if si el usuario es el expert
        else{
          #Usuario admin
          tabItems(
            tabItem(tabName = "manageEvalSummaries", class = "active",
                    fluidRow(
                      box(title="Tamaño de la muestra a validar", width = 12, solidHeader = TRUE,status = "primary",
                        box(width=6,
                          strong("Número total de artículos-resúmenes: "), verbatimTextOutput("numberRowsArticles"),
                          strong("Tamaño actual de la muestra (en %):"), verbatimTextOutput("currentSampleSize"),
                          strong("Tamaño actual de la muestra (nº): "), verbatimTextOutput("numberCurrentSampleRows")),
                      box(width=6,
                        numericInput("sample",label='Seleccione el tamaño de la muestra a validar [en %]',
                                     min=1, max=100, value=conf$sampleSize),
                        p("El tamaño de la muestra sería (nº): "), verbatimTextOutput("numberRowsSample")
                      ))),
                    fluidRow(
                      box(title="Número de validaciones y nivel de acuerdo mínimo por resumen", width = 12, solidHeader = TRUE,status = "primary",
                          box(title="Número mínimo de validaciones por resumen", width = 6,
                              strong("Mínimo número actual de validaciones por resumen: "), verbatimTextOutput("minValid"),
                              sliderInput("minValid",label='Seleccione el número mínimo de validaciones por resumen:',
                                          min=3, max=10, value = conf$minNumValid)),
                          box(title="Mínimo % de nivel de acuerdo por resumen", width = 6,
                              strong("Mínimo % actual de acuerdo por resumen: "), verbatimTextOutput("minAgreem"),
                              sliderInput("minAgreem",label='Seleccione el tamaño de la muestra a validar [en %]',
                                          min=0, max=100, value = conf$minLevelAgreem)
                    ))),
                    fluidRow(
                      box(width = 12, title = "Gestión de ficheros",solidHeader = TRUE,status = "primary",
                          box(title="Fichero de artículos a validar", width = 6,
                              strong("Nombre del fichero actual:"),verbatimTextOutput("currentArticlesFile"),
                              radioButtons("changeArticlesFile",label="Desea cambiar el fichero de artículos a validar ",
                                           choices=list("Yes" = 1, "No" = 2),
                                           selected = 2),
                              conditionalPanel(
                                condition = "input.changeArticlesFile == 1",
                                fileInput("newArticlesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                          multiple = FALSE, accept = ".csv")
                              )
                          ),
                          box(title="Fichero de resúmenes a validar", width = 6,
                              strong("Nombre del fichero actual:"),verbatimTextOutput("currentSummariesFile"),
                              radioButtons("changeSummariesFile",label="Desea cambiar el fichero de resúmenes a validar ",
                                           choices=list("Yes" = 1, "No" = 2),
                                           selected = 2),
                              conditionalPanel(
                                condition = "input.changeSummariesFile == 1",
                                fileInput("newSummariesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                          multiple = FALSE, accept = ".csv")
                              )
                          ),
                      )),#outer box y fluidRow
                    actionButton("saveConfig", label= "Guardar configuración",class="btn-info")
             ),#tabItem manageEvalSummaries
            tabItem(tabName ="dashboadEvalSummaries",
                    fluidRow(
                      infoBoxOutput("agreementBox"),
                      infoBoxOutput("validatedBox"),
                      infoBoxOutput("pendingBox")
                    ),
                    fluidRow(
                      box(width=6, plotOutput("OKKO_plot")),
                      box(width=6, plotOutput("typeErrorsCountPlot"))
                    ),
                    fluidRow(
                      box(
                      strong("Agreed Answers: muestra la respuesta acordada entre los usuarios que han validado los resúmenes 
                         a la pregunta: \"El resumen generado: ¿trasmite la idea general del artículo?\""),
                      p("1: SI       2: NO"),
                      br()),
                      box(width = 12, dataTableOutput("results"))
                    )#fluidRow
            ),#tabItem dashboardEvalSummaries
            tabItem(tabName = "users",
                    fluidRow(
                      box(width = 6, title = "Crear nuevo usuario",solidHeader = TRUE,status = "primary",
                          textInput("usernameInput", label = "Ingrese el nombre de usuario", value = ""),
                          textInput("pswInput", label = "Ingrese la contraseña", value = ""),
                          selectInput("typeUserInput", label = "Seleccione",
                                      choices = credentials$permission),
                          actionButton("saveNewUser", label= "Crear nuevo usuario",class="btn-primary")
                      ),
                      box(width = 6, title = "Modificar usuario",solidHeader = TRUE,status = "primary",
                          selectInput("usernameInputChg", label = "Seleccione",
                                      choices = REG_USERS$users$username_id),
                          textInput("pswInputChg", label = "Ingrese la nueva contraseña", value = ""),
                          selectInput("typeUserInputChg", label = "Seleccione",
                                      choices = list("expert", "admin")),
                          actionButton("changeUser", label= "Modificar usuario",class="btn-primary")
                      )),
                    fluidRow(
                      box(width = 12, title = "Listado de usuarios",solidHeader = TRUE,status = "primary",
                          checkboxGroupInput("cgTypeUser", label = "Filtrar por tipo de usuario",
                                  choices = list("expert", "admin"),
                                  selected = c("expert")),
                        dataTableOutput("tableUsers"))
                    )
            ),#tabItem users
            tabItem(tabName = "manageData",
                    box(width = 6, title = "Guardar workspace",solidHeader = TRUE,status = "primary",
                      #actionButton("saveImage", label = "Guardar")
                      actionButton("saveImage", span("Guardar", id="UpdateAnimate", class="")),
                      #####
                      tags$head(tags$style(type="text/css", '
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
                margin-top: -0.3em;
                line-height: 1.5em;
                vertical-align: text-bottom;
                box-sizing: border-box;
            }
            .loading.dots::after {
                text-rendering: geometricPrecision;
                content: "⠋\\A⠙\\A⠹\\A⠸\\A⠼\\A⠴\\A⠦\\A⠧\\A⠇\\A⠏";
                animation: spin10 1s steps(10) infinite;
                animation-duration: 1s;
                animation-timing-function: steps(10);
                animation-delay: 0s;
                animation-iteration-count: infinite;
                animation-direction: normal;
                animation-fill-mode: none;
                animation-play-state: running;
                animation-name: spin10;
            }
            .loading::after {
                display: inline-table;
                white-space: pre;
                text-align: left;
            }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            '))
                    )
            )
          )#tabItems
        } #fin if cuando el usuario es el administrador
  }
    else {
    loginpage
  }
  })


  #User Expert
  ######

  selectedArticleData <- reactive ({
    if(!is.null(articles)){
    articles %>% select(title,url,text) %>% filter(title %in% input$selectTitle)
    }
    else{
      shinyalert(title="Salga de la aplicación",
                 text="No se ha cargado el fichero con los artículos cuyos
                 resúmenes hay que validar",
                 type="error")
    }
  })

  filteredGeneratedSummary<- reactive ({
    if(!is.null(summaries)){
      summaries[[1]][which(articles$title == input$selectTitle)]
    }
    else{
      shinyalert(title="Salga de la aplicación",
                 text="No se han cargado el fichero con los
                 resúmenes a validar",
                 type="error")
    }
  })

  output$expertGreetingBox <- renderInfoBox({
    infoBox(
      "Hola,",
      USERNAME$name,
      icon = icon("fal fa-user"),   # ("glyphicon-check", lib = "glyphicon"),
      color = "purple"
    )
  })

  output$expertValidatedBox <- renderInfoBox({
    infoBox(
      "Validated Summaries",
      nrow(distinct(VALIDATIONS$positions)),
      icon = icon("fal fa-check"),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })

  output$expertPendingBox <- renderInfoBox({
    infoBox(
      "Pending Validation",
      length(TITLES$userTitles), 
      icon = icon("fal fa-edit"), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })

  output$articleURL <- renderUI({
    HTML(paste0("<p><b>Para leer el artículo visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))
  })

  output$text <- renderText(selectedArticleData()$text)

  output$generatedSummary <- renderText(filteredGeneratedSummary())

  ######
  # SAVE VALIDATION AND UPDATE TITLES LIST
  observeEvent(input$validateButton,{
    #create structure to save validation
    validation <- data.frame(matrix(ncol=numColEV,nrow=1))
    colnames(validation) <- expertsValidationsColNames
    #save values
    validation$usernameId <- USERNAME$name #input$userName
    validation$position <- articles[which(articles$title == input$selectTitle),6]
    validation$question1 <- input$question1
    if(input$question1 == 2){
      validation$question2 <- 0
      validation$question3 <- 0
    }
    else{
      validation$question2 <- input$question2
      validation$question3 <- input$question3
    }
    validation$timeStamp <- Sys.time()
    validation$summariesNameFile <- conf$fileSummaries
    validation$articlesNameFile <- conf$fileArticles
    validation$articleTitle <- input$selectTitle
    print(validation)

    #write.table(validation,file="data/expertsValidations.csv",append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    filePath <- file.path(tempdir(), FILENAMEEV$name) #"validations.csv")
    write.table(validation,file=filePath,append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    drop_upload(filePath,outputDir) #"summariesApp/responses"

    shinyalert(title="Validation stored",type="success")

    #update select input title list pending validation by user ---> OJO FALTA AGREGAR EL NIVEL DE ACUERDO ENTRE TODOS
    position <- validation$position
    VALIDATIONS$positions <- append(VALIDATIONS$positions$position,position)
    VALIDATIONS$positions <- as.data.frame(VALIDATIONS$positions)
    colnames(VALIDATIONS$positions) <- c("position")
    TITLES$userTitles <- sample(ARTICLES$df[!(row.names(ARTICLES$df) %in% VALIDATIONS$positions$position),3])
    updateSelectInput(session,"selectTitle",choices=TITLES$userTitles)
  })

  #Admin
  ######

  # Configure Experiment
  ######

  # Reactives Value to store the the current name of the file of summaries and articles that are being validated
  SAMPLE_ROWS <- reactiveValues(size = sampleSize) #SAMPLE$size
  SAMPLE_PERC <- reactiveValues(size = conf$sampleSize) #SAMPLE$size
  SUMM_FILE <- reactiveValues(fileName = conf$fileSummaries) #SUMM_FILE$fileName
  ARTIC_FILE <- reactiveValues(fileName = conf$fileArticles) #ARTIC_FILE$fileName
  MIN_VALID <- reactiveValues(n = conf$minNumValid) #MIN_VALID$n
  MIN_AGREEM <- reactiveValues(l = conf$minLevelAgreem) #MIN_AGREEM$l

  output$numberRowsArticles <- renderPrint({
    numArticles
  })

  output$currentSampleSize <- renderPrint({
    SAMPLE_PERC$size
  })

  output$numberCurrentSampleRows <- renderPrint({
    SAMPLE_ROWS$size
  })

  output$numberRowsSample <- renderPrint({
    round(input$sample*numArticles/100,0)
  })

  output$minValid <- renderPrint({
    MIN_VALID$n
  })

  output$minAgreem <- renderPrint({
    MIN_AGREEM$l
  })

  output$currentSummariesFile <- renderPrint({
    SUMM_FILE$fileName
  })

  output$currentArticlesFile <- renderPrint({
    ARTIC_FILE$fileName
  })

  #####
  observeEvent(input$saveConfig,{
    #save input values in case there are changes
    conf$sampleSize <<- input$sample
    SAMPLE_PERC$size <- input$sample
    SAMPLE_ROWS$size <- round(input$sample*numArticles/100,0)
    conf$minNumValid <<- input$minValid
    MIN_VALID$n <- input$minValid
    conf$minLevelAgreem <<- input$minAgreem
    MIN_AGREEM$l <- input$minAgreem

    if(input$changeSummariesFile == 1){
      if(input$newSummariesFile$type == "text/csv"){
        conf$fileSummaries <<- input$newSummariesFile$name
        SUMM_FILE$fileName <- input$newSummariesFile$name
        #load new summaries file
        dir.create("tempdir")
        file.copy(input$newSummariesFile$datapath, file.path("tempdir",input$newSummariesFile$name))
        drop_upload(paste0("tempdir/",input$newSummariesFile$name),inputDir, mode = "overwrite")
        summaries <<- loadCSV(inputDir,conf$fileSummaries)
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileSummaries ," porque no es de tipo csv")
        shinyalert(title=msg,type="warning")
        message(msg)
      }
    }

    if(input$changeArticlesFile == 1){
      if(input$newArticlesFile$type == "text/csv"){
        conf$fileArticles <<- input$newArticlesFile$name
        ARTIC_FILE$fileName <- input$newArticlesFile$name
        #load new articles file
        dir.create("tempdir")
        file.copy(input$newArticlesFile$datapath, file.path("tempdir",input$newArticlesFile$name))
        drop_upload(paste0("tempdir/",input$newArticlesFile$name),inputDir, mode = "overwrite")
        articles <- loadCSV(inputDir,conf$fileArticles)
        numArticles <<- nrow(articles)
        ARTICLES$df <- articles
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileArticles," porque no es de tipo csv")
        shinyalert(title=msg,type="warning")
        message(msg)
      }
    }
    filePath <- file.path(tempdir(),"conf.csv")
    write.table(conf,file=filePath,append = FALSE,sep=',',row.names = FALSE)
    drop_upload(filePath,inputDir)
    sampleSize <<- round(conf$sampleSize*numArticles/100,0)
    samplePositions <<- sort(sample(1:numArticles,sampleSize,replace=F))
    articles <<- articles[samplePositions,]
    summaries <<- as.data.frame(summaries[samplePositions,])
    shinyalert(title="Configuración actualizada",type="success")
  })

  #Dashboard Experiment
  ######
  output$agreementBox <- renderInfoBox({
    print("pasa por krippendorf")
    infoBox(
      "Krippendorff's Alpha",
      round(krippAgreementValidated$alpha.hat,2),
      icon = icon("fal fa-handshake"),
      color = "purple"
    )
  })

  output$validatedBox <- renderInfoBox({
    print("pasa por articulos validados")
    infoBox(
      "Validated Articles",
      nrow(validationsbyUser),
      icon = icon("fal fa-check"),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })

  output$pendingBox <- renderInfoBox({
    print("pasa por pendingArticles")
    infoBox(
      "Pending Articles",
      nrow(articles) - nrow(validationsbyUser),
      icon = icon("fal fa-edit"), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })

  output$OKKO_plot <-renderPlot({
    print("pasa por OKKO plot")
    df <- expertsValidations %>%  select(question1) %>% count(question1) %>% mutate(percentage = paste0(round(n/sum(n)*100),"%"))
    df <- df %>% mutate(pos = cumsum(n) -0.5 * n)
    df$question1 <- as.factor(df$question1)
    levels(df$question1)<-c("Resumen OK","Resumen KO")

    ggplot(df,aes(y='',x=n,fill=question1)) +
      geom_col(position="fill") +
      labs(x="",title="Question 1") +
      #geom_text(aes(x=pos,label=percentage),size=3)
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="right",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank()) +
      theme_void()
  })

  output$results <-  DT::renderDataTable({
    #table <- adminArticles %>% select(-c("id","summary","text","position","url")) 
    table <- ADMIN_ARTICLES$df %>% select(-c("id","summary","text","position","url")) 
    datatable(table,options = list(autoWidth = TRUE,searching = TRUE))
  })

  # output$usersValidations <- renderPlot({
  #   #heatmapData <- tableAdmin[!is.na(tableAdmin$numValid),-c(1,3:5)]
  #   p <- expertsValidations %>% ggplot(aes(x=username_id,y=substr(title,1,20))) +
  #     geom_tile(aes(fill=as.factor(error))) +
  #     labs(x = "Expert username",y = "Title", title = "Tipo de error por resumen y usuario") #Summaries type of error by expert")
  #   p + scale_fill_discrete(name= "Error", labels=names(typeErrors))
  # })

  ######
  #Manage Users
  REG_USERS <- reactiveValues(users = credentials) #REG_USERS$users

  output$tableUsers <-  DT::renderDataTable({
   data <- REG_USERS$users %>% select(username_id,permission) %>% filter(permission %in% input$cgTypeUser)
   datatable(data, options = list(autoWidth = TRUE,searching = FALSE))
  })

  #####
  observeEvent(input$saveNewUser,{
    newUser <- data.frame(matrix(ncol=length(credentials),nrow=1))
    colnames(newUser) <- colnames(credentials)
    newUser$username_id = input$usernameInput
    newUser$passod = sapply(input$pswInput, sodium::password_store)
    newUser$permission = input$typeUserInput
    if(input$usernameInput %in% credentials$username_id){
      shinyalert(title="Nombre de usuario repetido, elija otro",type="error")
    }
    else{
      if(input$usernameInput != "" && input$pswInput != ""){
        filePath <- file.path(tempdir(),"users.csv")
        write.table(newUser,file=filePath,append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
        drop_upload(filePath,inputDir)
        credentials <<- rbind(credentials,newUser)
        REG_USERS$users <- credentials
        shinyalert(title="Nuevo usuario creado",type="success")
        #updateTabItems(session, inputId = "tabsAdmin", selected = "users") --> no funciona
      }
      else
      {
        shinyalert(title="Faltan datos, no se puede crear el usuario",type="error")
      }
    }
})
  
  #####
  observeEvent(input$changeUser,{
    if(input$pswInputChg != ""){
      credentials["passod"][which(credentials$username_id==input$usernameInputChg),] <<- sapply(input$pswInputChg, sodium::password_store)
      print("entro a if psw")
    }
    credentials["permission"][which(credentials$username_id==input$usernameInputChg),] <<- input$typeUserInputChg
    REG_USERS$users <- credentials
    filePath <- file.path(tempdir(),"users.csv")
    write.table(credentials,file=filePath,append = FALSE,sep=',',col.names = TRUE, row.names = FALSE)
    drop_upload(filePath,inputDir,mode = "overwrite")
    shinyalert(title="Cambios sobre el usuario guardados",type="success")
    #updateTabItems(session, inputId = "tabsAdmin", selected = "users") --> no funciona
  })

  
  #Save workspace
  ######
  observeEvent(input$saveImage,{
    shinyjs::addClass(id = "UpdateAnimate", class = "loading dots")
    shinyjs::disable("saveImage")
    fn <- file.path(tempdir(),paste0("summariesAppWorkspace-",Sys.Date(),".RData"))
    save.image(file = fn)
    drop_upload(fn,inputDir,mode = "overwrite")
    shinyjs::enable("saveImage")
    shinyjs::removeClass(id = "UpdateAnimate", class = "loading dots")
  })
  
}
