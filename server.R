library(shiny)
library(shinydashboard)
library(shinyjs)
library(rstan)
library(tidyverse)
library(dplyr)
library(tidyr)
library(shinyalert)
library(tibble)
library(DT)
library(krippendorffsalpha)
library(RColorBrewer)
library(ggplot2)

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

########
# SERVER
########
server <- function(input, output, session) {

  rv <<- reactiveValues(cred=0, confS=0, confAg=0, confArt=0, confSum=0, init=0, newExp=0, newValid=0, mergeEV=0) #rv$mergeEV  para actualizar tabla admin
  expertValid <- reactiveValues(atv=articles[FALSE,],donePos=NULL, flag=0)
  
  # Credentials reactive variables
  numExperts <<- reactive({
    rv$cred
    
    aux <- credentials %>% filter(permission == "expert")
    message(paste0("numExperts"),nrow(aux))
    nrow(aux)
  })
  
  typeUser <- reactive ({
    req(USER$login)
    
    credentials["permission"][which(credentials$username_id==USER$name),]
  })
  
  # Articles and summaries reactive variables
  numArticles <<- reactive({
    rv$confArt
    message(" calcula numArticles")
    nrow(articles)
  })
  
  sampleSize <<- reactive({
    req(numArticles)
    rv$confS
    rv$confArt
    
    message(" calcula sample size")
    round(conf$sampleSize[nrow(conf)]*numArticles()/100,0)
  })
  
  samplePositions <<- reactive({
    req(numArticles)
    req(sampleSize)
    rv$confS
    rv$confArt
    
    message(" calcula sample positions")
    sort(sample(1:numArticles(),sampleSize(),replace=F))
  })

  #LOGIN: SHOW APPROPIATE INTERFACE
  ######
  login <- FALSE
  name <- NULL
  USER <- reactiveValues(login = login, name = name)
  
  observe({
    rv$cred
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(!is.null(credentials)){
            if(length(which(credentials$username_id==Username))==1) {
              if(credentials["passod"][which(credentials$username_id==Username),] == Password){
                USER$login <- TRUE
                USER$name <- Username
              }
              else {
                shinyjs::toggle(id = "no password match", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
              Password <- NULL
            } #if (length(which ....))
            else {
              shinyjs::toggle(id = "no username match", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
          else{
            shinyalert(title="Salga de la aplicación",text="No es posible realizar la autenticación (no se ha cargado el fichero credentials).",
                       closeOnClickOutside = TRUE,type="error")
          } # end else if(!is.null(credentials))
        } # end if(input$login > 0)
      } # end if(!is.null(input$login))
    } # end if(USER$login == FALSE)
  }) # end observe
  
  ######
  # Logout button Render UI
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fas fa-sign-out",verify_fa = FALSE), "Logout",
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
          id="tabsExpert",
          menuItem("Informacion", tabName = "information", icon = icon("fas fa-info",verify_fa = FALSE)),
          menuItem("Validar resumen", tabName = "validate", icon = icon("th",lib = "font-awesome",verify_fa = FALSE))
        )
      }
      else{
        sidebarMenu(
          id="tabsAdmin",
          menuItem('Configurar "Validar Resúmenes"', tabName = "manageEvalSummaries", icon = icon("fal fa-cog",verify_fa = FALSE)),
          menuItem('Dashboard "Validar Resúmenes"', tabName = "dashboadEvalSummaries", icon = icon("tachometer-alt",lib = "font-awesome",verify_fa = FALSE)),
          menuItem('Gestionar Usuarios', tabName = "users", icon = icon("fal fa-user",verify_fa = FALSE)),
          menuItem('Guardar workspace', tabName = "manageData", icon = icon("fal fa-database",verify_fa = FALSE))
        )
      }
    }
  })
  
  #####
  observeEvent(input$login,{
  if (USER$login == TRUE) {
    rv$newExp
    rv$init

    # Save last user access
    credentials["lastLogin"][which(credentials$username_id==USER$name),] <<- format(Sys.Date(),origin="1970-01-01")
    
    if(typeUser() == "expert"){
      if(conf$init[nrow(conf)] == 0 ){
        shinyalert(title="Experimento no iniciado. Puede salir de la aplicación.", closeOnClickOutside = TRUE, type="info")
        print("está en conf$init[nrow(conf)] == 0")
      }
      else{
        # Initialize local variables
        expertValid$atv <- articlesToValidate
        df <- data.frame(matrix(ncol=1,nrow=0))
        colnames(df) <- "position"
        expertValid$donePos <- df
        # Check if the expert has validated any summaries of this experiment and discard them from the articles/summaries that are pending to validate
        validatedTitlesPos <- expertsValidations %>% filter(username_id == USER$name) %>% filter(idExp == conf$id[nrow(conf)]) %>% select(position)
        if(nrow(expertValid$donePos) >= 1){
          expertValid$donePos <- validatedTitlesPos$position
          print(expertValid$donePos)
          expertValid$atv <- expertValid$atv %>% anti_join(expertValid$donePos,by="position")
        }
        print(paste0(USER$name," ---- num done: ", nrow(expertValid$donePos)," --  num pending:  ",nrow(expertValid$atv)))
         # posDiscard <- agreements %>% filter(agreemPerc >= conf$minLevelAgreem) %>% select(position)
        # print(posDiscard$position)
        # expertValid$atv <- subset(expertValid$atv,!(position %in% posDiscard$position))
        print(expertValid$atv[1:3,c(-1,-2,-3)])
        
       
      }  
      # #   #No procesa antes currExpertPendingValid()
      # #   if((nrow(currExpertPendingValid() == 0))){ #(is.null(currExpertPendingValid())) || 
      # #   print("está en nrow(currExpertPendingValid() == 0")
      # #   shinyalert(title="No tiene resúmenes para validar. Puede salir de la aplicación.", closeOnClickOutside = TRUE, type="info")
      # #   }
      #   #else{
      # 
      #   #}

    }
  }#if login true
})

  output$body <- renderUI({
    if (USER$login == TRUE) {
      rv$confSum
      rv$confArt
      rv$confAg
      rv$confS
      rv$newExp
      
      if(typeUser() == "expert"){
        expertValid$flag
        
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
                                  choices = sample(expertValid$atv$title))) #expertValid$pendingTitles
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
                      actionButton('validateButton',span("Validar Resumen",id="UpdateAnimateValidateButton",class="")),
                      ######
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
                    )#box
                  ) #última fluidRow
          ) #tabItem validate
        ) # tabItems
      }#fin if si el usuario es el expert
      else{
        #Usuario admin
        tabItems(
          tabItem(tabName = "manageEvalSummaries", class = "active",
                  fluidRow(
                    box(title="Configuración del experimento de validación", width = 12, solidHeader = TRUE,status = "primary",
                        p("En esta sección podrá", strong(" configurar experimento de validación de resúmenes "),"generados por un sistema
                        inteligente indicanco los ficheros de artículos y resúmenes a utilizar, el tamaño de muestra y 
                          los parámetros para el cálculo del nivel acuerdo."),
                        p("Una vez que la configuración sea la definitiva, ", strong("active el experimento con el botón <<Iniciar>> "), "de
                        esta sección para que los expertos puedan realizar las validaciones. Se iniciarán todas las estructuras necesarias
                          para llevar a cabo y almacenar los resultados de la validación. "),
                        p(strong("Para configurar un nuevo experimento, "),"deberá deshabilitar el experimento en marcha con el ",
                          strong("botón <<Parar>>")," que aparecerá cuando inicie un experimento."),
                        )),
                  fluidRow(
                    useShinyjs(),
                    conditionalPanel("false",
                                     box(width = 6,title = "Estado Validación",
                                         radioButtons("stateValid",label="NO MODIFICAR",
                                                      choices=list("Iniciado" = 1, "No iniciado" = 0),
                                                      selected = conf$init[nrow(conf)]))), 
                    conditionalPanel(
                    condition = "input.stateValid == 1",
                      box(title="Parar Validación", width = 4, solidHeader = TRUE,status = "primary",
                          actionButton("stopValid", label = "Parar",class="btn-danger" )),
                    ),
                    conditionalPanel(
                      condition = "input.stateValid == 0",
                      box(title="Iniciar Validación", width = 4, solidHeader = TRUE,status = "primary",
                          actionButton("startValid", label = "Iniciar",class="btn-success" )),
                    ),
                  ), #fluidRow buttons
                  fluidRow(
                    box(title="Configuración", width = 12, solidHeader = TRUE,status = "primary",
                        tabsetPanel(type="tabs",
                                    tabPanel("Muestra",
                                             fluidRow(
                                               box(title="Tamaño de la muestra a validar", width = 12, solidHeader = TRUE,status = "primary",
                                                   box(width=6,
                                                       strong("Número total de artículos-resúmenes: "), verbatimTextOutput("numberRowsArticles"),
                                                       strong("Tamaño actual de la muestra (en %):"), verbatimTextOutput("currentSampleSize"),
                                                       strong("Tamaño actual de la muestra (nº): "), verbatimTextOutput("numberCurrentSampleRows")),
                                                   box(width=6,
                                                       numericInput("sample",label='Seleccione el tamaño de la muestra a validar [en %]',
                                                                    min=1, max=100, value=conf$sampleSize[nrow(conf)]),
                                                       p("El tamaño de la muestra sería (nº): "), verbatimTextOutput("numberRowsSample"),
                                                       actionButton('saveSampleSize',label="Guardar",class="btn-primary"))
                                                   ))
                                             ),
                                    tabPanel("Acuerdo",
                                             fluidRow(
                                               box(title="Número de validaciones y nivel de acuerdo mínimo por resumen", width = 12, solidHeader = TRUE,status = "primary",
                                                   box(title="Número mínimo de validaciones por resumen", width = 6,
                                                       strong("Mínimo número actual de validaciones por resumen: "), verbatimTextOutput("minValid"),
                                                       sliderInput("minValid",label='Seleccione el número mínimo de validaciones por resumen:',
                                                                   min=3, max=10, value = conf$minNumValid[nrow(conf)])), 
                                                   box(title="Mínimo % de nivel de acuerdo por resumen", width = 6,
                                                       strong("Mínimo % actual de acuerdo por resumen: "), verbatimTextOutput("minAgreem"),
                                                       sliderInput("minAgreem",label='Seleccione el tamaño de la muestra a validar [en %]',
                                                                   min=0, max=100, value = conf$minLevelAgreem[nrow(conf)])),
                                                   box(width = 12,
                                                       actionButton('saveParamAgreem',label="Guardar",class="btn-primary"))
                                              ))#outer box and fluidrow
                                             ),
                                    tabPanel("Ficheros",
                                      fluidRow(
                                               box(width = 12, title = "Gestión de ficheros",solidHeader = TRUE,status = "primary",
                                                   box(title="Fichero de artículos a validar", width = 6,
                                                       strong("Nombre del fichero actual:"),
                                                       verbatimTextOutput("currentArticlesFile"),
                                                       fileInput("newArticlesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                                                 multiple = FALSE, accept = ".csv"),
                                                       actionButton('saveNewArtFile',label="Guardar",class="btn-primary")),
                                                   box(title="Fichero de resúmenes a validar", width = 6,
                                                       strong("Nombre del fichero actual:"),
                                                       verbatimTextOutput("currentSummariesFile"),
                                                       fileInput("newSummariesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                                                 multiple = FALSE, accept = ".csv"),
                                                       actionButton('saveNewSumFile',label="Guardar",class="btn-primary"))
                                                   ),#outer box files
                                      ),#fluidRow
                                    )#tabPanel
                        )#tabsetpanel
                    ),#outer box conf
                  ) #fluidRow config
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
          #Tab users
          ###### 
          tabItem(tabName = "users",
                  fluidRow(
                    box(width = 4, title = "Crear nuevo usuario",solidHeader = TRUE,status = "primary",
                        textInput("usernameInput", label = "Ingrese el nombre de usuario", value = ""),
                        textInput("pswInput", label = "Ingrese la contraseña", value = ""),
                        selectInput("typeUserInput", label = "Seleccione",
                                    choices = credentials$permission,
                                    selected = "expert"),
                        actionButton("saveNewUser", label = "Crear Usuario", class="")),
                    box(width = 4, title = "Modificar contraseña usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgPsw", label = "Seleccione el usuario cuya contraseña quiere modificar",
                                    choices = credentials$username_id),
                        textInput("pswInputChg", label = "Ingrese la nueva contraseña", value = ""),
                        actionButton("changePswUser", label= "Modificar",class="")
                    ), #box
                    # box(width = 4, title = "Modificar tipo de usuario",solidHeader = TRUE,status = "primary",
                    #     selectInput("usernameInputChgType", label = "Seleccione el usuario cuyos permisos quiera modificar",
                    #                 choices = credentials$username_id),
                    #     selectInput("typeUserInputChgType", label = "Seleccione el nuevo tipo de usuario",
                    #                 choices = list("expert", "admin")),
                    #     actionButton("changeTypeUser", label= "Modificar",class="")
                    # ) #box
                    ), #fluidRow
                  fluidRow(
                    box(width = 12, title = "Listado de usuarios",solidHeader = TRUE,status = "primary",
                        checkboxGroupInput("cgTypeUser", label = "Filtrar por tipo de usuario",
                                           choices = list("expert", "admin"),
                                           selected = c("expert")),
                        dataTableOutput("tableUsers"))
                  )
          ),#tabItem users
          # tab manageData
          ##### 
          tabItem(tabName = "manageData",
                  box(width = 6, title = "Guardar workspace",solidHeader = TRUE,status = "primary",
                      #actionButton("saveImage", label = "Guardar")
                      actionButton("saveImage", span("Guardar Imagen en disco", id="UpdateAnimateSaveImage", class="")),
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
            ')),
                  ) #box
          ) #tabItem
        )#tabItems
      } #fin if cuando el usuario es el administrador
    }
    else {
      loginpage
    }
  })

  # Expert
  ######  
  output$expertGreetingBox <- renderInfoBox({
    infoBox(
      "Hola,",
      USER$name,
      icon = icon("fal fa-user",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "purple"
    )
  })

  output$expertValidatedBox <- renderInfoBox({
    expertValid$flag

    message("En box validated")
    infoBox(
      "Validated Summaries",
      nrow((expertValid$donePos)),
      icon = icon("fal fa-check",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })

  output$expertPendingBox <- renderInfoBox({
    expertValid$flag
    
    message("En box pending valids")
    infoBox(
      "Pending Validation",
      nrow(expertValid$atv),
      icon = icon("fal fa-edit",verify_fa = FALSE), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })

  selectedArticleData <- reactive ({
    if(articlesToValidateExists == 1){
    articlesToValidate %>% select(title,url,text,position,summary) %>% filter(title %in% input$selectTitle)
  }
  else{NULL}
  })
  
  output$articleURL <- renderUI({
    if(articlesToValidateExists ==1){
    HTML(paste0("<p><b>Para leer el artículo visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))
    }
    else{NULL}
  })
  
  output$text <- renderText(selectedArticleData()$text)
  
  output$generatedSummary <- renderText(selectedArticleData()$summary) #filteredGeneratedSummary())
  
  ######
  # SAVE VALIDATION AND UPDATE TITLES LIST
  observeEvent(input$validateButton,{

    # Save validation (user's answers)
    validation <- data.frame(matrix(ncol=length(expertsValidations),nrow=1))
    colnames(validation) <- colnames(expertsValidations)
    validation$idExp <- conf$id[nrow(conf)]
    validation$username_id <- USER$name
    validation$position <- articlesToValidate %>% filter(title == input$selectTitle)  %>% select(position)
    validation$question1 <- as.integer(input$question1)
    if(input$question1 == 2){
      validation$question2 <- 0
      validation$question3 <- 0
    }
    else{
      validation$question2 <- as.integer(input$question2)
      validation$question3 <- as.integer(input$question3)
    }
    validation$date <- format(Sys.Date(),origin="1970-01-01")
    validation$articleTitle <- input$selectTitle
    validation$questions  <- as.integer(validation$question1)*100+as.integer(validation$question2)*10+as.integer(validation$question3)
    print(validation)
    
    # Add validation to expertsValidations global dataframe
    expertsValidations <<- rbind(expertsValidations,validation)
    expertValid$done <- rbind(expertValid$done,validation$position)
    print(nrow(expertValid$done))
    expertValid$atv <- expertValid$atv %>% filter(!(position == validation$position))
    print(nrow(expertValid$atv))
    expertValid$flag <- expertValid$flag +1
    
    
    
    # agreements[agreements$position == validation$position,validation$username_id] <- validation$questions
    # agreements$numResp[agreements$position == validation$position] <- numExperts()-sum(is.na(agreements[agreements$position == validation$position,2:(numExperts()+1)]))
    # if(agreements$numResp[agreements$position == validation$position] >= conf$minNumValid[nrow(conf)]){
    #   comb <- combn(agreements[agreements$position == validation$position,2:(numExperts()+1)],2,simplify = FALSE)
    #   agreements$posPairs[agreements$position == validation$position] <- calcPairs(agreements$numResp[agreements$position == validation$position])
    #   n <- numExperts()+8
    #   for(val in comb){
    #     agreements[agreements$position == validation$position,n] <- (val[1]==val[2])
    #     n <- n+1
    #   }
    #   agreements$agreemCount[agreements$position == validation$position] <- 
    #     rowSums(agreements[agreements$position == validation$position,(numExperts()+8):length(agreements)],na.rm = TRUE)
    #   agreements$agreemPerc[agreements$position == validation$position] <- 
    #     with(agreements,ifelse(agreements$possPairs[agreements$position == validation$position] >= conf$minNumValid[nrow(conf)](),
    #                            round(agreements$agreemCount[agreements$position == validation$position] / 
    #                                    agreements$possPairs[agreements$position == validation$position] * 100,2),
    #                                                                                    0))
    #   if(agreements$agreemPerc < conf$minLevelAgreem[nrow(conf)]){
    #     agreements$agreedAnswer[agreements$position == validation$position] <- 0
    #   }
    #   else{
    #     agreements$agreedAnswer[agreements$position == validation$position] <- 
    #       strtoi(names(which.max(table(agreements[agreements$position == validation$position,2:(numExperts()+1)]))))
    #   }
    #   print(agreements[agreements$position == validation$position,])
    #   
    # }

    shinyalert(title="Validation stored",type="success")
    updateSelectInput(session,"selectTitle",choices=sample(expertValid$atv$title))

  })
  
  # Configure Experiment
  ######

  output$numberRowsArticles <- renderPrint({
    rv$confArt
    message("Actualiza number Rows articles")
    numArticles()
  })
  
  output$currentSampleSize <- renderPrint({
    rv$confS
    message("Actualiza current sample size perc ")
    conf$sampleSize[nrow(conf)]
  })
  
  output$numberCurrentSampleRows <- renderPrint({
    rv$confS
    message("Actualiza number rows current sample size")
    sampleSize() 
  })
  
  output$numberRowsSample <- renderPrint({
    round(input$sample*numArticles()/100,0)
  })
  
  output$minValid <- renderPrint({
    rv$confAg
    message("Actualiza min Valid")
    conf$minNumValid[nrow(conf)]
  })
  
  output$minAgreem <- renderPrint({
    rv$confAg
    message("Actualiza min Agreem")
    conf$minLevelAgreem[nrow(conf)]
  })
  
  output$currentSummariesFile <- renderPrint({
    rv$confSum
    message("Actualiza current SummariesFile")
    conf$fileSummaries[nrow(conf)]
  })
  
  output$currentArticlesFile <- renderPrint({
    rv$confArt
    message("Actualiza currentArticlesFile")
    conf$fileArticles[nrow(conf)]
  })
  
  observeEvent(input$startValid,{
    req(samplePositions)
    # Create the table structures to calculate agreements and to save users answers
      # if(articlesToValidateExists == 0){
        articlesToValidate <<- articles[samplePositions(),c(2,3,5)] # 
        articlesToValidate$position <<- samplePositions()
        articlesToValidate$summary <<- summaries[samplePositions(),]
        saveRDS(articlesToValidate,file=filePathAV)
        #colnames(articlesToValidate$summary) <<- "position"
        if(agreemExists == 1){
          posDiscard <- agreements %>% filter(agreemPerc >= conf$minLevelAgreem[nrow(conf)]) %>% select(position)
          articlesToValidate <<- subset(articlesToValidate,!(position %in% posDiscard$position))
        }
        print(head(articlesToValidate[1:5,-c(3)]))
        print(nrow(articlesToValidate))     
    # }
    # if(agreemExists == 0){
      nameUsers <- credentials %>% filter(permission == "expert") %>%  select(username_id)
      aux1 <- cbind(samplePositions(),data.frame(matrix(ncol=nrow(nameUsers)+6,nrow=sampleSize())))
      colnames(aux1)[1] <- "position"
      colnames(aux1)[2:(nrow(nameUsers)+1)] <- nameUsers$username_id
      colnames(aux1)[(nrow(nameUsers)+2):length(aux1)] <- c("numResp","posPairs","agreemCount","agreemPerc","agreedAnswer","Auto-Manual") 
      aux1[length(aux1)] <- "Automatic"
      nc <- numCols(nrow(nameUsers)) #factorial(nrow(nameUsers)) / (2 * factorial(nrow(nameUsers) - 2))
      aux2 <- data.frame(matrix(NA,ncol=nc,nrow=sampleSize()))
      colN <- rep("P",nc)
      colnames(aux2) <- paste0(colN,c(1:nc))
      agreements <<- cbind(aux1,aux2)
      print(agreements)
      agreemExists <<- 1
      rv$init <<- rv$init + 1
    # }
    # if(expValidExists == 0){
      expertsValidations <<- data.frame(matrix(ncol=9,nrow=0))
      colnames(expertsValidations) <<- c( "idExp","position","question1","question2","question3","questions","username_id","date","articleTitle")
      print(expertsValidations)
      expValidExists <<- 1
    # }
    
    # Show the Stop button and disable configuration buttons
    conf$init[nrow(conf)] <<- 1
    conf$dateInit[nrow(conf)] <<- format(Sys.Date(),origin="1970-01-01")
    updateRadioButtons(session,"stateValid",
                       choices=list("Yes" = 1, "No" = 0),
                       selected = 1)
    shinyjs::disable("saveNewSumFile")
    shinyjs::disable("saveNewArtFile")
    shinyjs::disable("saveParamAgreem")
    shinyjs::disable("saveSampleSize")
    shinyjs::disable("startValid")
    shinyjs::enable("stopValid")
  })

  observeEvent(input$stopValid,{
    updateRadioButtons(session,"stateValid",
                       choices=list("Yes" = 1, "No" = 0),
                       selected = 0)
    shinyjs::disable("stopValid")
    shinyjs::enable("startValid")
    shinyjs::enable("saveNewSumFile")
    shinyjs::enable("saveNewArtFile")
    shinyjs::enable("saveParamAgreem")
    shinyjs::enable("saveSampleSize")
    nrowC <- nrow(conf)
    conf$dateStop[nrowC] <<- format(Sys.Date(),origin="1970-01-01")
    newConf <- conf[nrowC,]
    newConf$id <- conf$id[nrowC]+1
    newConf$init <- 0
    newConf$dateInit <- NA
    newConf$dateStop <- NA
    conf <<- rbind(conf,newConf)
    #save data of this experiment, create paths for the new ones 
    saveRDS(agreements,file=filePathAg)
    filePathAg <<- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".rds"))
    agreemExists <<- 0
    saveRDS(expertsValidations,file=filePathEV)
    filePathEV <- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".rds"))
    expValidExists <<- 0
    saveRDS(articlesToValidate,file=filePathAV)
    filePathAV <- file.path(inputDir,paste0("articlesValidate-",conf$id[nrow(conf)],".rds"))
    rv$newExp <- rv$newExp + 1
    rv$init <<- rv$init + 1
  })
  
  observeEvent(input$saveSampleSize,{
    if(conf$init[nrow(conf)]==0){
      print("Se va a cambiar el tamaño de la muestra en conf")
      conf$sampleSize[nrow(conf)] <<- input$sample
      rv$confS <<- rv$confS + 1
      shinyalert(title="Tamaño de la muestra modificado", closeOnClickOutside = TRUE, type="success")
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  observeEvent(input$saveParamAgreem,{
    if(conf$init[nrow(conf)]==0){
      print("Se van a cambiar los parametros de configuracion para calcular el acuerdo")
      conf$minNumValid[nrow(conf)] <<- input$minValid
      conf$minLevelAgreem[nrow(conf)] <<- input$minAgreem
      rv$confAg <<- rv$confAg + 1
      print(rv$confAg)
      shinyalert(title="Cambios guardados", closeOnClickOutside = TRUE, type="success")
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  observeEvent(input$saveNewArtFile,{
    if(conf$init[nrow(conf)]==0){
      print("Se va a cambiar el fichero de artículos a validar")
      if(!is.null(input$newArticlesFile) && input$newArticlesFile$type == "text/csv"){
        file.copy(input$newArticlesFile$datapath, paste0(inputDir,input$newArticlesFile$name))
        conf$fileArticles[nrow(conf)] <<- input$newArticlesFile$name
        print(conf$fileArticles[nrow(conf)])
        articles <<- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
        rv$confArt <<- rv$confArt + 1
        print(rv$confArt)
        shinyalert(title="Nuevo fichero de artículos almacenado", closeOnClickOutside = TRUE, type="success")
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileArticles[nrow(conf)])
        shinyalert(title=msg,closeOnClickOutside = TRUE,type="warning")
        message(msg)
      }
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  observeEvent(input$saveNewSumFile,{
    if(conf$init[nrow(conf)]==0){
      print("Se va a cambiar el fichero de resúmenes a validar")
      if(!is.null(input$newSummariesFile) && input$newSummariesFile$type == "text/csv"){
        file.copy(input$newSummariesFile$datapath, paste0(inputDir,input$newSummariesFile$name))
        conf$fileSummaries[nrow(conf)] <<- input$newSummariesFile$name
        print(conf$fileSummaries[nrow(conf)])
        summaries <<- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)]))
        rv$confSum <<- rv$confSum + 1
        print(rv$confSum)
        shinyalert(title="Nuevo fichero de resúmenes almacenado", closeOnClickOutside = TRUE, type="success")
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileSummaries [nrow(conf)])
        shinyalert(title=msg,closeOnClickOutside = TRUE,type="warning")
        message(msg)
      }
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  #Manage Users
  ######
  output$tableUsers <-  DT::renderDataTable({
    rv$cred
    data <- credentials %>% select(username_id,permission) %>% filter(permission %in% input$cgTypeUser)
    datatable(data, options = list(autoWidth = TRUE,searching = FALSE))
  })
  
  observeEvent(input$saveNewUser,{
    if(input$usernameInput %in% credentials$username_id){
      shinyalert(title="Nombre de usuario repetido, elija otro",closeOnClickOutside = TRUE,type="error")
    }
    else{
      if(input$usernameInput != "" && input$pswInput != ""){
        newUser <- data.frame(matrix(ncol=length(credentials),nrow=1))
        colnames(newUser) <- colnames(credentials)
        newUser$username_id <- input$usernameInput
        newUser$passod <- input$pswInput #sapply(input$pswInput, sodium::password_store)
        newUser$permission <- input$typeUserInput
        newUser$lastLogin <- NA
        credentials <<- rbind(credentials,newUser)
        print(credentials)
        rv$cred <<- rv$cred + 1
        shinyalert(title="Nuevo usuario creado",closeOnClickOutside = TRUE,type="success")
        # Add new user to agreements table if exists and only if new user is an expert
        if((agreemExists == 1) && (newUser$permission == "expert")){
          newColUser <- rep(NA,sampleSize())
          agreements <<- add_column(agreements,newColUser,.after = numExperts()+1) #library(tibble)
          colnames(agreements)[numExperts()+1] <<- input$usernameInput
          #difCol <-numExperts() # En realidad es numExperts() - 1 pero todavía no se llegó a actualizar el numExperts() --> si se actualiza
          agreements <<- cbind(as.data.frame(agreements),as.data.frame(matrix(NA,ncol=numExperts()-1,nrow=sampleSize())))
          nc<-numCols(numExperts())
          colN <- rep("P",nc)
          colnames(agreements)[(length(agreements)-nc+1):length(agreements)] <<- paste0(colN,c(1:nc))
        }
        print(agreements)
      }
      else
      {
        shinyalert(title="Faltan datos, no se puede crear el usuario",closeOnClickOutside = TRUE,type="error")
      }
    }
  })
  
  #####
  # observeEvent(input$changeTypeUser,{
  #   currentType <- credentials["permission"][which(credentials$username_id==input$usernameInputChgType),] 
  #   newType <- input$typeUserInputChgType
  #   if( newType != currentType){
  #     print("entro a if cambio tipo usuario")
  #     credentials["permission"][which(credentials$username_id==input$usernameInputChgType),] <<- newType
  #     rv$cred <<- rv$cred + 1
  #     shinyalert(title="El tipo de usuario guardados", 
  #                text="El usuario deberá salir de la sesión para que los cambios se actualicen",closeOnClickOutside = TRUE,type="success")
  #   }
  #   else{
  #     shinyalert(title="Introduzca un tipo de usuario diferente para continuar",closeOnClickOutside = TRUE,type="error")
  #   }
  # })
  
  #####
  observeEvent(input$changePswUser,{
    if(input$pswInputChg != ""){
      print("entro a if psw")
      credentials["passod"][which(credentials$username_id==input$usernameInputChgPsw),] <<- input$pswInputChg #sapply(input$pswInputChg, sodium::password_store)
      rv$cred <<- rv$cred + 1
      shinyalert(title="Cambios sobre el usuario guardados",closeOnClickOutside = TRUE,type="success")
    }
    else{
      shinyalert(title="Introduzca una contraseña para continuar",closeOnClickOutside = TRUE,type="error")
    }
  })

  #Save workspace
  ######
  observeEvent(input$saveImage,{
    shinyjs::addClass(id = "UpdateAnimateSaveImage", class = "loading dots")
    shinyjs::disable("saveImage")
    save.image(paste0(inputDir,"summariesAppWorkspace-",Sys.Date(),".RData"))
    shinyalert(title="Imagen del workspace almacenada",closeOnClickOutside = TRUE,type="success")
    shinyjs::enable("saveImage")
    shinyjs::removeClass(id = "UpdateAnimateSaveImage", class = "loading dots")
  })
  
  # session$onSessionEnded(function(){
  #   rv$mergeEV <- rv$mergeEV +1
  #   expertsValidations <<- merge(expertsValidations,currExpertValidations(),all = TRUE)
  #   write.csv(x=expertsValidations,file=filePathEV,row.names = FALSE) 
  # })
}
