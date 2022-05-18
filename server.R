library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(tidyr)
library(shinyalert)
library(DT)
library(krippendorffsalpha)
library(RColorBrewer)
library(ggplot2)
library(rdrop2)


########
# SERVER
########
server <- function(input, output, session) {

  rv <<- reactiveValues(cred=0, confS=0, confAg=0, confArt=0, confSum=0)
 
  numArticles <<- reactive({
    rv$confArt
    message(" calcula numero de filas articulos")
    nrow(articles)
  })
  
  sampleSize <<- reactive({
    req(numArticles())
    rv$confS
    
    message(" calcula sample size")
    round(conf$sampleSize[nrow(conf)]*numArticles()/100,0)
  })
  
  samplePositions <<- reactive({
    req(numArticles())
    req(sampleSize())
    rv$confS
    rv$confArt
    
    message(" calcula sample positions")
    sort(sample(1:numArticles(),sampleSize(),replace=F))
  })
  
  articlesValidate <<- reactive({
    rv$confS
    rv$confArt
    req(samplePositions())
    
    message("calcula articulos validar")
    articlesValidate <- articles[samplePositions(),]
    articlesValidate$position <- samplePositions()
    articlesValidate
  })
  
  summariesValidate <<- reactive({
    rv$confS
    rv$confSum
    req(samplePositions())
    
    message("calcula summariesValidate")
    
    summariesValidate <- as.data.frame(summaries[samplePositions(),])
    summariesValidate$position <- samplePositions()
    colnames(summariesValidate) <- c("summary","articlesPosition")  
    print(head(summariesValidate))
    summariesValidate
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
              # pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              # pasverify <- password_verify(pasmatch, Password)
              # Password <- NULL # to avoid keeping the password available
              # if(pasverify) {
              #   USER$login <- TRUE
              #   USER$name <- Username
              # }
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
    # No need to check if credential is loaded, can't reach this point if it wasn't loaded.
    credentials["permission"][which(credentials$username_id==USER$name),]
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
  
  observeEvent(input$login,{
  if (USER$login == TRUE) {
     if(typeUser() == "expert"){
       if(conf$init == 0){ #AÑADIR CONDICION DE SI LA LISTA DEL USER EXPERT ESTÁ VACÍA
         shinyalert(title="No hay resúmenes para validar. Salga de la aplicación.", 
                    closeOnClickOutside = TRUE, 
                    type="warning")
       }
       
    # 
    #   #EXPERT_VALIDATION$df <- loadCSV(outputDir,FILENAMEEV$name)
    # 
    #   #filter positions of validated articles by logged user
    # 
    #   print(expertsValidations %>%
    #           filter(usernameId == USERNAME$name))
    #   validatedTitlesPos <- expertsValidations %>% filter(usernameId == USERNAME$name) %>% select(position)
    #   VALIDATIONS$positions <- as.data.frame(validatedTitlesPos)
    #   print(validatedTitlesPos)
    #   # If there's agreement in certain summaries-articles, there's no need of validating them any more
    #   if(AGREEM$n==1){
    #     posDiscard <- AGREEMENTS$table%>% filter(agreemPerc > conf$minLevelAgreem) %>%  select(position)
    #     print(posDiscard$position)
    #     ARTICLES$df <- subset(articles,!(position %in% posDiscard$position))
    #   }
    #   # Randomize titles to validate so different users validate articles in different order
    #   if (length(VALIDATIONS$positions) >= 1){
    #     TITLES$userTitles <- sample(ARTICLES$df[!(row.names(ARTICLES$df) %in% VALIDATIONS$positions$position),3])
    #   }
    #   else{
    #     TITLES$userTitles <- sample(ARTICLES$df$title)
    #   }
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
          print("admin loged")
          # print("crea admin articles")
          # adminArticles <- cbind(articles,summaries$summary)
          # names(adminArticles)[length(adminArticles)] <- "generatedSummary"
          # #agregar nivel de cuerdo
          # if(AGREEM$n == 1){
          #   dfa <- AGREEMENTS$table %>%  select(position,agreemPerc,agreedAnswer)
          #   ADMIN_ARTICLES$df <- adminArticles %>% left_join(dfa,by="position")
          }
        }
      }
    # } #end if user admin
  }#if login true
})

  output$body <- renderUI({
    if (USER$login == TRUE) {
      rv$confSum
      rv$confArt
      rv$confAg
      rv$confS
      
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
                                  choices = articlesValidate()$title)) #TITLES$userTitles))
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
                                                       # radioButtons("changeSummariesFile",label="Desea cambiar el fichero de resúmenes a validar ",
                                                       #              choices=list("Yes" = 1, "No" = 2),
                                                       #              selected = 2),
                                                       # conditionalPanel(
                                                       # condition = "input.changeSummariesFile == 1",
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
                                    choices = credentials$permission),
                        actionButton("saveNewUser", label = "Crear Usuario", class="")),
                    box(width = 4, title = "Modificar contraseña usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgPsw", label = "Seleccione el usuario cuya contraseña quiere modificar",
                                    choices = credentials$username_id),
                        textInput("pswInputChg", label = "Ingrese la nueva contraseña", value = ""),
                        actionButton("changePswUser", label= "Modificar",class="")
                    ), #box
                    box(width = 4, title = "Modificar tipo de usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgType", label = "Seleccione el usuario cuyos permisos quiera modificar",
                                    choices = credentials$username_id),
                        selectInput("typeUserInputChgType", label = "Seleccione el nuevo tipo de usuario",
                                    choices = list("expert", "admin")),
                        actionButton("changeTypeUser", label= "Modificar",class="")
                    ) #box
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
    #crear tablas base: agreem, user validations, muestra articulos a validar, deshabilitar botones conf
    conf$init[nrow(conf)] <<- 1
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
    newConf <- conf[nrowC,]
    newConf$id <- conf$id[nrowC]+1
    newConf$init <- 0
    conf <<- rbind(conf,newConf)
  })
  
  #####
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
  
  #####
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
  
  #####
  observeEvent(input$saveNewArtFile,{
    if(conf$init[nrow(conf)]==0){
      print("Se va a cambiar el fichero de artículos a validar")
      if(!is.null(input$newArticlesFile) && input$newArticlesFile$type == "text/csv"){
        # dir.create("tempdir")
        # file.copy(input$newArticlesFile$datapath, file.path("tempdir",input$newArticlesFile$name))
        # drop_upload(paste0("tempdir/",input$newArticlesFile$name),inputDir, mode = "overwrite")
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
        shinyalert(title=msg,type="warning")
        message(msg)
      }
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  #####
  observeEvent(input$saveNewSumFile,{
    if(conf$init[nrow(conf)]==0){
      print("Se va a cambiar el fichero de resúmenes a validar")
      if(!is.null(input$newSummariesFile) && input$newSummariesFile$type == "text/csv"){
        # dir.create("tempdir")
        # file.copy(input$newSummariesFile$datapath, file.path("tempdir",input$newSummariesFile$name))
        print(input$newSummariesFile$datapath)
        print(paste0(inputDir,input$newSummariesFile$name))
        file.copy(input$newSummariesFile$datapath, paste0(inputDir,input$newSummariesFile$name))
        #drop_upload(paste0("tempdir/",input$newSummariesFile$name),inputDir, mode = "overwrite")
        conf$fileSummaries[nrow(conf)] <<- input$newSummariesFile$name
        print(conf$fileSummaries[nrow(conf)])
        summaries <<- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)]))
        print(head(summaries))
        rv$confSum <<- rv$confSum + 1
        print(rv$confSum)
        shinyalert(title="Nuevo fichero de resúmenes almacenado", closeOnClickOutside = TRUE, type="success")
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileSummaries [nrow(conf)])
        shinyalert(title=msg,type="warning")
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
  
  #####
  observeEvent(input$saveNewUser,{
    newUser <- data.frame(matrix(ncol=length(credentials),nrow=1))
    colnames(newUser) <- colnames(credentials)
    newUser$username_id <- input$usernameInput
    newUser$passod <- input$pswInput #sapply(input$pswInput, sodium::password_store)
    newUser$permission <- input$typeUserInput
    if(input$usernameInput %in% credentials$username_id){
      shinyalert(title="Nombre de usuario repetido, elija otro",type="error")
    }
    else{
      if(input$usernameInput != "" && input$pswInput != ""){
        credentials <<- rbind(credentials,newUser)
        rv$cred <<- rv$cred + 1
        shinyalert(title="Nuevo usuario creado",type="success")
      }
      else
      {
        shinyalert(title="Faltan datos, no se puede crear el usuario",type="error")
      }
    }
  })
  
  observeEvent(input$changeTypeUser,{
    currentType <- credentials["permission"][which(credentials$username_id==input$usernameInputChgType),] 
    newType <- input$typeUserInputChgType
    if( newType != currentType){
      print("entro a if cambio tipo usuario")
      credentials["permission"][which(credentials$username_id==input$usernameInputChgType),] <<- newType
      rv$cred <<- rv$cred + 1
      # filePath <- file.path(tempdir(),"users.csv")
      # write.table(credentials,file=filePath,append = FALSE,sep=',',col.names = TRUE, row.names = FALSE)
      # drop_upload(filePath,inputDir,mode = "overwrite")
      shinyalert(title="El tipo de usuario guardados", 
                 text="El usuario deberá salir de la sesión para que los cambios se actualicen",type="success")
    }
    else{
      shinyalert(title="Introduzca un tipo de usuario diferente para continuar",type="error")
    }
  })
  
  observeEvent(input$changePswUser,{
    if(input$pswInputChg != ""){
      print("entro a if psw")
      credentials["passod"][which(credentials$username_id==input$usernameInputChgPsw),] <<- input$pswInputChg #sapply(input$pswInputChg, sodium::password_store)
      rv$cred <<- rv$cred + 1
      # filePath <- file.path(tempdir(),"users.csv")
      # write.table(credentials,file=filePath,append = FALSE,sep=',',col.names = TRUE, row.names = FALSE)
      # drop_upload(filePath,inputDir,mode = "overwrite")
      shinyalert(title="Cambios sobre el usuario guardados",type="success")
    }
    else{
      shinyalert(title="Introduzca una contraseña para continuar",type="error")
    }
  })

  #Save workspace
  ######
  observeEvent(input$saveImage,{
    shinyjs::addClass(id = "UpdateAnimateSaveImage", class = "loading dots")
    shinyjs::disable("saveImage")
    # fn <- file.path(tempdir(),paste0("summariesAppWorkspace-",Sys.Date(),".RData"))
    # save.image(file = fn)
    # drop_upload(fn,inputDir,mode = "overwrite")
    save.image(paste0(inputDir,"summariesAppWorkspace-",Sys.Date(),".RData"))
    shinyalert(title="Imagen del workspace almacenada",type="success")
    shinyjs::enable("saveImage")
    shinyjs::removeClass(id = "UpdateAnimateSaveImage", class = "loading dots")
  })
  
   
}
