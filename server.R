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
                   tags$h3("VALIDACIÓN DE RESÚMENES", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   br(),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Nombre de usuario")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Contraseña")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "ACCEDER", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),#,icon=icon("far fa-sign-in",lib= "font-awesome")
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                   ))
)

########
# SERVER
########
server <- function(input, output, session) {
  
  rv <<- reactiveValues(cred=0, confS=0, confAg=0, confArt=0, confSum=0, init=0, newExp=0, listExp=c(0))
  filesAdmin <- reactiveValues(agreem=NULL, index=0) 
  
  # Credentials reactive variables
  numExperts <<- reactive({
    rv$cred
    
    aux <- credentials %>% filter(permission == "expert")
    #message(paste0("numExperts: "),nrow(aux))
    nrow(aux)
  })
  
  typeUser <- reactive ({
    req(USER$login)
    rv$cred
    
    credentials["permission"][which(credentials$username_id==USER$name),]
  })
  
  # Articles and summaries reactive variables
  numArticles <<- reactive({
    rv$confArt
    #message(" calcula numArticles")
    numArticlesG #nrow(articles)
  })
  
  sampleSize <<- reactive({
    req(numArticles)
    rv$confS
    rv$confArt
    
    #message(" calcula sample size")
    round(conf$sampleSize[nrow(conf)]*numArticles()/100,0)
  })
  
  samplePositions <<- reactive({
    req(numArticles)
    req(sampleSize)
    rv$confS
    rv$confArt
    
    #message(" calcula sample positions")
    sort(sample(1:numArticles(),sampleSize(),replace=F))
  })
  
  #LOGIN: SHOW APPROPIATE INTERFACE
  ######
  login <- FALSE
  name <- NULL
  USER <- reactiveValues(login = login, name = name)
  
  observe({
    rv$cred
    print("Observe Login")
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
          menuItem('Configurar experimento', tabName = "manageEvalSummaries", icon = icon("fal fa-cog",verify_fa = FALSE)),
          menuItem('Estado experimento', tabName = "dashboadEvalSummaries", icon = icon("tachometer-alt",lib = "font-awesome",verify_fa = FALSE)),
          menuItem('Gestionar usuarios', tabName = "users", icon = icon("fal fa-user",verify_fa = FALSE)),
          menuItem('Gestionar datos', tabName = "manageData", icon = icon("fal fa-database",verify_fa = FALSE)),
          menuItem('Resultados anteriores', tabName = "dashboadPrev", icon = icon("fal fa-history",lib = "font-awesome",verify_fa = FALSE))
        )
      }
    }
  })
  
  # Reactive Value to store the position of all validated titles perform by the user (previous and new)
  positions <- data.frame(matrix(ncol=1,nrow=0))
  colnames(positions) <- c("position")
  # userTitles <- data.frame(matrix(ncol=1,nrow=0))
  # colnames(userTitles) <- c("title")
  validations <- reactiveValues(positions = positions, pending = articlesToValidate, id= conf$id[nrow(conf)]) #userTitles = userTitles,flag = 0 ) #validations$positions
  
  #####
  observeEvent(input$login,{
    if (USER$login == TRUE) {
      rv$newExp
      rv$init
      
      # Save last user access
      credentials["lastLogin"][which(credentials$username_id==USER$name),] <<- format(Sys.Date(),origin="1970-01-01")
      
      print(typeUser())
      if(typeUser() == "expert"){
        if(conf$init[nrow(conf)] == 0){
          shinyalert(title="Experimento no iniciado. Puede salir de la aplicación.", closeOnClickOutside = TRUE, type="info")
        }
        else{
          #print(paste0("numero filas validations$pending antes de descartar posibles acuerdos en login: ",nrow(validations$pending)))
          # If there's agreement in certain summaries-articles, there's no need of validating them any more
          if(!is.null(agreements)){
            posDiscard <- agreements %>% filter(agreemPerc >= conf$minLevelAgreem[nrow(conf)]) %>%  select(position)
            print(posDiscard$position)
            validations$pending <- subset(validations$pending,!(position %in% posDiscard$position))
          }
          #print(paste0("numero filas validations$pending despues de descartar posibles acuerdos en login: ",nrow(validations$pending)))
          
          validatedTitlesPos <- expertsValidations %>% filter(username_id == USER$name) %>% filter(idExp == conf$id[nrow(conf)]) %>% select(position) 
          validations$positions <- as.data.frame(validatedTitlesPos$position)
          colnames(validations$positions) <- "position"
          
          # Randomize titles to validate so different users validate articles in different order
          if (nrow(validations$positions) >= 1){
            #validations$userTitles <- sample(validations$pending[!(validations$pending$position %in% validations$positions$position),2])
            validations$pending <- validations$pending %>% anti_join(validations$positions,by="position")
          }
          # else{
          #   #validations$userTitles <- sample(validations$pending$title)
          # }
          #if((length(validations$userTitles)==0) || (conf$init[nrow(conf)] == 2)){
          if((nrow(validations$pending)==0) || (conf$init[nrow(conf)] == 2)){
            shinyalert(title="No tiene más resúmenes por validar. Puede salir de la aplicación.", closeOnClickOutside = TRUE, type="info")
          }
        }  
      }
      else{ #admin
        print("entra en oberse button login admin")
        if(!is.na(conf$pendingAgreem[nrow(conf)])){
          if ((agreemExists == 1) && (conf$init[nrow(conf)]==1) && (conf$pendingAgreem[nrow(conf)] == 0)){
            shinyalert(title = "Puede finalizar el experimento",
                       text ="Los expertos han validado todos los resúmenes", closeOnClickOutside = TRUE, type="info")
          }
          print("sale if qeu creia daba error")
        }
        
        print(paste0("imprime numero filas conf :",nrow(conf)))
        if(nrow(conf) > 1){
          rv$listExp <<- conf$id[1:(nrow(conf)-1)]
          print(rv$listExp)
        }
      }
    }#if login true
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      if(typeUser() == "expert"){
        #validations$flag
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
                                  choices = sample(validations$pending$title))) #validations$userTitles
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
                      radioButtons("question1", label = ("El resumen generado, ¿trasmite la idea general del artículo?"),
                                   choices = list("Sí" = 1, "No" = 2),
                                   selected = 2), 
                      conditionalPanel(
                        condition = "input.question1 == 1",
                        radioButtons("question2", label = ("2- El resumen, ¿contiene información inconsistente con el artículo?"),
                                     choices = list("Sí" = 1, "No" = 2),
                                     selected = 2),
                        radioButtons("question3", label = ("3- El resumen, ¿contiene alguna información que no puede ser inferida del artículo?"),
                                     choices = list("Sí" = 1, "No" = 2),
                                     selected = 2) 
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
        rv$confSum
        rv$confArt
        rv$confAg
        rv$confS
        rv$newExp
        rv$cred
        
        tabItems(
          # tab conf experiment
          #####
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
          ),###### tabItem manageEvalSummaries
          #####
          # tab status
          #####
          tabItem(tabName ="dashboadEvalSummaries",
                  fluidRow(
                    box(width = 12, title="Estado del experimento",solidHeader = TRUE, status="primary",
                        p("En cada sección se puede consultar el estado del último experimento configurado. Solamente 
                          se verán resultados cuando el experimento haya iniciado.")
                    )),
                  fluidRow(               
                    box(width = 12, title="Preguntas realizadas en el experimento",solidHeader = TRUE, status="primary",
                        collapsible = TRUE, #collapsed = TRUE,
                        p("En cada experimento, ", strong(" se realizan entre una y tres preguntas "),"al usuario ",
                          strong("sobre el resumen ") ,"generado que pueden responderse con un ",strong("sí o no"),":"),
                        p(strong("Pregunta 1: "),"El resumen generado: ¿trasmite la idea general del artículo? Sí/No"),
                        p(strong("Solo si ha contestado sí la pregunta 1:")),
                        p(strong("Pregunta 2: "),"El resumen, ¿contiene información inconsistente con el artículo? Sí/No"),
                        p(strong("Pregunta 3: "),"El resumen, ¿contiene alguna información que no puede ser inferida del artículo? Sí/No"),
                    )),
                  fluidRow(
                    box(width = 12, title= "Principales indicadores",solidHeader = TRUE, status="primary",
                      infoBoxOutput("agreementBox"),
                      infoBoxOutput("validatedBox"),
                      infoBoxOutput("pendingBox")
                  )),
                  fluidRow(
                    box(title= "Respuestas de los usuarios por resumen",width = 12, status = "primary", solidHeader = TRUE,
                        dataTableOutput("results")),
                    box(title= "Información sobre el resumen",width = 12, status = "primary", solidHeader = TRUE,
                        dataTableOutput("atv"))
                  ),#fluidRow
                  fluidRow(
                    box(width=12, title="Número de validaciones por usuario y tipo de respuesta",solidHeader = TRUE, status="primary",
                        box(width=6, title = "Número y tipo de respuestas por usuario",
                            plotOutput("countUsersAnswersPlot")),
                        box(width=6, title = "Número de validaciones por tipo de respuesta",
                            plotOutput("countTypeAnswerPlot"))
                    ),
                  ),#fluidRow
                  # fluidRow(
                  #   box(width = 12,title="Heatmap: tipo de respuestas por usuario",solidHeader = TRUE, status="primary",
                  #       plotOutput("usersValidations"))
                  # ),
 
          ),#tabItem dashboardPrev
          #####
          tabItem(tabName ="dashboadPrev",
                  fluidRow(
                    box(width = 12, title="Estado de experimentos finalizados",solidHeader = TRUE, status="primary",
                      p("En cada sección se puede consultar el estado de experimentos anteriores.")
                  )),
                  fluidRow(
                    box(width = 12, title = "Seleccione el id del experimento",solidHeader = TRUE,status="primary",
                        collapsible = TRUE, #collapsed = TRUE,
                        box(width = 3,
                            selectInput("selectIdExp",label = "",
                                        choices = rv$listExp,
                                        selected = 1), 
                            br(),br(),br(),
                            actionButton("selectIdBtn",label="Seleccionar",class="btn-primary")
                        ),
                        box(width = 9,
                            column(5,
                                   strong("Fichero artículos:"),textOutput("articlesFileOutput"),br(),
                                   strong("Fichero resúmenes:"),textOutput("summariesFileOutput"),br(),
                                   strong("Tamaño de la muestra:"),textOutput("sampleSizeOutput"),br()),
                            column(4,
                                   strong("Nº mínimo validaciones:"),textOutput("numValidOutput"),br(),
                                   strong("Mínimo nivel acuerdo:"),textOutput("minPercOutput"),br()),
                            column(3,
                                   strong("Estado:"),textOutput("statusOutput"),br(),
                                   strong("Fecha de inicio:"),textOutput("dateInitOutput"),br(),
                                   strong("Fecha de fin:"),textOutput("dateFinishOutput")),
                        ),#box
                    ), #outer box
                  ), #fluidRow
                  fluidRow(               
                    box(width = 12, title="Preguntas realizadas en el experimento",solidHeader = TRUE, status="primary",
                        collapsible = TRUE, #collapsed = TRUE,
                        p("En cada experimento, ", strong(" se realizan entre una y tres preguntas "),"al usuario ",
                          strong("sobre el resumen ") ,"generado que pueden ser repondidas con un ",strong("sí o no"),":"),
                        p(strong("Pregunta 1: "),"El resumen generado: ¿trasmite la idea general del artículo? Sí/No"),
                        p(strong("Solo si ha contestado sí la pregunta 1:")),
                        p(strong("Pregunta 2: "),"El resumen, ¿contiene información inconsistente con el artículo? Sí/No"),
                        p(strong("Pregunta 3: "),"El resumen, ¿contiene alguna información que no puede ser inferida del artículo? Sí/No"),
                    )),
                  fluidRow(
                    box(width = 12, title= "Principales indicadores",solidHeader = TRUE, status="primary",
                        infoBoxOutput("agreementBoxPrev"),
                        infoBoxOutput("validatedBoxPrev"),
                        infoBoxOutput("pendingBoxPrev")
                    )),
                  fluidRow(
                    box(width = 12, dataTableOutput("resultsPrev")),
                    box(width = 12, 
                        p("Seleccione una fila de la tabla anterior para ver los detalles del resumen:"),
                        dataTableOutput("atvSelected"))
                  ),#fluidRow
          ),#tabItem dashboardPrev
          #####
          #Tab users
          ##### 
          tabItem(tabName = "users",
                  fluidRow(
                    box(width = 12, title="Gestión de usuarios",solidHeader = TRUE, status="primary",
                      p("En esta sección se pueden crear, consultar y modificar usuarios.")
                  )),
                  fluidRow(
                    infoBoxOutput("usersTotalBox"),
                    infoBoxOutput("usersExpertsBox"),
                    infoBoxOutput("usersAdminBox")
                  ),
                  fluidRow(
                    box(width = 4, title = "Crear nuevo usuario",solidHeader = TRUE,status = "primary",
                        textInput("usernameInput", label = "Ingrese el nombre de usuario", value = ""),
                        textInput("pswInput", label = "Ingrese la contraseña", value = ""),
                        selectInput("typeUserInput", label = "Seleccione",
                                    choices = credentials$permission),
                        actionButton("saveNewUser", label = "Crear Usuario", class="")),
                    box(width = 4, title = "Modificar contraseña usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgPsw", label = "Seleccione el usuario cuya contraseña quiere modificar",
                                    choices = sort(credentials$username_id)),
                        textInput("pswInputChg", label = "Ingrese la nueva contraseña", value = ""),
                        actionButton("changePswUser", label= "Modificar",class="")
                    ), #box
                    box(width = 4, title = "Modificar tipo de usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgType", label = "Seleccione el usuario cuyos permisos quiera modificar",
                                    choices = sort(credentials$username_id)),
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
          ##### 
          #Tab manageData
          ##### 
          tabItem(tabName = "manageData",
                  fluidRow(
                    box(width = 12, title="Gestión de datos",solidHeader = TRUE, status="primary",
                        p("En esta sección se pueden descargar los ficheros y el workspace de la aplicación.")
                    )),
                  fluidRow(
                    box(width = 6, title = "Ficheros de entrada a la app (inputs)",solidHeader = TRUE,status = "primary",
                      selectInput("selectInputFile", label = "Seleccione el fichero que quiera descargar", 
                                  choices = list.files(inputDir), 
                                  selected = 1),
                      downloadButton("downloadInputFile",label = "Descargar")
                  )),
                  fluidRow(box(width = 6, title = "Ficheros de entrada a la app (inputs)",solidHeader = TRUE,status = "primary",
                      selectInput("selectOutputFile", label = "Seleccione el fichero que quiera descargar", 
                                  choices = list.files(outputDir), 
                                  selected = 1),
                      downloadButton("downloadOutputFile",label = "Descargar")
                  )),
                  box(width = 6, title = "Guardar workspace actual",solidHeader = TRUE,status = "primary",
                      #actionButton("saveImage", label = "Guardar")
                      actionButton("saveImage", span("Guardar Imagen en disco", id="UpdateAnimateSaveImage", class="")),
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
    infoBox(
      "Validaciones",
      nrow((validations$positions)),
      icon = icon("fal fa-check",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  output$expertPendingBox <- renderInfoBox({
    infoBox(
      "Pendientes",
      nrow(validations$pending), #length(validations$userTitles)
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
  
  output$generatedSummary <- renderText(selectedArticleData()$summary) 
  
  ######
  # SAVE VALIDATION AND UPDATE TITLES LIST
  observeEvent(input$validateButton,{
    shinyjs::disable("validateButton")
    # Do whats needed if the experiment is still running
    if((validations$id == conf$id[nrow(conf)]) && (conf$init[validations$id] == 1)){
      # Save validation (user's answers)
      #####
      validation <- data.frame(matrix(ncol=length(expertsValidations),nrow=1))
      colnames(validation) <- colnames(expertsValidations)
      #validation$idExp <- conf$id[nrow(conf)]
      validation$idExp <- validations$id
      validation$username_id <- USER$name
      pos <- articlesToValidate %>% filter(title == input$selectTitle)  %>% select(position)
      validation$position <- pos$position
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
      #####
      
      # Add validation to expertsValidations global dataframe
      #####
      expertsValidations <<- rbind(expertsValidations,validation)
      #####
      
      # Calculate if with this new answer generates a new agreement for this articles-summary
      # Calculate Krippendorff and store result in conf file
      #####
      rowIndex <- which(agreements$position == validation$position)
      colIndex <- which(colnames(agreements) == validation$username_id)
      agreements[rowIndex ,colIndex] <<- validation$questions
      agreements$numResp[rowIndex] <<- numExperts()-sum(is.na(agreements[rowIndex,2:(numExperts()+1)]))
      if(agreements$numResp[rowIndex] >= conf$minNumValid[nrow(conf)]){
        #message(paste0("Num Resp: ",agreements$numResp[rowIndex]))
        comb <- combn(agreements[rowIndex,2:(numExperts()+1)],2,simplify = FALSE)
        #message("Calcula combinaciones")
        agreements$posPairs[rowIndex] <<- calcPairs(agreements$numResp[rowIndex])
        #message("Calcula possPairs")
        n <- numExperts()+8
        for(val in comb){
          agreements[rowIndex,n] <<- (val[1]==val[2])
          #message("Calcula si los pares son o no iguales")
          n <- n+1
        }
        #message("Va a calcular agreemCount qeu dio error en rowSums")
        agreements$agreemCount[rowIndex] <<- rowSums(agreements[rowIndex,(numExperts()+8):length(agreements)],na.rm = TRUE)
        #print(paste0("agreements$agreemCount[rowIndex] ",agreements$agreemCount[rowIndex]))
        #print(paste0("agreements$posPairs[rowIndex] ",agreements$posPairs[rowIndex]))
        agreements$agreemPerc[rowIndex] <<- round(agreements$agreemCount[rowIndex] / agreements$posPairs[rowIndex] * 100,2)
        #print(paste0("agreements$agreemPerc[rowIndex] ",agreements$agreemPerc[rowIndex]))
        #print(paste0("conf$minLevelAgreem[nrow(conf)] ",conf$minLevelAgreem[nrow(conf)]))
        if(agreements$agreemPerc[rowIndex] < conf$minLevelAgreem[nrow(conf)]){
          #message("si el porcentaje de acuerdo es menor del configurado, da 0")
          agreements$agreedAnswer[rowIndex] <<- 0
        }
        else{
          vec <- table(unname(unlist(agreements[rowIndex,2:(numExperts()+1)])))
          agreements$agreedAnswer[rowIndex] <<- strtoi(names(which.max(vec)))
        }
      }
      #message("Deja de calcular agreement")
      # Check if there's agreement
      tableR <- agreements[,2:(numExperts()+1)]
      krip <- krippendorffs.alpha(as.matrix(tableR), level = "nominal", control = list(parallel = FALSE,bootit=100),verbose = TRUE)
      conf$kripp[nrow(conf)] <<- round(krip$alpha.hat,2)
      #message("Calcula Krippendorf con la nueva validación")
      #####
      
      # Discard other titles that may have reached an agreement with the answers of other users.
      # Save num. agreements and pending in conf file
      #####
      if(!is.null(agreements)){
        posDiscard <- agreements %>% filter(agreemPerc >= conf$minLevelAgreem[nrow(conf)]) %>%  select(position)
        validations$pending <- subset(validations$pending,!(position %in% posDiscard$position))
        conf$validAgreem[nrow(conf)] <<- nrow(posDiscard)
        conf$pendingAgreem[nrow(conf)] <<- nrow(articlesToValidate) - conf$validAgreem[nrow(conf)]
        # message(paste0("nrow(articlesToValidate) ",nrow(articlesToValidate)))
        # message(paste0(""))
        # message("Descarta, si toca, resumenes que tengan concenso")
      }
      #####
      
      # Discard the current validation from the titles that need validation
      #####
      position <- validation$position
      validations$positions <- as.data.frame(validations$positions)
      validations$positions <- rbind(validations$positions,position)
      colnames(validations$positions) <- c("position")
      #validations$userTitles <- validations$pending[!(validations$pending$position %in% validations$positions$position),2]
      validations$pending <- validations$pending[!(validations$pending$position %in% validations$positions$position),]
      #message("Descarta de lista de títulos a validar el qeu acaba de validar")
      #####
      
      #####  
      # Show confirmation to user and update list of titles pending to validate
      if((nrow(validations$pending)==0) || (conf$init[nrow(conf)] == 2)){
        shinyalert(title="Experimento finalizado. Puede salir de la aplicación.", closeOnClickOutside = TRUE, type="info")
      }
      else{
        shinyalert(title="Validación registrada",type="success")
      }
      updateSelectInput(session,"selectTitle",choices=sample(validations$pending$title)) #
    }
    else{
      shinyalert(title="Experimento finalizado. Puede salir d ela apliación.",type="info")
    }
    shinyjs::enable("validateButton")
  })
  
  
  ######
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
  
  observeEvent(input$saveSampleSize,{
    if(conf$init[nrow(conf)]==0){
      #print("Se va a cambiar el tamaño de la muestra en conf")
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
      #print("Se van a cambiar los parametros de configuracion para calcular el acuerdo")
      conf$minNumValid[nrow(conf)] <<- input$minValid
      conf$minLevelAgreem[nrow(conf)] <<- input$minAgreem
      rv$confAg <<- rv$confAg + 1
      #print(rv$confAg)
      shinyalert(title="Cambios guardados", closeOnClickOutside = TRUE, type="success")
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el experimento mientras está activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  observeEvent(input$saveNewArtFile,{
    if(conf$init[nrow(conf)]==0){
      #print("Se va a cambiar el fichero de artículos a validar")
      if(!is.null(input$newArticlesFile) && input$newArticlesFile$type == "text/csv"){
        file.copy(input$newArticlesFile$datapath, paste0(inputDir,input$newArticlesFile$name))
        conf$fileArticles[nrow(conf)] <<- input$newArticlesFile$name
        #print(conf$fileArticles[nrow(conf)])
        articles <<- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
        rv$confArt <<- rv$confArt + 1
        #print(rv$confArt)
        shinyalert(title="Nuevo fichero de artículos almacenado", closeOnClickOutside = TRUE, type="success")
        updateSelectInput("selectInputFile", label = "Seleccione el fichero que quiera descargar", 
                    choices = list.files(inputDir), 
                    selected = 1)
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
      #print("Se va a cambiar el fichero de resúmenes a validar")
      if(!is.null(input$newSummariesFile) && input$newSummariesFile$type == "text/csv"){
        file.copy(input$newSummariesFile$datapath, paste0(inputDir,input$newSummariesFile$name))
        conf$fileSummaries[nrow(conf)] <<- input$newSummariesFile$name
        #print(conf$fileSummaries[nrow(conf)])
        summaries <<- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)]))
        rv$confSum <<- rv$confSum + 1
        #print(rv$confSum)
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
  
  ##### Start Experiment
  observeEvent(input$startValid,{
    req(samplePositions)
    # Create the table structures to calculate agreements and to save users answers
    #if(articlesToValidateExists == 0){
      articlesToValidate <<- articles[samplePositions(),c(2,3,5)] # 
      articlesToValidate$position <<- samplePositions()
      articlesToValidate$summary <<- summaries[samplePositions(),]
      saveRDS(articlesToValidate,file=filePathAV)
      articlesToValidateExists <<- 1
    #}
    #if(agreemExists == 0){
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
      agreemExists <<- 1
      rv$init <<- rv$init + 1
      #saveRDS(agreements,file=filePathAg)
    #}
    #if(expValidExists == 0){
      expertsValidations <<- data.frame(matrix(ncol=9,nrow=0))
      colnames(expertsValidations) <<- c( "idExp","position","question1","question2","question3","questions","username_id","date","articleTitle")
      print(expertsValidations)
      expValidExists <<- 1
      #saveRDS(expertsValidations,file=filePathEV)
    #}
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
    shinyjs::disable("changeTypeUser")
    shinyjs::enable("stopValid")
  })
  
  ##### End Experiment
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
    shinyjs::enable("changeTypeUser")
    nrowC <- nrow(conf)
    conf$dateStop[nrowC] <<- format(Sys.Date(),origin="1970-01-01")
    conf$init[nrowC] <<- 2
    newConf <- conf[nrowC,]
    newConf$id <- conf$id[nrowC]+1
    newConf$init <- 0
    newConf$dateInit <- NA
    newConf$dateStop <- NA
    newConf$kripp <- NA
    newConf$validAgreem <- NA
    newConf$pendingAgreem <- NA
    conf <<- rbind(conf,newConf)
    #save data of this experiment, create paths for the new ones 
    saveRDS(agreements,file=filePathAg)
    filePathAg <<- file.path(outputDir,paste0("agreements-",conf$id[nrow(conf)],".rds"))
    agreemExists <<- 0
    agreements <<- NULL
    saveRDS(expertsValidations,file=filePathEV)
    filePathEV <<- file.path(outputDir,paste0("validations-",conf$id[nrow(conf)],".rds"))
    expValidExists <<- 0
    expertsValidations <<- NULL
    saveRDS(articlesToValidate,file=filePathAV)
    filePathAV <<- file.path(outputDir,paste0("articlesValidate-",conf$id[nrow(conf)],".rds"))
    articlesToValidateExists <<- 0
    articlesToValidate <<- NULL
    rv$newExp <<- rv$newExp + 1
    rv$init <<- rv$init + 1
    articles <<- loadCSV(paste0(inputDir,conf$fileArticles[nrow(conf)]))
    summaries <<- loadCSV(paste0(inputDir,conf$fileSummaries[nrow(conf)]))
    
    rv$listExp <<- nrow(conf)-1
    updateSelectInput(session,"selectIdExp",label = "",choices = rv$listExp)
  })
  
  
  ######
  #Validation Status Current
  ######
  
  output$agreementBox <- renderInfoBox({
    rv$newExp
    invalidateLater(10000,session)
    infoBox(
      "Alpha Krippendorff",
      conf$kripp[nrow(conf)], 
      icon = icon("fal fa-handshake",verify_fa = FALSE),
      color = "purple"
    )
  })
  
  output$validatedBox <- renderInfoBox({
    rv$newExp
    invalidateLater(10000,session)
    infoBox(
      "Validados con acuerdo",
      conf$validAgreem[nrow(conf)],
      icon = icon("fal fa-check",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  output$pendingBox <- renderInfoBox({
    rv$newExp
    invalidateLater(10000,session)
    infoBox(
      "Pendientes de acuerdo",
      conf$pendingAgreem[nrow(conf)],
      icon = icon("fal fa-edit",verify_fa = FALSE), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  output$results <-  DT::renderDataTable({
    rv$newExp
    invalidateLater(30000,session)
    if(agreemExists == 1){ 
      colIndex <- which(colnames(agreements)=="numResp")
      tableR <- agreements[1:colIndex]
      cols <- c(2:(colIndex-1))
      for (j in cols){
        for(i in 1:nrow(tableR)){
          tableR[i,j] <- ifelse(tableR[i,j] == 200,"No",
                                ifelse(tableR[i,j] == 111,"Sí-Sí-Sí",
                                       ifelse(tableR[i,j] == 112,"Sí-Sí-No",
                                              ifelse(tableR[i,j] == 121,"Sí-No-Sí",
                                                     ifelse(tableR[i,j] == 122, "Sí-No-No","Sin acuerdo")))))
        }
      }
      agreement <- ifelse(!is.na(agreements$agreemPerc),paste0(agreements$agreemPerc,"%"),"")
      tableR <- cbind(tableR,agreement)
      #tableR <- tableR %>% select_if(~ !all(is.na(.)))
      answers <- agreements$agreedAnswer
      n <- 1
      for(val in answers){
        answers[n] <- ifelse(val == 200,"No",ifelse(val == 111,"Sí-Sí-Sí",
                                                   ifelse(val == 112,"Sí-Sí-No",ifelse(val == 121,"Sí-No-Sí",
                                                                                    ifelse(val == 122, "Sí-No-No","Sin acuerdo")))))
        n <- n+1
      }
      tableR <- cbind(tableR,answers)
      colnames(tableR)[colnames(tableR) == 'position'] <- "Id"
      colnames(tableR)[colnames(tableR) == 'numResp'] <- "Número respuestas"
      colnames(tableR)[colnames(tableR) == 'answers'] <- "Respuesta acordada"
      colnames(tableR)[colnames(tableR) == 'agreement'] <- "Acuerdo alcanzado"
      tableR <- tableR[,c(1,(length(tableR)-2):length(tableR),2:(length(tableR)-3))]
      
      #tableR <- merge(tableR,articlesToValidate[,-3],by="position")
      DT::datatable(tableR,extensions = 'Buttons', selection = 'single',rownames = FALSE,
                    options = list(scrollX=TRUE, searching = FALSE, paging = TRUE, pageLength=10,
                                   dom = 'Bfrtip', buttons= c('copy', 'csv', 'excel')),
                    class = "display")
    }
  })
  
  output$atv <- DT::renderDataTable({
    rv$newExp
    invalidateLater(10000,session)
    if(articlesToValidateExists == 1){
      aux <- articlesToValidate[,c(4,2,5,1)]
      colnames(aux) <- c("Id","Título","Resumen","URL")
      limit <- min(nchar(aux$URL))
      aux$URL <- paste0('<a  target=_blank href=', aux$URL, '>', substr(aux$URL,13,limit),'</a>' ) 
      DT::datatable(aux,rownames = FALSE,escape=FALSE,options = list(autoWidth = TRUE, scrollX=TRUE, searching = TRUE, paging = TRUE,pageLength=5))
    }
  })
  
  auxData <- reactive({
    rv$newExp
    if(!is.null(expertsValidations)){
      aux <- expertsValidations
      aux$questions <- as.factor(aux$questions)
      levels(aux$questions) <- c("200","111","112","121","122")
      aux$questions <- recode(aux$questions, "112"= "Sí-Sí-No","111"="Sí-Sí-Sí","122"="Sí-No-No","121"="Sí-No-Sí","200"="No")
      colnames(aux)[colnames(aux) == 'questions'] <- "Respuestas"
    }
    else{
      aux <- NULL
    }
    aux
  })
  
  output$countUsersAnswersPlot <- renderPlot({
    rv$newExp
    if(!is.null(auxData())){
      ggplot(auxData(),aes(x=username_id,fill=as.factor(Respuestas))) +
        geom_bar() +
        coord_flip() +
        scale_fill_brewer(palette = "Pastel1",name= "Posibles respuestas") +  
        theme(legend.position="bottom")  + 
        #labs(title = "Cantidad y tipo de respuestas por usuario") +
        theme(panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    }
  })
  
  output$countTypeAnswerPlot <- renderPlot({
    rv$newExp
    if(!is.null(auxData())){
      ggplot(auxData(),aes(x= as.factor(Respuestas),fill=as.factor(Respuestas))) +
        geom_bar() +
        scale_fill_brewer(palette = "Pastel2",name= "Posibles respuestas") + 
        theme(legend.position="bottom") +
        geom_text(stat='count',aes(label=..count..),vjust=-0.7, size=3) +
        #labs(title="Número de validaciones por tipo de respuesta") +
        theme(panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    }
  })
  
  output$usersValidations <- renderPlot({
    rv$newExp
    if(exists("agreements") && (!is.null(agreements)) && (nrow(expertsValidations) > 1)){
      colMain <- colorRampPalette(brewer.pal(8, "Pastel1"))(5)
      heatmap(as.matrix(agreements[,2:(numExperts()+1)]),Colv = NA, Rowv = NA, scale="column",col = colMain,
              xlab="Expertos",#ylab="Resúmenes",#main="Type of Error of summaries by Users",
              labRow = paste0(agreements$position,"-",
                              substr(articlesToValidate$title[which(agreements$position==articlesToValidate$position)],1,30),
                              "..."))#=substr(tableAdmin$title,1,8))
      #heatmap(heatmapData,Colv = NA, Rowv = NA, scale="column",col = colMain)
      #legend(x="bottomright",fill=colMain) #,legend = names(typeErrors)
    }
  })
  
  
  ######
  #Validation Status Prev
  ######
  
  observeEvent(input$selectIdBtn,{
    if(input$selectIdExp > 0){
      filesAdmin$index <- unlist(conf %>% filter(id == input$selectIdExp) %>% select(id))
      #print(paste0("filesAdmin$index: ",filesAdmin$index))
      filesAdmin$agreem <- readRDS(file.path(outputDir,paste0("agreements-",conf$id[filesAdmin$index],".rds")))
    }
  })

  output$articlesFileOutput <- renderText({
    filesAdmin$index
    
    conf$fileArticles[filesAdmin$index]
  })
  
  output$summariesFileOutput <- renderText({
    filesAdmin$filesAdmin$index
    
    conf$fileSummaries[filesAdmin$index]
  })
  
  output$sampleSizeOutput <- renderText({
    filesAdmin$index
    
    paste0(conf$sampleSize[filesAdmin$index], "%")
  })

  output$numValidOutput <- renderText({
    filesAdmin$index
    
    paste0(conf$minNumValid[filesAdmin$index]," por resumen")
  })
  
  output$minPercOutput <- renderText({
    filesAdmin$index
    
    paste0(conf$minLevelAgreem[filesAdmin$index],"%")
  })

  output$statusOutput <- renderText({
    filesAdmin$index
    if(filesAdmin$index != 0){
      estado <- conf$init[filesAdmin$index]
      message(paste0("Estado: ",estado))
      switch(as.integer(estado+1),"No iniciado","Iniciado","Terminado")
    }
  })

  output$dateInitOutput <- renderText({
    filesAdmin$index
    
    conf$dateInit[filesAdmin$index]
  })

  output$dateFinishOutput <- renderText({
    filesAdmin$index
    
    conf$dateStop[filesAdmin$index]
  })
 
  output$agreementBoxPrev <- renderInfoBox({
    filesAdmin$index
    
    infoBox(
      "Krippendorff's Alpha",
      conf$kripp[filesAdmin$index], 
      icon = icon("fal fa-handshake",verify_fa = FALSE),
      color = "purple"
    )
  })
  
  output$validatedBoxPrev <- renderInfoBox({
    filesAdmin$index
    
    infoBox(
      "Validados con acuerdo",
      conf$validAgreem[filesAdmin$index], 
      icon = icon("fal fa-check",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })

  output$pendingBoxPrev <- renderInfoBox({
    filesAdmin$index
    
    infoBox(
      "Pendientes de validar",
      conf$pendingAgreem[filesAdmin$index],
      icon = icon("fal fa-edit",verify_fa = FALSE), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  output$resultsPrev <-  DT::renderDataTable({
    if((filesAdmin$index != 0) && (conf$init[filesAdmin$index] != 0)){
      filesAdmin$agreem <- readRDS(file.path(outputDir,paste0("agreements-",conf$id[filesAdmin$index],".rds")))
      if(!is.null(filesAdmin$agreem)){
        print("Pinta la tabla admin")
        colIndex <- which(colnames(filesAdmin$agreem)=="numResp")
        tableR <- filesAdmin$agreem[1:colIndex]
        
        cols <- c(2:(colIndex-1))
        for (j in cols){
          for(i in 1:nrow(tableR)){
            tableR[i,j] <- ifelse(tableR[i,j] == 200,"No",
                                  ifelse(tableR[i,j] == 111,"Sí-Sí-Sí",
                                         ifelse(tableR[i,j] == 112,"Sí-Sí-No",
                                                ifelse(tableR[i,j] == 121,"Sí-No-Sí",
                                                       ifelse(tableR[i,j] == 122, "Sí-No-No","Sin acuerdo")))))
          }
        }
        
        agreement <- ifelse(!is.na(filesAdmin$agreem$agreemPerc),paste0(filesAdmin$agreem$agreemPerc,"%"),"")
        tableR <- cbind(tableR,agreement)
        #tableR <- tableR[,colSums(is.na(tableR))<nrow(tableR)] #discard columns with no even one answer
        answers <- filesAdmin$agreem$agreedAnswer
        n <- 1
        for(val in filesAdmin$agreem$agreedAnswer){
          answers[n] <- ifelse(val == 200,"No",ifelse(val == 111,"Sí-Sí-Sí",
                                                     ifelse(val == 112,"Sí-Sí-No",ifelse(val == 121,"Sí-No-Sí",
                                                                                      ifelse(val == 122, "Sí-No-No","Sin acuerdo")))))
          n <- n+1
        }
        tableR <- cbind(tableR,answers)
        colnames(tableR)[colnames(tableR) == 'position'] <- "Id"
        colnames(tableR)[colnames(tableR) == 'numResp'] <- "Número respuestas"
        colnames(tableR)[colnames(tableR) == 'answers'] <- "Respuesta acordada"
        colnames(tableR)[colnames(tableR) == 'agreement'] <- "Acuerdo alcanzado"
        tableR <- tableR[,c(1,(length(tableR)-2):length(tableR),2:(length(tableR)-3))]
        
        DT::datatable(tableR,selection = 'single',rownames = FALSE,
                      options = list(autoWidth = TRUE, scrollX=TRUE, searching = FALSE, paging = TRUE))
      }
    }
  })
  
  observe({
    req(input$resultsPrev_rows_selected)
    
    filePathATV <- file.path(outputDir,paste0("articlesValidate-",filesAdmin$index,".rds"))
    if(file.exists(filePathAV)){
      dataATV <-  readRDS(filePathAV)
    }else{
      dataATV <- NULL
    }

    output$atvSelected <- DT::renderDataTable({
      if(!is.null(dataATV)){
        colnames(dataATV) <- c("URL","Título","Texto","Id","Resumen")
        dataATV <- dataATV[,c(4,2,3,5,1)]
        limit <- min(nchar(dataATV$URL))
        dataATV$URL <- paste0('<a  target=_blank href=', dataATV$URL, '>', substr(dataATV$URL,13,limit),'</a>' ) 
        DT::datatable(dataATV[input$resultsPrev_rows_selected, -c(3)], selection = 'single',rownames = FALSE,
                    options = list(autoWidth = TRUE, scrollX=TRUE))
      }
    })
    
  })
  
 
  ######
  #Manage Users
  ######
  
  output$usersTotalBox <- renderInfoBox({
    rv$cred
    infoBox(
      "Total",
      nrow(credentials),
      icon = icon("fal fa-users",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$usersExpertsBox <- renderInfoBox({
    rv$cred
    infoBox(
      "Expertos",
      count(credentials %>% filter(permission == "expert")),
      icon = icon("fal fa-user",verify_fa = FALSE),   # ("glyphicon-check", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  output$usersAdminBox <- renderInfoBox({
    rv$cred
    infoBox(
      "Administradores",
      count(credentials %>% filter(permission == "admin")),
      icon = icon("fal fa-user-cog",verify_fa = FALSE), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  output$tableUsers <-  DT::renderDataTable({
    rv$cred
    data <- credentials %>% select(username_id,permission,lastLogin) %>% filter(permission %in% input$cgTypeUser)
    colnames(data) <- c("Nombre de usuario","Tipo de usuario","Último login")
    datatable(data, rownames = FALSE, options = list(autoWidth = FALSE,searching = FALSE))
  })
  
  #####
  observeEvent(input$saveNewUser,{
    shinyjs::disable("saveNewUser")
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
        # print(paste0("Nueov usuario agregado",credentials$username_id))
        # print(credentials)
        
        # Add new user to agreements table if exists and only if new user is an expert
        if((agreemExists == 1) && (newUser$permission == "expert")){
          newColUser <- rep(NA,sampleSize())
          agreements <<- add_column(agreements,newColUser,.after = numExperts()+1) #library(tibble)
          agreements[numExperts()+2] <<- agreements[numExperts()+1]
          agreements[numExperts()+1] <<- rep(NA,sampleSize())
          colnames(agreements)[numExperts()+1] <<- input$usernameInput
          colnames(agreements)[numExperts()+2] <<- "numResp"
          agreements <<- cbind(as.data.frame(agreements),as.data.frame(matrix(NA,ncol=numExperts()-1,nrow=sampleSize())))
          nc<-numCols(numExperts())
          colN <- rep("P",nc)
          colnames(agreements)[(length(agreements)-nc+1):length(agreements)] <<- paste0(colN,c(1:nc))
        }
        shinyalert(title="Nuevo usuario creado",closeOnClickOutside = TRUE,type="success")
      }
      else
      {
        shinyalert(title="Faltan datos, no se puede crear el usuario",closeOnClickOutside = TRUE,type="error")
      }
    }
    updateSelectInput(session,"usernameInputChgPsw",choices=sort(credentials$username_id))
    updateSelectInput(session,"usernameInputChgType",choices=sort(credentials$username_id))
    updateTextInput(session, "usernameInput",value="")
    updateTextInput(session, "pswInput",value="")
    rv$cred <<- rv$cred + 1
    shinyjs::enable("saveNewUser")
  })
  
  #####
  observeEvent(input$changeTypeUser,{
    if(conf$init[nrow(conf)]==0){
      currentType <- credentials["permission"][which(credentials$username_id==input$usernameInputChgType),]
      newType <- input$typeUserInputChgType
      if( newType != currentType){
        #print("entro a if cambio tipo usuario")
        credentials["permission"][which(credentials$username_id==input$usernameInputChgType),] <<- newType
        rv$cred <<- rv$cred + 1
        shinyalert(title="El tipo de usuario guardados",
                   text="El usuario deberá salir de la sesión para que los cambios se actualicen",closeOnClickOutside = TRUE,type="success")
      }
      else{
        shinyalert(title="Introduzca un tipo de usuario diferente para continuar",closeOnClickOutside = TRUE,type="error")
      }
    }
    else{
      shinyalert(title="No se pueden realizar modificaciones en el tipo de usuario mientras hay un experimento de validación activo", 
                 closeOnClickOutside = TRUE, type="error")
    }
  })
  
  #####
  observeEvent(input$changePswUser,{
    if(input$pswInputChg != ""){
      #print("entro a if psw")
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
  
  dataCSV <- reactive({
    read.csv(paste0(inputDir,input$selectInputFile))
  })
  
  output$downloadInputFile <- downloadHandler(
    filename = function() {
      paste(input$selectInputFile, sep = "")
    },
    content = function(con) {
      write.csv(dataCSV(), con, row.names = FALSE)
    }
  )
  
  dataRDS <- reactive({
    readRDS(paste0(outputDir,input$selectOutputFile))
  })
  
  output$downloadOutputFile <- downloadHandler(
    filename = function() {
      paste(input$selectOutputFile, sep = "")
    },
    content = function(con) {
      saveRDS(dataRDS(), con)
    }
  )
  
  observeEvent(input$saveImage,{
    shinyjs::addClass(id = "UpdateAnimateSaveImage", class = "loading dots")
    shinyjs::disable("saveImage")
    save.image(paste0(inputDir,"summariesAppWorkspace-",Sys.Date(),".RData"))
    shinyalert(title="Imagen del workspace almacenada",closeOnClickOutside = TRUE,type="success")
    shinyjs::enable("saveImage")
    shinyjs::removeClass(id = "UpdateAnimateSaveImage", class = "loading dots")
  })
  
}