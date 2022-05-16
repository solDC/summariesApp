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
library(rdrop2)



########
# SERVER
########
server <- function(input, output, session) {

  rv <<- reactiveValues(conf=0,cred=0)
  
  articles <<- reactive({
    rv$conf
    
    articles <- loadCSV(paste0(inputDir,conf$fileArticles))
    message("Cargando artículos")
    articles
  })

  summaries <<- reactive({
    rv$conf
    req(articles())
    
    sum <- loadCSV(paste0(inputDir,conf$fileSummaries))
    message("Cargando resúmenes")
    sum
  })
  
  numArticles <<- reactive({
    req(articles())
    rv$conf
    message(" calcula numero de filas articulos")
    nrow(articles())
  })
  
  sampleSize <<- reactive({
    req(numArticles())
    rv$conf
    
    message(" calcula sample size")
    round(conf$sampleSize*numArticles()/100,0)
  })
  
  samplePositions <<- reactive({
    req(numArticles())
    req(sampleSize())
    rv$conf
    
    message(" calcula sample positions")
    sort(sample(1:numArticles(),sampleSize(),replace=F))
  })
  
  articlesValidate <<- reactive({
    rv$conf
    req(samplePositions())
    req(articles())
    
    message("calcula articulos validar")
    articlesValidate <- articles()[samplePositions(),]
    articlesValidate$position <- samplePositions()
    articlesValidate
  })

  summariesValidate <<- reactive({
    rv$conf
    req(summaries())
    req(samplePositions())
    
    message("calcula summariesValidate")
    summariesValidate <- as.data.frame(summaries[samplePositions(),])
    summariesValidate$position <- samplePositions()
    colnames(summariesValidate) <- c("summary","articlesPosition")  
    summariesValidate
  })
  
  #LOGIN: SHOW APPROPIATE INTERFACE
  ######
  login <- FALSE
  name <- NULL
  USER <- reactiveValues(login = login, name = name)
  
  observe({
    rv$cred
    req(articles())
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
                USER$name <- Username
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
  
#   observeEvent(input$login,{
#   if (USER$login == TRUE) {
#     if(typeUser() == "expert"){
#       
#       #EXPERT_VALIDATION$df <- loadCSV(outputDir,FILENAMEEV$name)
#       
#       #filter positions of validated articles by logged user
#       
#       print(expertsValidations %>%
#               filter(usernameId == USERNAME$name))
#       validatedTitlesPos <- expertsValidations %>% filter(usernameId == USERNAME$name) %>% select(position)
#       VALIDATIONS$positions <- as.data.frame(validatedTitlesPos)
#       print(validatedTitlesPos)
#       # If there's agreement in certain summaries-articles, there's no need of validating them any more
#       if(AGREEM$n==1){
#         posDiscard <- AGREEMENTS$table%>% filter(agreemPerc > conf$minLevelAgreem) %>%  select(position)
#         print(posDiscard$position)
#         ARTICLES$df <- subset(articles,!(position %in% posDiscard$position))
#       }
#       # Randomize titles to validate so different users validate articles in different order
#       if (length(VALIDATIONS$positions) >= 1){
#         TITLES$userTitles <- sample(ARTICLES$df[!(row.names(ARTICLES$df) %in% VALIDATIONS$positions$position),3])
#       }
#       else{
#         TITLES$userTitles <- sample(ARTICLES$df$title)
#       }
#     }
#     else{#user admin
#       if(is.null(articles)){
#         message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
#       }
#       else{
#         if(is.null(summaries)){
#           message("El fichero con los resúmenes no se ha cargado.")
#         }
#         else{
#           print("crea admin articles")
#           adminArticles <- cbind(articles,summaries$summary)
#           names(adminArticles)[length(adminArticles)] <- "generatedSummary"
#           #agregar nivel de cuerdo
#           if(AGREEM$n == 1){
#             dfa <- AGREEMENTS$table %>%  select(position,agreemPerc,agreedAnswer)
#             ADMIN_ARTICLES$df <- adminArticles %>% left_join(dfa,by="position")
#           }
#         }
#       }
#     } #end if user admin
#   }#if login true
# })

  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      rv$conf
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
                  actionButton('saveConfig',span("Guardar configuración",id="UpdateAnimateSaveConfig",class="")),
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
                    box(width = 4, title = "Crear nuevo usuario",solidHeader = TRUE,status = "primary",
                        textInput("usernameInput", label = "Ingrese el nombre de usuario", value = ""),
                        textInput("pswInput", label = "Ingrese la contraseña", value = ""),
                        selectInput("typeUserInput", label = "Seleccione",
                                    choices = credentials$permission),
                        actionButton("saveNewUser", span("Crear Usuario", id="UpdateAnimateSaveUser", class="btn-primary"))),
                    box(width = 4, title = "Modificar contraseña usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgPsw", label = "Seleccione el usuario cuya contraseña quiere modificar",
                                    choices = credentials$username_id),
                        textInput("pswInputChg", label = "Ingrese la nueva contraseña", value = "btn-primary"),
                        actionButton("changePswUser", label= "Modificar",class="")
                    ), #box
                    box(width = 4, title = "Modificar tipo de usuario",solidHeader = TRUE,status = "primary",
                        selectInput("usernameInputChgType", label = "Seleccione el usuario cuyos permisos quiera modificar",
                                    choices = credentials$username_id),
                        selectInput("typeUserInputChgType", label = "Seleccione el nuevo tipo de usuario",
                                    choices = list("expert", "admin")),
                        actionButton("changeTypeUser", label= "Modificar",class="btn-primary")
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
          tabItem(tabName = "manageData",
                  box(width = 6, title = "Guardar workspace",solidHeader = TRUE,status = "primary",
                      #actionButton("saveImage", label = "Guardar")
                      actionButton("saveImage", span("Guardar", id="UpdateAnimateSaveImage", class="")),
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
    numArticles()
  })
  
  output$currentSampleSize <- renderPrint({
    conf$sampleSize
  })
  
  output$numberCurrentSampleRows <- renderPrint({
    sampleSize() 
  })
  
  output$numberRowsSample <- renderPrint({
    round(input$sample*numArticles()/100,0)
  })
  
  output$minValid <- renderPrint({
    conf$minNumValid
  })
  
  output$minAgreem <- renderPrint({
    conf$minLevelAgreem
  })
  
  output$currentSummariesFile <- renderPrint({
    conf$fileSummaries
  })
  
  output$currentArticlesFile <- renderPrint({
    conf$fileArticles
  })
  
 
  #####
  observeEvent(input$saveConfig,{
    message("entra en SaveConfig")
    shinyjs::addClass(id = "UpdateAnimateSaveConfig", class = "loading dots")
    shinyjs::disable("saveConfig")
    newSampleSize <- input$sample
    newMinNumValid <- input$minValid
    newMinLevelAgreem <- input$minAgreem
    newSummariesFile <- conf$fileSummaries
    newArticlesFile <- conf$fileArticles
    if(input$changeSummariesFile == 1){
      if(input$newSummariesFile$type == "text/csv"){
        newSummariesFile <- input$newSummariesFile$name
        #load new summaries file
        dir.create("tempdir")
        file.copy(input$newSummariesFile$datapath, file.path("tempdir",input$newSummariesFile$name))
        drop_upload(paste0("tempdir/",input$newSummariesFile$name),inputDir, mode = "overwrite")
        summaries <- paste0(inputDir,conf$fileSummaries)
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileSummaries ," porque no es de tipo csv")
        shinyalert(title=msg,type="warning")
        message(msg)
      }
    }
    
    if(input$changeArticlesFile == 1){
      if(input$newArticlesFile$type == "text/csv"){
        newArticlesFile <- input$newArticlesFile$name
        #load new articles file
        dir.create("tempdir")
        file.copy(input$newArticlesFile$datapath, file.path("tempdir",input$newArticlesFile$name))
        drop_upload(paste0("tempdir/",input$newArticlesFile$name),inputDir, mode = "overwrite")
        articles <<- loadCSV(paste0(inputDir,conf$fileArticles))
      }
      else{
        msg <- paste0("No se puede cargar el fichero ",conf$fileArticles," porque no es de tipo csv")
        shinyalert(title=msg,type="warning")
        message(msg)
      }
    }
    filePath <- file.path(tempdir(),"conf.csv")
    newConf <- data.frame(matrix(ncol=length(conf),nrow=1))
    colnames(newConf) <- colnames(conf)
    newConf$sampleSize <- newSampleSize
    newConf$fileSummaries <- newSummariesFile
    newConf$fileArticles <- newArticlesFile
    newConf$minNumValid <- newMinNumValid
    newConf$minLevelAgreem <- newMinLevelAgreem
    write.table(newConf,file=filePath,append = FALSE,sep=',',row.names = FALSE) #,col.names = FALSE)
    drop_upload(filePath,inputDir)
    conf <<- newConf #############################
    rv$conf <<- rv$conf + 1
    shinyalert(title="Configuración actualizada",type="success")
    shinyjs::enable("saveConfig")
    shinyjs::removeClass(id = "UpdateAnimateSaveConfig", class = "loading dots")
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
    # shinyjs::addClass(id = "UpdateAnimateSaveUser", class = "loading dots")
    # shinyjs::disable("saveUser")
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
        # filePath <- file.path(tempdir(),"users.csv")
        # write.table(newUser,file=filePath,append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
        # drop_upload(filePath,inputDir)
        credentials <<- rbind(credentials,newUser)
        rv$cred <<- rv$cred + 1
        shinyalert(title="Nuevo usuario creado",type="success")
        #updateTabItems(session, inputId = "tabsAdmin", selected = "users") 
      }
      else
      {
        shinyalert(title="Faltan datos, no se puede crear el usuario",type="error")
      }
    }
    # shinyjs::enable("saveUser")
    # shinyjs::removeClass(id = "UpdateAnimateSaveUser", class = "loading dots")
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
      shinyalert(title="Cambios sobre el usuario guardados",type="success")
    }
    else{
      shinyalert(title="Introduzca un tipo de usuario diferente para continuar",type="error")
    }
  })
  
  observeEvent(input$changePswUser,{
    if(input$pswInputChg != ""){
      print("entro a if psw")
      credentials["passod"][which(credentials$username_id==input$usernameInputChgPsw),] <<- sapply(input$pswInputChg, sodium::password_store)
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
    fn <- file.path(tempdir(),paste0("summariesAppWorkspace-",Sys.Date(),".RData"))
    save.image(file = fn)
    drop_upload(fn,inputDir,mode = "overwrite")
    shinyalert(title="Imagen del workspace almacenada",type="success")
    shinyjs::enable("saveImage")
    shinyjs::removeClass(id = "UpdateAnimateSaveImage", class = "loading dots")
  })
  
   
}
