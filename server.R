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
          menuItem("Informacion", tabName = "information", icon = icon("fas fa-info")),
          menuItem("Validar resumen", tabName = "validate", icon = icon("th",lib = "font-awesome"))
        )
      }
      else{
        sidebarMenu(
          menuItem('Gestionar "Validar Resúmenes"', tabName = "manageEvalSummaries", icon = icon("fal fa-database")),
          menuItem('Dashboard "Validar Resúmenes"', tabName = "dashboadEvalSummaries", icon = icon("tachometer-alt",lib = "font-awesome")),
          menuItem('Gestionar Usuarios', tabName = "users", icon = icon("fal fa-user"))
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
  EXPERTS_VALIDATIONS <- reactiveValues(df = df) #EXPERTS_VALIDATIONS$df

   # Reactives Value to store the the current name of the file of summaries and articles that are being validated
  SUMM_FILE <- reactiveValues(fileName = conf$fileSummaries) #SUMM_FILE$fileName
  ARTIC_FILE <- reactiveValues(fileName = conf$fileArticles) #ARTIC_FILE$fileName
  
  # Reactive Value to store the position of all validated titles perform by the user (previous and new)
  positions <- data.frame(matrix(ncol=1,nrow=0))
  colnames(positions) <- c("position")
  VALIDATIONS <- reactiveValues(positions = positions) #VALIDATIONS$positions
  
  # Reactive Value to store the titles that need validation from the user
  userTitles <- data.frame(matrix(ncol=1,nrow=0))
  colnames(userTitles) <- c("title")
  TITLES <- reactiveValues(userTitles = userTitles) #TITLES$userTitles
  
  ######
  observeEvent(input$login,{
    FILENAMEEV$name <- paste0("validations-",USERNAME$name,".csv")
    x <- drop_search(FILENAMEEV$name)
    if(x$start == 0){ 
      msg <- paste0("El fichero ",FILENAMEEV$name, " con las validaciones de los usuarios no existe y se va a crear uno vacío.")
      message(msg)
      expertsValidations <- data.frame(matrix(ncol=numColEV,nrow=0))
      colnames(expertsValidations) <- expertsValidationsColNames
      filePath <- file.path(tempdir(),FILENAMEEV$name)
      write.table(expertsValidations,file=filePath,sep=',',row.names = FALSE)
      drop_upload(filePath,outputDir) 
      message(paste0("Creado fichero",FILENAMEEV$name))
    }
    EXPERTS_VALIDATIONS$df <- loadCSV(outputDir,FILENAMEEV$name)
    if (USER$login == TRUE) {
      if(typeUser() == "expert"){
        #filter positions of validated articles by logged user
        print(EXPERTS_VALIDATIONS$df)
        validatedTitles <- EXPERTS_VALIDATIONS$df %>% filter(usernameId == input$userName) %>% select(position)
        VALIDATIONS$positions <- as.data.frame(validatedTitles)
        # Randomize titles to validate so different users validate articles in different order
        if (length(VALIDATIONS$positions > 0)){
          TITLES$userTitles <- sample(articles[-VALIDATIONS$positions$position,3])
        }
        else{
          TITLES$userTitles <- sample(articles$title) 
        }
      } #if type user expert
    }#if login true
    print(VALIDATIONS$positions)
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
        else{ #Usuario admin
          tabItems(
            tabItem(tabName = "manageEvalSummaries", class = "active",
                    fluidRow(
                      box(title="Tamaño de la muestra a validar", width = 12, solidHeader = TRUE,status = "primary",
                        box(width=6,
                        p("El tamaño actual es:"), verbatimTextOutput("currentSampleSize"),
                        sliderInput("sample",label='Seleccione el tamaño de la muestra a validar [en %]',
                                min=0, max=100, value = conf$sampleSize),
                        actionButton("saveSample", label= "Guardar",class="btn-primary"))
                        )),
                    fluidRow(
                      box(width = 12, title = "Gestión de ficheros",solidHeader = TRUE,status = "primary",
                          box(title="Fichero de artículos a validar", width = 6, 
                              p("Nombre del fichero actual:"),verbatimTextOutput("currentArticlesFile"),
                              radioButtons("changeArticlesFile",label="Desea cambiar el fichero de artículos a validar ",
                                           choices=list("Yes" = 1, "No" = 2),
                                           selected = 2),
                              conditionalPanel(
                                condition = "input.changeArticlesFile == 1",
                                fileInput("newArticlesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                          multiple = FALSE, accept = ".csv")
                              ),#conditional Panel 
                              actionButton("saveArticlesFile", label= "Guardar",class="btn-primary")
                          ),#box fichero de artículos a validar
                          box(title="Fichero de resúmenes a validar", width = 6, 
                              p("Nombre del fichero actual:"),verbatimTextOutput("currentSummariesFile"),
                              radioButtons("changeSummariesFile",label="Desea cambiar el fichero de resúmenes a validar ",
                                           choices=list("Yes" = 1, "No" = 2),
                                           selected = 2),
                              conditionalPanel(
                                condition = "input.changeSummariesFile == 1",
                                fileInput("newSummariesFile",label="Subir el nuevo fichero con los resúmenes a validar (solo csv)",
                                          multiple = FALSE, accept = ".csv")
                              ),#conditional Panel 
                              actionButton("saveSummariesFile", label= "Guardar",class="btn-primary")
                          ),#box fichero de resúmenes a validar
                          # box(title = "Nombre del fichero para las respuestas de los evaluadores", 
                          #     width = 6, #height = 320,
                          #     p("Nombre del fichero actual:"),verbatimTextOutput("currentFileNameResponses"),
                          #     textInput("responsesFileName",label="Ingrese el nombre del fichero donde se guardaran las respuestas de las 
                          #               validaciones para el nuevo fichero de resúmenes", 
                          #               value = conf$fileNameResponses)
                          # ), #box nombre fichero de respuestas
                      ))#big box and fluidRow
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
                      box(width = 12, dataTableOutput("results"))
                    )#fluidRow
            ),#tabItem dashboardEvalSummaries
            tabItem(tabName = "users",
                    h4("acá ver movimiento de los usuarios, alta de nuevo usuario"),
                    #fluidRow(
                      #column(width = 12, offset = 0,
                      plotOutput("usersValidations")
                    #))
            )#tabItem users
            
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
      "Welcome",
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
      nrow(articles) - nrow(distinct(VALIDATIONS$positions)),
      icon = icon("fal fa-edit"), #("glyphicon-edit", lib = "glyphicon"),
      color = "maroon"
    )
  })

  output$articleURL <- renderUI({
    HTML(paste0("<p><b>Para leer el artículo visite: </b><a href=",selectedArticleData()$url,' target="_blank">',selectedArticleData()$url,"</a></p>"))
  })
  
  output$text <- renderText(selectedArticleData()$text)
  
  output$generatedSummary <- renderText(filteredGeneratedSummary())

  #####
  # SAVE VALIDATION AND UPDATE TITLES LIST
  observeEvent(input$validateButton,{
    #create structure to save validation
    validation <- data.frame(matrix(ncol=numColEV,nrow=1))
    colnames(validation) <- expertsValidationsColNames
    #save values
    validation$usernameId <- USERNAME$name #input$userName
    validation$position <- which(articles$title == input$selectTitle)
    validation$question1 <- input$question1
    if(input$question1 == 2){
      validation$question2 <- NA
      validation$question3 <- NA
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

    #tryCatch
    #write.table(validation,file="data/expertsValidations.csv",append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    filePath <- file.path(tempdir(), FILENAMEEV$name) #"validations.csv")
    write.table(validation,file=filePath,append = TRUE,sep=',',row.names = FALSE,col.names = FALSE)
    drop_upload(filePath,outputDir) #"summariesApp/responses"

    shinyalert(title="Validation stored",type="success")

    #update select input title list pending validation by user
    position <- validation$position
    VALIDATIONS$positions <- append(VALIDATIONS$positions$position,position)
    VALIDATIONS$positions <- as.data.frame(VALIDATIONS$positions)
    colnames(VALIDATIONS$positions) <- c("position")
    TITLES$userTitles <- articles[-VALIDATIONS$positions$position,3]
    updateSelectInput(session,"selectTitle",choices=TITLES$userTitles)
  })

  # ######
  # #User Admin ---> No puede estar suelto
  # if(nrow(expertsValidations) > 0){
  #   validationsbyUserQ1 <- expertsValidations %>% select(position,question1,usernameId) %>%  distinct(position,usernameId, .keep_all = TRUE) %>% spread(usernameId,question1)
  #   print(validationsbyUserQ1)
  #   # aux <- validationsbyUserQ1 %>% select(-position)
  #   # numValid <- apply(X=!is.na(aux),MARGIN = 1, FUN= sum) #rowSums(!is.na(validationsbyUser))
  #   # numValid <- cbind(numValid,validationsbyUserQ1$position)
  #   # colnames(numValid)[2] <- "position"
  #   # tableAdmin <- left_join(adminArticles,as.data.frame(numValid),by=c("row_num"="position"))
  #   # tableAdmin <- left_join(tableAdmin,validationsbyUserQ1,by=c("row_num"="position"))
  #   # #colnames(tableAdmin)[5] <- "numberValidations"
  #   # #set.seed(123) en global
  #   # # krippTable <- data.matrix(tableAdmin[,-c(1:4)]) #all articles
  #   # # colnames(krippTable) <- NULL
  #   # # krippAgreement <- krippendorffs.alpha(krippTable, level = "nominal", control = list(bootit = 100, parallel = FALSE),verbose = TRUE) #validationbyCoder
  #   # # print(krippAgreement$alpha.hat)
  #   # # summary(krippAgreement)
  #   # 
  #   # validationsbyUserKripp <- data.matrix(aux) #(validationsbyUser[,-c(1)])
  #   # colnames(validationsbyUserKripp) <- NULL
  #   # # validationsbyUserKripp <- cbind(validationsbyUserKripp,validationsbyUserKripp[,1]) #Agrego otra columna igual a ver si mejora el coeficiente
  #   # krippAgreementValidated <- krippendorffs.alpha(validationsbyUserKripp,
  #   #                                                level = "nominal",
  #   #                                                control = list(bootit = 100, parallel = FALSE),
  #   #                                                verbose = TRUE) #validationbyCoder
  #   # print(krippAgreementValidated$alpha.hat)
  #   # #summary(krippAgreementValidated)
  # }
  # else{
  #   krippAgreementValidated <- NULL
  #   krippAgreementValidated$alpha.hat <- 0
  #   validationsbyUser <- data.frame(matrix(ncol=0,nrow=0))
  # }


######## Admin 

  output$currentSampleSize <- renderPrint({
    input$sample
  })

  output$currentSummariesFile <- renderPrint({
    SUMM_FILE$fileName
  })

  output$currentArticlesFile <- renderPrint({
    ARTIC_FILE$fileName
  })

  observeEvent(input$saveSample,{
    print("pasa por observe event save sample")
    conf$sampleSize <<- input$sample
    print(conf$sampleSize)
    filePath <- file.path(tempdir(),"conf.csv")
    write.table(conf,file=filePath,append = FALSE,sep=',',row.names = FALSE)
    drop_upload(filePath,inputDir)
    shinyalert(title="Configuración actualizada",type="success")
    print(conf)
  })

  observeEvent(input$saveSummariesFile,{
    print("pasa por observe event save summaries file")
    if(input$changeSummariesFile == 1){ #se podría verificar que tenga el formato correcto
      if(input$newSummariesFile$type == "text/csv"){
        conf$fileSummaries <<- input$newSummariesFile$name
        SUMM_FILE$fileName <- input$newSummariesFile$name
        filePathConf <- file.path(tempdir(),"conf.csv")
        write.table(conf,file=filePathConf,append = FALSE,sep=',',row.names = FALSE)
        drop_upload(filePathConf,inputDir)
        dir.create("tempdir")
        file.copy(input$newSummariesFile$datapath, file.path("tempdir",input$newSummariesFile$name))
        drop_upload(paste0("tempdir/",input$newSummariesFile$name),inputDir, mode = "overwrite") #no hace overwrite
        summaries <- loadCSV(inputDir,conf$fileSummaries)
        shinyalert(title="Configuración actualizada",type="success")
      }
      else{
        shinyalert(title="Error en el tipo de fichero seleccionado, no se subirá el nuevo fichero",type="error")
        message("No se puede cargar el fichero seleccionado porque no es de tipo csv")
      }
    }
    #print(conf)
    })

  observeEvent(input$saveArticlesFile,{
    print("pasa por observe event savearticles file")
    if(input$changeArticlesFile == 1){ #se podría verificar que tenga el formato correcto
      if(input$newArticlesFile$type == "text/csv"){
        conf$fileArticles <<- input$newArticlesFile$name
        ARTIC_FILE$fileName <- input$newArticlesFile$name
        filePathConf <- file.path(tempdir(),"conf.csv")
        write.table(conf,file=filePathConf,append = FALSE,sep=',',row.names = FALSE)
        drop_upload(filePathConf,inputDir)
        dir.create("tempdir")
        file.copy(input$newArticlesFile$datapath, file.path("tempdir",input$newArticlesFile$name))
        drop_upload(paste0("tempdir/",input$newArticlesFile$name),inputDir, mode = "overwrite") #no hace overwrite
        articles <- loadCSV(inputDir,conf$fileArticles)
        shinyalert(title="Configuración actualizada",type="success")
      }
      else{
        shinyalert(title="Error en el tipo de fichero seleccionado, no se subirá el nuevo fichero",type="error")
        message("No se puede cargar el fichero seleccionado porque no es de tipo csv")
      }
    }
    #print(conf)
  })

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
    print("pasa por render table admin")
    #no sé si leer, por si se conecta otro usuario......
    # validationsbyUser <- expertsValidations %>% select(position,error,username_id) %>% arrange(position)
    # spreadTest <- validationsbyUser %>%  spread(username_id, error)
    # validationsbyUser <- expertsValidations %>% select(position,error,username_id) %>%  spread(username_id, error)
    # tableAdmin <- left_join(adminArticles,validationsbyUser,by=c("row_num"="position"))
    #nrow(tableAdmin) #ok, todas las filas
    #datatable(tableAdmin[,-c(1)], options = list(autoWidth = TRUE,
                                                 #searching = FALSE))
  })

  # output$usersValidations <- renderPlot({
  #   #heatmapData <- tableAdmin[!is.na(tableAdmin$numValid),-c(1,3:5)]
  #   p <- expertsValidations %>% ggplot(aes(x=username_id,y=substr(title,1,20))) +
  #     geom_tile(aes(fill=as.factor(error))) +
  #     labs(x = "Expert username",y = "Title", title = "Tipo de error por resumen y usuario") #Summaries type of error by expert")
  #   p + scale_fill_discrete(name= "Error", labels=names(typeErrors))
  # })

}
