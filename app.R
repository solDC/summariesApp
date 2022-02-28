library(shiny)
library(shinyauthr)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(tidyverse)

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

credentials <- read.csv(file="data/users.csv")

text <- read.csv("data/summaries.csv")
#text$title <- as.factor(text$title)
str(text)
numChoicesSummVal <- text %>% filter(numValid < 3) %>%  count()
choicesSummVal <- text %>% filter(numValid < 3)  %>% select(title) 

header <- dashboardHeader( title = "Summaries Validation", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 

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
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("far fa-sign-out",lib = "font-awesome"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt",lib = "font-awesome")),
        menuItem("Validate", tabName = "second", icon = icon("th",lib = "font-awesome"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        # First tab
        tabItem(tabName ="dashboard", class = "active",
                fluidRow(
                  box(width = 12, dataTableOutput('results'))
                )),
        
        # Second tab
        tabItem(tabName = "second",
                fluidRow(
                  selectInput("selectTitle",label=("Select a text to validate"),choices = choicesSummVal),
                  # box(width = 12, textOutput('title')),
                  # box(width = 12, textOutput('body')),
                  # box(width = 12, textOutput('summary'))
                  tableOutput('contents')
                )
        ))
      
    }
    else {
      loginpage
    }
  })
  

  output$results <-  DT::renderDataTable({
    datatable(text, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })

  
  filteredText<- reactive ({
    text %>% select(title,body,summary) %>% filter(grepl(input$selectTitle,title))
  })

  # filteredTitle <- reactive ({
  #   text %>% select(title) %>% filter(grepl(input$selectTitle,title))
  # })
  # 
  # filteredText <- reactive ({
  #   text %>% select(text) %>% filter(grepl(input$selectTitle,title))
  # })
  # 
  # filteredSummary <- reactive ({
  #   text %>% select(summary) %>% filter(grepl(input$selectTitle,title))
  # })

  output$contents <- renderTable(
    filteredText()
  )
  
  # output$title<- renderText({
  #   filteredTitle()
  # })
  # 
  # output$body <- renderText({
  #   filteredText()
  # })
  # 
  # output$summary <- renderText({
  #   filteredSummary()
  # })

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)