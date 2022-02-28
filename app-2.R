library(shiny)
library(shinyauthr)
library(shinydashboard)
library(DT) #Data objects in R can be rendered as HTML tables using the JavaScript library 'DataTables' (typically via R Markdown or Shiny). The 'DataTables' library has been included in this R package. The package name 'DT' is an abbreviation of 'DataTables'.
library(shinyjs)


# dataframe that holds usernames, passwords and other user data

credentials <- read.csv('data/users.tsv',stringsAsFactors = F)

header <- dashboardHeader(title="Summaries validation") #,uiOutput("logoutbtn")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui <- dashboardPage(header, sidebar, body, skin = "blue")  
  
# # logout button
# div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
# 
# # login section
# shinyauthr::loginUI(id = "login"),
# 
# # Sidebar to show user info after login
# uiOutput("sidebarpanel"),
# 
# # Plot to show user info after login
# plotOutput("distPlot")


server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  if (interactive()) {
    
    # Create a Font Awesome `html_dependency`
    fa_html_dependency()
    
  }
  
  
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  

  output$sidebarpanel <- renderUI({
    if (credentials$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Second Page", tabName = "second", icon = icon("th"))
      )
    }
  })
  
  output$distPlot <- renderPlot({
    
    # Show plot only when authenticated
    req(credentials()$user_auth)
    
    if(!is.null(input$obs)) {
      hist(rnorm(input$obs)) 
    }
    
  })

  
}

shinyApp(ui = ui, server = server)
