library(shiny)
library(shinyauthr)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- fluidPage(
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("sidebarpanel"),
  
  # Plot to show user info after login
  plotOutput("distPlot")
  
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    tagList(
      # Sidebar with a slider input
      column(width = 4,
             sliderInput("obs",
                         "Number of observations:",
                         min = 0,
                         max = 1000,
                         value = 500)
      ),
      
      column(width = 4,
             p(paste("You have", credentials()$info[["permissions"]],"permission"))
      )
    )
    
  })
  
  # Plot
  output$distPlot <- renderPlot({
    
    # Show plot only when authenticated
    req(credentials()$user_auth)
    
    if(!is.null(input$obs)) {
      hist(rnorm(input$obs)) 
    }
    
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
