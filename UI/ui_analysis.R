source("Main.R")

body_analysis <- dashboardBody(
  fluidRow(
    column(
      6,h3("US Daily Trend"),
      plotlyOutput("daily_trend",height = "300px")
    ),
    column(
      6,h3("US Cumulative Cases"),
      plotlyOutput("cum_trend",height = "300px"),
      checkboxInput("log_y","Log y axis",value = FALSE)
      
    )
  ),
  fluidRow(
    column(
      6,h3("State Trend"),
      selectInput("state_filter", h3("Select States"), 
                  choices = unique(as.character(state_daily_master$Province_State)), 
                  selected = unique(as.character(state_daily_master$Province_State))[1]),
      selectInput("variable_choice", h3("Select Variable"), 
                  choices = c("Cumulative" = 1,"Daily" = 2), 
                  selected = 1),
      
      plotlyOutput("state_trend",height = "300px")
      
    ),
    column(
      6,h3("Top States Trend"),
      selectInput("confirmed_deaths", h3("Select from below"), 
                  choices = c("Confirmed" = 1,"Deaths" = 2), 
                  selected = 1),
      numericInput("top_n",label = "Top: ",value = 5,min = 5,max = 20,step = 1),
      plotlyOutput("top_states",height = "300px")
    )
  )  
)



page_analysis <- dashboardPage(
  title = "Abalysis",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = body_analysis
)
