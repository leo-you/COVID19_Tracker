source("Main.R")

body_analysis <- dashboardBody(
  fluidRow(
    column(
      6,h3("US Daily Trend"),
      plotlyOutput("daily_trend",height = "400px")
    ),
    column(
      6,h3("US Cumulative Cases"),
      plotlyOutput("cum_trend",height = "400px"),
      checkboxInput("log_y","Log y axis",value = FALSE)
      
    )
  ),
  fluidRow(
    column(
      6,h3("State Trend"),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          selectInput("state_filter", "Select States",
                      choices = unique(as.character(state_daily_master[state_daily_master$confirmed > 0,]$Province_State)),
                      selected = unique(as.character(state_daily_master[state_daily_master$confirmed > 0,]$Province_State))[1])),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          selectInput("variable_choice", "Select Variable", 
                      choices = c("Cumulative" = 1,"Daily" = 2), 
                      selected = 1)),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          selectInput("start_date", "Plot start from:",
                      choices = list("1st Case" = 1,
                                     "100th Case" = 2,
                                     "100th Death" = 3),
                      selected = 1)),

      plotlyOutput("state_trend",height = "400px")
      
    ),
    column(
      6,h3("Top States Trend"),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          selectInput("confirmed_deaths", "Select variable: ", 
                      choices = c("Confirmed" = 1,"Deaths" = 2), 
                      selected = 1)),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput("top_n",label = "Top: ",value = 10,min = 5,max = 20,step = 1)),

      plotlyOutput("top_states",height = "400px")
    )
  )  
)



page_analysis <- dashboardPage(
  title = "Abalysis",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = body_analysis
)
