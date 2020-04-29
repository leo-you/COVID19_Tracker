source("Main.R")

body_timeline <- dashboardBody(
  fluidRow(
    column(
      12,h3("Key Figures:")
      ,fluidRow(
        valueBoxOutput("New_Confirmed",width = 3),
        valueBoxOutput("Total_Confirmed",width = 3),
        valueBoxOutput("New_Deaths",width = 3),
        valueBoxOutput("Total_Deaths",width = 3),

      )
    )
  ),
  fluidRow(
    # box("Confirmed Case Map",width = 8, height = 600,
    #     leafletOutput("map")),
    # box("Summary Table",width = 4, height = 600,
    #     DTOutput("summary_table"))
    column(
      width = 6,h3("Case Map"),
      radioButtons("heatmap_type","Map Type",choices = list("Confirmed Cases" = 1,
                                                       "Death Cases" = 2,
                                                       "Confirmed/1M" = 3,
                                                       "Deaths/1M" = 4),
                  selected = 1,inline = TRUE),
      leafletOutput("heatmap",height = 500)
    ),
    
    tabBox(
      width = 6,title = "Summary Table", height = 600,
      tabPanel("By Case", DTOutput("table_case",height = 450)),
      tabPanel("By Case/Population", DTOutput("table_population",height = 450))
      
    )
  ),
  fluidRow(
    column(12,align = "center",
      sliderInput("Date_Slider",label = "Select Date: ",min = min(us_evolution$date),
                  max = max(us_evolution$date),value = latest_date,width = "90%",
                  timeFormat="%d-%m-%Y",animate = animationOptions(playButton = "Play",loop = FALSE))
    )
  )
)


page_timeline <- dashboardPage(
  title = "Overview",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = body_timeline
)


