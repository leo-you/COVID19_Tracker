source("Main.R")

body_overview <- dashboardBody(
  fluidRow(
    box(
      width = 7, height = 900,
      h3(paste0("Last Updated: ",latest_date),
                  class = "box-title",style = "font-size: 15px"),
           DTOutput("full_table")
    ),
    column(
      5,h3("State Case Map"),
      radioButtons("choroplethmap_type","Map Type",choices = list("Confirmed Cases" = 1,
                                                            "Death Cases" = 2,
                                                            "Confirmed/1M" = 3,
                                                            "Deaths/1M" = 4,
                                                            "Tested" = 5),
                   selected = 1,inline = TRUE),
      leafletOutput("choropleth_map",height = "700px")
    )
  )
)


page_overview <- dashboardPage(
  title = "Overview",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = body_overview
)
