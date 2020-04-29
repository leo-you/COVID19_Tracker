source("UI/ui_timeline.R",local = TRUE)
source("UI/ui_overview.R",local = TRUE)
source("UI/ui_analysis.R",local = TRUE)
source("UI/ui_about.R",local = TRUE)

ui <- fluidPage(
  title = "COVID-19 Tracker",
  navbarPage(
    title = div("COVID-19 Tracker", style = "padding-left: 10px"),
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel("Timeline",page_timeline,value = "page-timeline"),
    tabPanel("Overview",page_overview,value = "page-overview"),
    tabPanel("Analysis",page_analysis,value = "page-analysis"),
    tabPanel("About",page_about,value = "page-about")
  )
  
  
)
