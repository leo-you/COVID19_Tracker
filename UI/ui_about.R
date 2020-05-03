
body_about <- dashboardBody(
  fluidRow(
    column(
      box(
        title = div(title = "About this project",style = "padding-left: 20px",class = "h3"),
        column(
          tags$h3("About this Project"),
          tags$br(),
          tags$h4("Background"),
          "This dashboard demonstrates the recent COVID-19 pandemic that's happened in the US, from its first case in the state of Washington till most recent.
          This dashboard allows the user to follow the timeline of the outbreak in each state of the US, the new cases and deaths cases, interactively. It also
          includes US COVID-19 testing data and hospitalized data from the COVID Tracking project.",
          tags$br(),
          tags$h4("Data Source"),
          "This app uses data from the following sources:",
          tags$li(tags$a("Johns Hopkins Data",href = "https://github.com/CSSEGISandData/COVID-19")),
          tags$li(tags$a("1point3acres",href = "https://coronavirus.1point3acres.com")),
          tags$li(tags$a("The COVID Tracking Project",href = "https://covidtracking.com")),
          tags$br(),
          "This app is developed in R Studio using Shiny and was inspired by",
          tags$a("this video.",href = "https://www.youtube.com/watch?v=jr6YLRJbJ5M"),
          tags$br(),
          tags$h4("Disclaimer"),
          "This tracker is a personal project and for education purpose only. The data and analysis only represent the creater's own
          opinion and not affiliate with any other institution or parties. The source code can be found on Github",
          tags$a("COVID19_Tracker",href = "https://github.com/leo-you/COVID19_Tracker"),
          tags$br(),
          tags$h4("Creator"),
          "Leo You",
          tags$br(),
          tags$a("Linkedin",href = "https://www.linkedin.com/in/leoyouumd/"),
          "|",
          tags$a("Github",href = "https://github.com/leo-you"),
          "|",
          tags$a("Portfolio",href = "https://leoyou.netlify.com/"),
          tags$br(),
          tags$br(),
          width = 12
        ),
        width = 6,
      ),
      width = 12,
      style = "padding: 15px"
    )
  )
)



page_about <- dashboardPage(
  
  title = "About",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = body_about
)