
body_about <- dashboardBody(
  fluidRow(
    column(
      box(
        title = div(title = "About this project",style = "padding-left: 20px",class = "h3"),
        column(
          tags$h3("About this Project"),
          "This dashboard demonstrates the recent COVID-19 pandemic that's happened in the US, from its first case in the state of Washington till most recent.
          This dashboard allows the user to follow the timeline of the outbreak in each state of the US, the new cases and deaths cases, interactively. This app uses
          data from Johns Hopkins data and was inspired by ",
          tags$a("this video.",href = "https://www.youtube.com/watch?v=jr6YLRJbJ5M"),
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