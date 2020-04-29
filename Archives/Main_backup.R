library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(reshape2)
library(tidyr)
library(tigris)
library(DT)
library(dplyr)
library(dygraphs)
library(xts)
library(shiny)
library(shinydashboard)

source("Utils.R")

DownloadData <- function(){
  download.file(
    url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19.zip"
  )
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

  unzip(
    zipfile = "data/covid19.zip",
    files = paste0(data_path,c("confirmed_US.csv","deaths_US.csv",
                               "confirmed_global.csv","deaths_global.csv","recovered_global.csv")),
    exdir = "data",
    junkpaths = T
    
  )
}

DownloadData()

us_confirmed <- read.csv(file = "Data/time_series_covid19_confirmed_US.csv")
us_deaths <- read.csv(file = "Data/time_series_covid19_deaths_US.csv")


global_confirmed <- read.csv(file = "Data/time_series_covid19_confirmed_global.csv")
global_deaths <- read.csv(file = "Data/time_series_covid19_deaths_global.csv")
global_recovered <- read.csv(file = "Data/time_series_covid19_recovered_global.csv")

us_confirmed_sub <- us_confirmed %>%
  pivot_longer(names_to = "date",cols = 12:ncol(us_confirmed)) %>%
  group_by(UID,iso2,iso3,code3,FIPS,Admin2,Province_State,Country_Region,Lat,Long_,Combined_Key,date) %>%
  summarise("confirmed" = sum(value, na.rm = T))

us_confirmed_sub$date <- as.Date(substr(us_confirmed_sub$date,2,nchar("X1.26.20")),format = "%m.%d.%y")

us_deaths_sub <- us_deaths %>%
  pivot_longer(names_to = "date",cols = 13:ncol(us_deaths)) %>%
  group_by(UID,iso2,iso3,code3,FIPS,Admin2,Province_State,Country_Region,Lat,Long_,Combined_Key,Population,date) %>%
  summarise("deaths" = sum(value, na.rm = T))

us_deaths_sub$date <- as.Date(substr(us_deaths_sub$date,2,nchar("X1.26.20")),format = "%m.%d.%y")


us_evolution <- merge(us_confirmed_sub,us_deaths_sub, by = c(
  "UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key","date")
                      ) %>%
  select(Province_State:deaths) %>%
  arrange(Combined_Key,date) %>%
  group_by(Province_State,Country_Region,Lat,Long_,Combined_Key,Population,date) %>%
  summarise("cum_confirmed" = sum(confirmed,na.rm = T),
            "cum_deaths" = sum(deaths,na.rm = T)) %>%
  mutate(daily_confirmed = ifelse(is.na(cum_confirmed - lag(cum_confirmed)),cum_confirmed,cum_confirmed - lag(cum_confirmed)),
         daily_deaths = ifelse(is.na(cum_deaths - lag(cum_deaths)),cum_deaths,cum_deaths - lag(cum_deaths))) 


# us_confirmed_us <- us_confirmed_sub %>%
#   group_by(date) %>%
#   arrange(date) %>%
#   summarise("confirmed" = sum(confirmed, na.rm = T)) %>%
#   mutate(daily_confirmed = confirmed - lag(confirmed))
# 
# us_deaths_us <- us_deaths_sub %>%
#   group_by(date) %>%
#   summarise("deaths" = sum(deaths, na.rm = T)) %>%
#   mutate(daily_deaths = deaths - lag(deaths))

us_evolution_us <- us_evolution %>%
  group_by(date) %>%
  summarise("cum_confirmed" = sum(cum_confirmed, na.rm = T),
            "daily_confirmed" = sum(daily_confirmed, na.rm = T),
            "cum_deaths" = sum(cum_deaths, na.rm = T),
            "daily_deaths" = sum(daily_deaths, na.rm = T))

# us cumulative trend line

fig <- plot_ly(us_evolution_us, x = ~date) %>% 
  add_lines(y = ~cum_confirmed, name = 'Cum Cases') %>%
  add_lines(y = ~cum_deaths, name = "Cum Deaths") %>%
  layout(yaxis = list(tickformat = "0"),
         title = "US Cases Trend")
fig

# us cumulative trend line

# us daily case barchart

fig <- plot_ly(us_evolution_us, x = ~date) %>% 
  add_bars(y = ~daily_confirmed, name = 'Daily Cases') %>%
  add_bars(y = ~(daily_deaths * -1), name = "Daily Deaths",hoverinfo = ~-daily_deaths) %>%
  layout(yaxis = list(tickformat = "0"),
         title = "US Daily Cases Trend",
         barmode = "relative",legend = list(x = 0,y = 1))
fig

# us daily case barchart

# fig <- plot_ly(us_confirmed_us, x = ~date) %>% 
#   add_trace(y = ~confirmed, name = 'Cum Cases',mode = 'lines+markers') %>%
#   add_bars(y = ~daily_confirmed, name = "New Cases") %>%
#   layout(yaxis = list(tickformat = "0"),
#          title = "US confirmed Cases")
# fig
# 
# fig <- plot_ly(us_deaths_us, x = ~date) %>% 
#   add_trace(y = ~deaths, name = 'Cum deaths',mode = 'lines+markers') %>%
#   add_bars(y = ~daily_deaths, name = "New deaths") %>%
#   layout(yaxis = list(tickformat = "0"),
#          title = "US deaths Cases")
# fig

states_shape <- states(cb=T)

latest_date <- max(us_evolution$date)

us_case_map_state <- us_evolution %>%
  filter(date == latest_date) %>%
  group_by(Province_State) %>%
  summarise("confirmed" = sum(cum_confirmed,na.rm = T),
            "deaths" = sum(cum_deaths,na.rm = T),
            "population" = sum(Population, na.rm = T)) %>%
  mutate("confirmed_pop" = round(confirmed * 1000000 / population,2),
         "deaths_pop" = round(deaths * 1000000 / population,2)) %>%
  filter(population > 0)

us_case_map_state <- subset(geo_join(states_shape,us_case_map_state,"NAME","Province_State"),confirmed >0)



# latest_date_confirmed <- max(us_confirmed_sub$date)
# 
# us_confirmed_state <- us_confirmed_sub %>%
#   filter(date == latest_date_confirmed) %>% # data is cumulative
#   group_by(Province_State) %>%
#   summarise("confirmed" = sum(confirmed, na.rm = T))


# class(states_shape)
# 
# states_shape %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(popup=~NAME)
# 
# names(states_shape)

# us_confirmed_state_map <- geo_join(states_shape,us_confirmed_state,"NAME","Province_State")
# us_confirmed_state_map <- subset(us_confirmed_state_map, !is.na(confirmed))

bins_confirmed = c(0,quantile(us_case_map_state$confirmed,0.2),
         quantile(us_case_map_state$confirmed,0.4),
         quantile(us_case_map_state$confirmed,0.6),
         quantile(us_case_map_state$confirmed,0.8),
         quantile(us_case_map_state$confirmed,1)
         )

pal_confirmed <- colorBin("YlOrRd", domain = us_case_map_state$confirmed, bins = bins_confirmed)

us_confirmed_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g",
  us_case_map_state$NAME, us_case_map_state$confirmed
) %>% lapply(htmltools::HTML)

# latest_date_death <- max(us_deaths_sub$date)
# 
# us_deaths_state <- us_deaths_sub %>%
#   filter(date == latest_date_death) %>% # data is cumulative
#   group_by(Province_State) %>%
#   summarise("deaths" = sum(deaths, na.rm = T))
# 
# us_deaths_state_map <- geo_join(states_shape,us_deaths_state,"NAME","Province_State")
# us_deaths_state_map <- subset(us_deaths_state_map, !is.na(deaths))

bins_deaths = c(0,quantile(us_case_map_state$deaths,0.2),
         quantile(us_case_map_state$deaths,0.4),
         quantile(us_case_map_state$deaths,0.6),
         quantile(us_case_map_state$deaths,0.8),
         quantile(us_case_map_state$deaths,1)
)

pal_deaths <- colorBin("YlOrRd", domain = us_deaths_state_map$deaths, bins = bins_deaths)

us_deaths_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Deaths Cases: </strong>%g",
  us_deaths_state_map$NAME, us_deaths_state_map$deaths
) %>% lapply(htmltools::HTML)

map <- leaflet(us_case_map_state) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(
    fillColor = ~pal_confirmed(us_case_map_state$confirmed),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = us_confirmed_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Confirmed Case") %>%
  leaflet::addLegend(
            pal = pal_confirmed,
            values = us_case_map_state$confirmed, 
            position = "bottomright", 
            title = "US Confirmed Cases",
            labFormat = labelFormat(digits = 0),
            group = "Confirmed Case") %>%
  addPolygons(
    fillColor = ~pal_deaths(us_case_map_state$deaths),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = us_deaths_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Deaths") %>%
  leaflet::addLegend(
            pal = pal_deaths,
            values = us_case_map_state$deaths,
            position = "bottomright",
            title = "US Deaths Cases",
            labFormat = labelFormat(digits = 0),
            group = "Deaths") %>%
  addLayersControl(
    overlayGroups = c("Confirmed Case",
                   "Deaths"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Deaths")

map

bins_confirmed_pop = c(0,quantile(us_case_map_state$confirmed_pop,0.2),
                   quantile(us_case_map_state$confirmed_pop,0.4),
                   quantile(us_case_map_state$confirmed_pop,0.6),
                   quantile(us_case_map_state$confirmed_pop,0.8),
                   quantile(us_case_map_state$confirmed_pop,1)
)

pal_confirmed_pop <- colorBin("YlOrRd", domain = us_case_map_state$confirmed_pop, bins = bins_confirmed_pop)

us_confirmed_pop_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g<strong>Population: </strong>%s<br/><strong>Confirmed/Population (in 1M): </strong>%g",
  us_case_map_state$NAME, us_case_map_state$confirmed, us_case_map_state$population, us_case_map_state$confirmed_pop
) %>% lapply(htmltools::HTML)

bins_deaths_pop = c(0,quantile(us_case_map_state$deaths_pop,0.2),
                       quantile(us_case_map_state$deaths_pop,0.4),
                       quantile(us_case_map_state$deaths_pop,0.6),
                       quantile(us_case_map_state$deaths_pop,0.8),
                       quantile(us_case_map_state$deaths_pop,1)
)

pal_deaths_pop <- colorBin("YlOrRd", domain = us_case_map_state$deaths_pop, bins = bins_deaths_pop)

us_deaths_pop_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g<strong>Population: </strong>%s<br/><strong>Deaths/Population (in 1M): </strong>%g",
  us_case_map_state$NAME, us_case_map_state$deaths, us_case_map_state$population, us_case_map_state$deaths_pop
) %>% lapply(htmltools::HTML)


map <- leaflet(us_case_map_state) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(
    fillColor = ~pal_confirmed_pop(us_case_map_state$confirmed_pop),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = us_confirmed_pop_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Confirmed Case/Population (in 1M)") %>%
  leaflet::addLegend(
    pal = pal_confirmed_pop,
    values = us_case_map_state$confirmed_pop, 
    position = "bottomright", 
    title = "US Confirmed Cases",
    labFormat = labelFormat(digits = 0),
    group = "Confirmed Case/Population (in 1M)") %>%
  addPolygons(
    fillColor = ~pal_deaths_pop(us_case_map_state$deaths_pop),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = us_deaths_pop_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Deaths/Population (in 1M)") %>%
  leaflet::addLegend(
    pal = pal_deaths_pop,
    values = us_case_map_state$deaths_pop,
    position = "bottomright",
    title = "US Deaths Cases",
    labFormat = labelFormat(digits = 0),
    group = "Deaths/Population (in 1M)") %>%
  addLayersControl(
    overlayGroups = c("Confirmed Case/Population (in 1M)",
                      "Deaths/Population (in 1M)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Deaths/Population (in 1M)")

map


# us_confirmed_us <- us_confirmed_sub %>%
#   group_by(date) %>%
#   summarise("confirmed" = sum(confirmed, na.rm = T)) %>%
#   mutate(daily_new = confirmed - lag(confirmed))


us_case_map_city <- us_evolution %>%
  filter(date == latest_date) %>%
  filter(Lat != 0.00000) %>%
  group_by(Combined_Key,Lat,Long_) %>%
  summarise("confirmed" = sum(cum_confirmed,na.rm = T),
            "deaths" = sum(cum_deaths,na.rm = T))

leaflet(us_case_map_city) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>%
  addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~confirmed/100,
             radius = 10,blur = 10)

leaflet(us_case_map_city) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>%
  addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~deaths/100,
             radius = 10,blur = 10)



us_master <- us_evolution %>%
  filter(date %in% c(latest_date,latest_date - 1)) %>%
  group_by(Province_State,Combined_Key,date) %>%
  summarise("confirmed" = sum(cum_confirmed, na.rm = T),
            "deaths" = sum(cum_deaths, na.rm = T),
            "population" = mean(Population,na.rm = T)) %>%
  group_by(Province_State,date) %>%
  summarise("confirmed" = sum(confirmed, na.rm = T),
            "deaths" = sum(deaths, na.rm = T),
            "population" = round(sum(population, na.rm = T)/1000000,2)) %>%
  mutate(confirmed_growth = confirmed/lag(confirmed) - 1,
         deaths_growth = deaths/lag(deaths) - 1) %>%
  filter(!is.na(confirmed_growth) & !is.na(deaths_growth)) %>%
  filter(population > 0) %>%
  mutate("deaths_rate" = deaths/confirmed,
         "confirmed_pop" = round(confirmed / population,2),
         "deaths_pop" = round(deaths / population,2)) %>%
  arrange(desc(confirmed)) %>%
  select(Province_State,confirmed,confirmed_growth,deaths,deaths_growth,deaths_rate,population,confirmed_pop,deaths_pop)


# us_confirmed_growth <- us_confirmed_sub %>%
#   filter(date %in% c(latest_date_confirmed,latest_date_confirmed-1)) %>%
#   group_by(Province_State,date) %>%
#   summarise("confirmed" = sum(confirmed, na.rm = T)) %>%
#   mutate(confirmed_growth = confirmed/lag(confirmed) - 1) %>%
#   filter(!is.na(confirmed_growth)) %>%
#   select(Province_State,confirmed,confirmed_growth)
# 
# 
# us_deaths_growth <- us_deaths_sub %>%
#   filter(date %in% c(latest_date_death,latest_date_death-1)) %>%
#   group_by(Province_State,date) %>%
#   summarise("deaths" = sum(deaths, na.rm = T)) %>%
#   mutate(death_growth = deaths/lag(deaths) - 1) %>%
#   filter(!is.na(death_growth)) %>%
#   select(Province_State,deaths,death_growth)

# us_master <- merge(us_confirmed_growth,us_deaths_growth,by = "Province_State",all.x = TRUE)
# 
# us_master$death_rate <- us_master$deaths / us_master$confirmed
# 
# us_master <- us_master %>% arrange(desc(confirmed))

header = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'State'),
      th(colspan = 2, 'Confirmed'),
      th(colspan = 2, 'Deaths'),
      th(rowspan = 2, 'Death Rate %'),
      th(rowspan = 2, 'Population (M)'),
      th(rowspan = 2, 'Confirmed/1M'),
      th(rowspan = 2, 'Deaths/1M'),
    ),
    tr(
      lapply(rep(c('Count', 'Growth %'), 2), th)
    )
  )
))


full_table <- datatable(us_master,container = header,rownames = FALSE, 
                        extensions = c('FixedHeader',"Scroller"),
          options = list(pageLength = nrow(us_master),
                         deferRender = TRUE,
                         scrollY = 650,
                         scroller = TRUE
                         )
 ) %>%
  formatPercentage(c('confirmed_growth',"deaths_growth","deaths_rate"), 2) %>%
  formatStyle(
    'confirmed',
    background = styleColorBar(us_master$confirmed, color = "lightblue"),
    backgroundSize = '95% 80%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'deaths',
    background = styleColorBar(us_master$deaths, color = "lightgray"),
    backgroundSize = '95% 80%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) 

full_table

# Start here tomorrow

# us_daily_master <- merge(us_confirmed_us,us_deaths_us,by = "date",all.x = TRUE)

plot_ly(us_evolution_us, x = ~date, y = ~daily_confirmed, type = 'scatter', mode = 'lines',
        fill = 'tonexty', fillcolor='rgba(180,180,180,0.2)', line = list(color='rgb(0,100,80)',shape = "spline"),
        showlegend = TRUE, name = 'Daily New') %>%
  add_trace(y = ~daily_deaths, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'black',shape = "spline"),
            showlegend = TRUE, name = 'Daily Deaths') %>%
  layout(title = "US Daily Trend",
         paper_bgcolor='transparent', plot_bgcolor='transparent',
         xaxis = list(title = "Date",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      tickformat = '0',
                      zeroline = FALSE),
         legend = list(x = 0,y = 1))

# us_daily_master_ts = cbind(as.xts(us_daily_master$daily_confirmed,us_daily_master$date),
#                            as.xts(us_daily_master$daily_deaths,us_daily_master$date)
#                            )
# names(us_daily_master_ts)[1] <- "Daily Confirmed"
# names(us_daily_master_ts)[2] <- "Daily Deaths"
# 
# dygraph(us_daily_master_ts, main = "US Daily Trend")%>%
#   dyOptions(fillGraph = TRUE,fillAlpha = 0.4,colors = RColorBrewer::brewer.pal(3, "Set2"),
#             drawGrid = FALSE)%>%
#   dyRangeSelector(height = 40)%>%
#   dyHighlight(highlightCircleSize = 5)%>% 
#   dyLegend(width = 400)

# state_confirmed_date <- us_confirmed_sub %>%
#   group_by(Province_State,date) %>%
#   summarise(confirmed = sum(confirmed,na.rm = T))
# 
# state_deaths_date <- us_deaths_sub %>%
#   group_by(Province_State,date) %>%
#   summarise(deaths = sum(deaths,na.rm = T))
# 
# state_daily_master <- merge(state_confirmed_date, state_deaths_date, 
#                             by = c("Province_State","date"),all.x = TRUE)


state_daily_master <- us_evolution %>%
  group_by(Province_State,date) %>%
  summarise(confirmed = sum(cum_confirmed,na.rm = T),
            deaths = sum(cum_deaths,na.rm = T))

state_daily_top15_confirmed <- state_daily_master %>%
  arrange(date,desc(confirmed),Province_State) %>%
  group_by(date) %>%
  slice(1:15) %>%
  arrange(date,confirmed,Province_State) # reset level so that higher volume shows up first on the graph
  

fig <- state_daily_top15_confirmed %>%
  plot_ly(x = ~confirmed, y = ~Province_State, frame = ~ as.factor(date),
          type = 'bar', orientation = 'h', showlegend = FALSE, 
          marker = list(color = ~confirmed,
                        colors = "Set1",
                        line = list(color = "rgb(20, 20, 20)",
                                    width = 1))
          ) %>%
  layout(
    xaxis = list(
    tickformat = "0"
  ),
  yaxis = list(title = "",
               categoryorder = "array",
               categoryarray = ~confirmed),
  title = "Top 15 Confirmed by States"
) %>%
  animation_slider(
    currentvalue = list(prefix = "Date ")
  )

fig

state_daily_top15_deaths <- state_daily_master %>%
  arrange(date,desc(deaths),Province_State) %>%
  group_by(date) %>%
  slice(1:15) %>%
  arrange(date,deaths,Province_State)  %>% # reset level so that higher volume shows up first on the graph
  filter(!is.na(deaths))

fig <- state_daily_top15_deaths %>%
  plot_ly(x = ~deaths, y = ~Province_State, frame = ~ as.factor(date),
          type = 'bar', orientation = 'h', showlegend = FALSE, 
          marker = list(color = ~deaths,
                        line = list(color = "rgb(20, 20, 20)",
                                    width = 1))
  ) %>%
  layout(
    xaxis = list(
      tickformat = "0"
    ),
    yaxis = list(title = "",
                 categoryorder = "array",
                 categoryarray = ~deaths),
    title = "Top 15 Deaths by States"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Date ")
  )

fig

