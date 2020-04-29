# Load Library
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

# Load Library

source("Utils.R")

# Function to download and update data as necessary

DownloadData <- function(){
  download.file(
    url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "Data/covid19.zip"
  )
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

  unzip(
    zipfile = "Data/covid19.zip",
    files = paste0(data_path,c("confirmed_US.csv","deaths_US.csv",
                               "confirmed_global.csv","deaths_global.csv","recovered_global.csv")),
    exdir = "Data",
    junkpaths = T
    
  )
}

UpdateData <- function(){
  T_refresh = 12 # hours
  if (!dir.exists("Data")) {
    dir.create("Data")
    DownloadData()
  } else if ((!file.exists("Data/covid19.zip")) || as.double(Sys.time() - file.info("Data/covid19.zip")$ctime,units = "hours" ) > T_refresh) {
    DownloadData()
    
    
  }
}

UpdateData()

# Function to download and update data as necessary

# Load Data
us_confirmed <- read.csv(file = "Data/time_series_covid19_confirmed_US.csv")
us_deaths <- read.csv(file = "Data/time_series_covid19_deaths_US.csv")


global_confirmed <- read.csv(file = "Data/time_series_covid19_confirmed_global.csv")
global_deaths <- read.csv(file = "Data/time_series_covid19_deaths_global.csv")
global_recovered <- read.csv(file = "Data/time_series_covid19_recovered_global.csv")

# Load Data

# Reshape Data

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


us_evolution_us <- us_evolution %>%
  group_by(date) %>%
  summarise("cum_confirmed" = sum(cum_confirmed, na.rm = T),
            "daily_confirmed" = sum(daily_confirmed, na.rm = T),
            "cum_deaths" = sum(cum_deaths, na.rm = T),
            "daily_deaths" = sum(daily_deaths, na.rm = T))

# Reshape Data

# Load shape file and customized labels for static choropleth map

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


bins_deaths = c(0,quantile(us_case_map_state$deaths,0.2),
         quantile(us_case_map_state$deaths,0.4),
         quantile(us_case_map_state$deaths,0.6),
         quantile(us_case_map_state$deaths,0.8),
         quantile(us_case_map_state$deaths,1)
)

pal_deaths <- colorBin("YlOrRd", domain = us_case_map_state$deaths, bins = bins_deaths)

us_deaths_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Deaths Cases: </strong>%g",
  us_case_map_state$NAME, us_case_map_state$deaths
) %>% lapply(htmltools::HTML)

bins_confirmed_pop = c(0,quantile(us_case_map_state$confirmed_pop,0.2),
                   quantile(us_case_map_state$confirmed_pop,0.4),
                   quantile(us_case_map_state$confirmed_pop,0.6),
                   quantile(us_case_map_state$confirmed_pop,0.8),
                   quantile(us_case_map_state$confirmed_pop,1)
)

pal_confirmed_pop <- colorBin("Blues", domain = us_case_map_state$confirmed_pop, bins = bins_confirmed_pop)

us_confirmed_pop_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g<br/><strong>Population: </strong>%s<br/><strong>Confirmed/Population (in 1M): </strong>%g",
  us_case_map_state$NAME, us_case_map_state$confirmed, us_case_map_state$population, us_case_map_state$confirmed_pop
) %>% lapply(htmltools::HTML)

bins_deaths_pop = c(0,quantile(us_case_map_state$deaths_pop,0.2),
                       quantile(us_case_map_state$deaths_pop,0.4),
                       quantile(us_case_map_state$deaths_pop,0.6),
                       quantile(us_case_map_state$deaths_pop,0.8),
                       quantile(us_case_map_state$deaths_pop,1)
)

pal_deaths_pop <- colorBin("Greens", domain = us_case_map_state$deaths_pop, bins = bins_deaths_pop)

us_deaths_pop_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g<br/><strong>Population: </strong>%s<br/><strong>Deaths/Population (in 1M): </strong>%g",
  us_case_map_state$NAME, us_case_map_state$deaths, us_case_map_state$population, us_case_map_state$deaths_pop
) %>% lapply(htmltools::HTML)

# Load shape file and customized labels for static choropleth map

# Create static datatable

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


table_header = htmltools::withTags(table(
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


full_table <- datatable(us_master,container = table_header,rownames = FALSE, 
                        extensions = c('FixedHeader',"Scroller","FixedColumns"),
          options = list(pageLength = nrow(us_master),
                         deferRender = TRUE,
                         scrollY = 650,
                         scroller = TRUE,
                         fixedHeader = TRUE,
                         fixedColumns = TRUE
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
# Create static datatable


state_daily_master <- us_evolution %>%
  group_by(Province_State,date) %>%
  summarise(confirmed = sum(cum_confirmed,na.rm = T),
            deaths = sum(cum_deaths,na.rm = T))

