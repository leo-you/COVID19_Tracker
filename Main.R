# Load Library
library(leaflet)
library(leaflet.extras)
library(plotly)
library(tidyr)
library(tigris)
library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)

# Load Library

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


DownloadData_hospital <- function(){
  download.file(
    url = "https://github.com/COVID19Tracking/covid-tracking-data/archive/master.zip",
    destfile = "Data/covid19_hospital.zip"
  )
  
  data_path <- "covid-tracking-data-master/data/"
  
  unzip(
    zipfile = "Data/covid19_hospital.zip",
    files = paste0(data_path,c("states_daily_4pm_et.csv","states_current.csv","states_info.csv")),
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
    DownloadData_hospital()
    
  }
}

UpdateData()

# Function to download and update data as necessary

# Load Data
us_confirmed <- read.csv(file = "Data/time_series_covid19_confirmed_US.csv")
us_deaths <- read.csv(file = "Data/time_series_covid19_deaths_US.csv")

us_test <- read.csv(file = "Data/states_daily_4pm_et.csv")
states_info <- read.csv("Data/states_info.csv")

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


us_test_sub <- merge(us_test,states_info,by= "state",all.x = TRUE)
us_test_sub$date <- as.Date(as.character(us_test_sub$date),format="%Y%m%d")
us_test_evolution <- us_test_sub %>%
  mutate("total_pending_tested" = total,
         "total_tested" = totalTestResults,
         "name" = replace(as.character(name), name == 'District Of Columbia', 'District of Columbia') # Fix DC Naming mismatch in two data
         ) %>%
  select("name","date":"hospitalizedCurrently","inIcuCurrently","onVentilatorCurrently","recovered","total_pending_tested","total_tested") %>%
  arrange(name,date)

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


# state level data will include testing data from covid tracking project

state_daily_master_sub <- us_evolution %>%
  group_by(Province_State,date) %>%
  summarise("cum_confirmed" = sum(cum_confirmed,na.rm = T),
            "cum_deaths" = sum(cum_deaths,na.rm = T),
            "daily_confirmed" = sum(daily_confirmed,na.rm = T),
            "daily_deaths" = sum(daily_deaths,na.rm = T),
            "population" = sum(Population,na.rm = T))

state_daily_master_sub <- merge(state_daily_master_sub, us_test_evolution, by.x=c("Province_State","date"),
                            by.y=c("name","date"),all.x = TRUE)
  

state_daily_master <- state_daily_master_sub %>%
  replace(is.na(.), 0) %>%
  group_by(Province_State) %>%
  arrange(Province_State,date) %>%
  mutate(daily_recovered = ifelse(is.na(recovered - lag(recovered)),recovered,recovered - lag(recovered)),
         daily_positive = ifelse(is.na(positive - lag(positive)),positive,positive - lag(positive)),
         daily_negative = ifelse(is.na(negative - lag(negative)),negative,negative - lag(negative)),
         daily_tested_total = ifelse(is.na(total_tested - lag(total_tested)),total_tested,total_tested - lag(total_tested))
         )


# state level data will include testing data from codiv tracking project

us_evolution_us <- state_daily_master %>%
  group_by(date) %>%
  summarise("cum_confirmed" = sum(cum_confirmed,na.rm = T),
            "cum_deaths" = sum(cum_deaths,na.rm = T),
            "daily_confirmed" = sum(daily_confirmed,na.rm = T),
            "daily_deaths" = sum(daily_deaths,na.rm = T),
            "population" = sum(population,na.rm = T),
            "recovered" = sum(recovered,na.rm = T),
            "positive" = sum(positive,na.rm = T),
            "negative" = sum(negative,na.rm = T),
            "pending" = sum(pending,na.rm = T),
            "total_tested" = sum(total_tested,na.rm = T),
            "daily_recovered" = sum(daily_recovered,na.rm = T),
            "daily_positive" = sum(daily_positive,na.rm = T),
            "daily_negative" = sum(daily_negative,na.rm = T),
            "daily_tested_total" = sum(daily_tested_total,na.rm = T),
            "hospitalized" = sum(hospitalizedCurrently,na.rm = T),
            "icu" = sum(inIcuCurrently,na.rm = T),
            "ventilator" = sum(onVentilatorCurrently,na.rm = T),
            "positive_rate" = sum(positive,na.rm = T)/sum(total_tested,na.rm = T),
            "death_rate" = sum(cum_deaths,na.rm = T)/sum(cum_confirmed,na.rm = T)
            )

  

# Reshape Data

# Load shape file and customized labels for static choropleth map

states_shape <- states(cb=T)

latest_date <- max(us_evolution$date)

us_case_map_state <- state_daily_master %>%
  filter(date == latest_date) %>%
  mutate("confirmed_pop" = round(cum_confirmed * 1000000 / population,2),
         "deaths_pop" = round(cum_deaths * 1000000 / population,2),
         "tested_pop" = round((positive + negative) * 1000000 / population,2),
         "positive_rate" = round(positive / total_tested,2)) %>%
  filter(population > 0)

us_case_map_state$Province_State <- as.character(us_case_map_state$Province_State)

us_case_map_state <- subset(geo_join(states_shape,us_case_map_state,"NAME","Province_State",how = "inner"),cum_confirmed > 0)

bins_confirmed = c(0,quantile(us_case_map_state$cum_confirmed,0.2),
         quantile(us_case_map_state$cum_confirmed,0.4),
         quantile(us_case_map_state$cum_confirmed,0.6),
         quantile(us_case_map_state$cum_confirmed,0.8),
         quantile(us_case_map_state$cum_confirmed,1)
         )

pal_confirmed <- colorBin("YlOrRd", domain = us_case_map_state$cum_confirmed, bins = bins_confirmed)

us_confirmed_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g",
  us_case_map_state$NAME, us_case_map_state$cum_confirmed
) %>% lapply(htmltools::HTML)


bins_deaths = c(0,quantile(us_case_map_state$cum_deaths,0.2),
         quantile(us_case_map_state$cum_deaths,0.4),
         quantile(us_case_map_state$cum_deaths,0.6),
         quantile(us_case_map_state$cum_deaths,0.8),
         quantile(us_case_map_state$cum_deaths,1)
)

pal_deaths <- colorBin("YlOrRd", domain = us_case_map_state$cum_deaths, bins = bins_deaths)

us_deaths_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Deaths Cases: </strong>%g",
  us_case_map_state$NAME, us_case_map_state$cum_deaths
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
  us_case_map_state$NAME, us_case_map_state$cum_confirmed, paste0(round(us_case_map_state$population/1000000,2),"M"), us_case_map_state$confirmed_pop
) %>% lapply(htmltools::HTML)

bins_deaths_pop = c(0,quantile(us_case_map_state$deaths_pop,0.2),
                       quantile(us_case_map_state$deaths_pop,0.4),
                       quantile(us_case_map_state$deaths_pop,0.6),
                       quantile(us_case_map_state$deaths_pop,0.8),
                       quantile(us_case_map_state$deaths_pop,1)
)

pal_deaths_pop <- colorBin("Greys", domain = us_case_map_state$deaths_pop, bins = bins_deaths_pop)

us_deaths_pop_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Confirmed Cases: </strong>%g<br/><strong>Population: </strong>%s<br/><strong>Deaths/Population (in 1M): </strong>%g",
  us_case_map_state$NAME, us_case_map_state$cum_deaths, paste0(round(us_case_map_state$population/1000000,2),"M"), us_case_map_state$deaths_pop
) %>% lapply(htmltools::HTML)


bins_tested = c(0,quantile(us_case_map_state$total_tested,0.2),
                    quantile(us_case_map_state$total_tested,0.4),
                    quantile(us_case_map_state$total_tested,0.6),
                    quantile(us_case_map_state$total_tested,0.8),
                    quantile(us_case_map_state$total_tested,1)
)

pal_tested <- colorBin("Greens", domain = us_case_map_state$total_tested, bins = bins_tested)

us_tested_labels <- sprintf(
  "<strong>State: </strong>%s<br/><strong>Total Tested Cases: </strong>%g<br/><strong>Tested Positive: </strong>%s<br/><strong>Positive Rate: </strong>%s",
  us_case_map_state$NAME, us_case_map_state$total_tested, us_case_map_state$positive, paste0(us_case_map_state$positive_rate*100,"%")
) %>% lapply(htmltools::HTML)



# Load shape file and customized labels for static choropleth map

# Create static datatable


us_master <- state_daily_master %>%
  filter(date %in% c(latest_date,latest_date - 1)) %>%
  mutate(confirmed_growth = cum_confirmed/lag(cum_confirmed) - 1,
         deaths_growth = cum_deaths/lag(cum_deaths) - 1) %>%
  filter(date == latest_date & cum_confirmed > 0 & population > 0) %>%
  mutate("deaths_rate" = cum_deaths/cum_confirmed,
         "confirmed_pop" = round(cum_confirmed * 1000000 / population,2),
         "deaths_pop" = round(cum_deaths* 1000000/ population,2),
         population = round(population/1000000,2),
         "positive_rate" = round(positive/total_tested,2)) %>%
  arrange(desc(cum_confirmed)) %>%
  select(!date)


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
      th(rowspan = 2, 'Recovered'),
      th(rowspan = 2, 'Total Tested'),
      th(rowspan = 2, 'Positive'),
      th(rowspan = 2, 'Positive Rate'),
      th(rowspan = 2, 'Hospitalized'),
      th(rowspan = 2, 'In ICU'),
      th(rowspan = 2, 'on Ventilator')
    ),
    tr(
      lapply(rep(c('Count', 'Growth %'), 2), th)
    )
  )
))


full_table <- datatable(us_master[,c(1,2,20,3,21,22,6,23,24,13,15,7,25,10:12)],container = table_header,rownames = FALSE, 
                        extensions = c('FixedHeader',"Scroller","FixedColumns"),
          options = list(pageLength = nrow(us_master),
                         dom = 't',
                         scrollY = 650,
                         scrollX = TRUE,
                         scroller = TRUE,
                         fixedHeader = TRUE,
                         fixedColumns = TRUE
                         )
 ) %>%
  formatPercentage(c('confirmed_growth',"deaths_growth","deaths_rate","positive_rate"), 2) %>%
  formatStyle(
    'cum_confirmed',
    background = styleColorBar(us_master$cum_confirmed, color = "lightblue"),
    backgroundSize = '95% 80%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'cum_deaths',
    background = styleColorBar(us_master$cum_deaths, color = "lightgray"),
    backgroundSize = '95% 80%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) 

full_table

# Create static datatable

