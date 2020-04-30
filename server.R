source("Main.R")

server <- function(input, output) {
  

  
  # summary table reactive functions
  # input$Date_Slider
  us_master_filtered <- reactive({
    
    us_evolution %>%
      filter(date %in% c(input$Date_Slider,input$Date_Slider-1)) %>%
      group_by(Province_State,Combined_Key,date) %>%
      summarise("confirmed" = sum(cum_confirmed, na.rm = T),
                "deaths" = sum(cum_deaths, na.rm = T),
                "population" = mean(Population,na.rm = T)) %>%
      group_by(Province_State,date) %>%
      summarise("confirmed" = sum(confirmed, na.rm = T),
                "deaths" = sum(deaths, na.rm = T),
                "population" = round(sum(population, na.rm = T)/1000000,2)) %>%
      mutate(confirmed_growth = ifelse(lag(confirmed) == 0,0,confirmed/lag(confirmed) - 1),
             deaths_growth = ifelse(lag(deaths) == 0,0,deaths/lag(deaths) - 1)) %>%
      filter(date == input$Date_Slider) %>%
      filter(population > 0 & confirmed > 0) %>%
      mutate("deaths_rate" = deaths/confirmed,
             "confirmed_pop" = round(confirmed / population,2),
             "deaths_pop" = round(deaths / population,2)) %>%
      arrange(desc(confirmed)) %>%
      select(Province_State,confirmed,confirmed_growth,deaths,deaths_growth,deaths_rate,population,confirmed_pop,deaths_pop)
    
    
    })
  
  output$table_case = DT::renderDataTable(isolate(
    
    datatable(us_master_filtered()[,c(1:6)],rownames = FALSE,
              colnames = c('State', 'Confirmed Cases', 'Confirmed Growth', 'Deaths Cases', 'Deaths Growth',"Death Rate"),
              extensions = c('FixedHeader',"Scroller"),
              options = list(pageLength = nrow(us_master_filtered()),
                             deferRender = TRUE,
                             scrollY = 400,
                             scroller = TRUE
              )
    ) %>%
      formatPercentage(c('confirmed_growth',"deaths_growth","deaths_rate"), 2) %>%
      formatStyle(
        'confirmed',
        background = styleColorBar(us_master_filtered()$confirmed, color = "lightblue"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'deaths',
        background = styleColorBar(us_master_filtered()$deaths, color = "lightgray"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) 
  ))
  
  
  proxy_table_case = dataTableProxy('table_case')

  observe({
    replaceData(proxy_table_case, us_master_filtered()[,c(1:6)], resetPaging = FALSE,clearSelection = "all",rownames = FALSE)
  })
  
  
  
  output$table_population = renderDT({isolate(
    
    datatable(us_master_filtered()[,c(1,7:9)],rownames = FALSE, 
              colnames = c('State', 'Population', 'Confirmed Cases/Population (1M)', 'Deaths Cases/Population (1M)'),
              extensions = c('FixedHeader',"Scroller"),
              options = list(pageLength = nrow(us_master_filtered()),
                             deferRender = TRUE,
                             scrollY = 400,
                             scroller = TRUE
              )
    ) %>%
      formatStyle(
        'confirmed_pop',
        background = styleColorBar(us_master_filtered()$confirmed_pop, color = "lightblue"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'deaths_pop',
        background = styleColorBar(us_master_filtered()$deaths_pop, color = "lightgray"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) 
  )
  })
  
  proxy_table_population = dataTableProxy('table_population')
  
  observe({
    replaceData(proxy_table_population, us_master_filtered()[,c(1,7:9)], resetPaging = FALSE,clearSelection = "all",rownames = FALSE)
  })
  
  
  
  output$full_table = renderDT({full_table})
  
  
  output$choropleth_map = renderLeaflet({
    if (input$choroplethmap_type == 1) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
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
            direction = "auto")) %>%
        leaflet::addLegend(
          data = us_case_map_state,
          pal = pal_confirmed,
          values = us_case_map_state$confirmed, 
          position = "bottomright", 
          title = "US Confirmed Cases",
          labFormat = labelFormat(digits = 0))
    } else if (input$choroplethmap_type == 2) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
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
            direction = "auto")) %>%
        leaflet::addLegend(
          data = us_case_map_state,
          pal = pal_deaths,
          values = us_case_map_state$deaths,
          position = "bottomright",
          title = "US Deaths Cases",
          labFormat = labelFormat(digits = 0))
    } else if (input$choroplethmap_type == 3) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
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
            direction = "auto")) %>%
        leaflet::addLegend(
          data = us_case_map_state,
          pal = pal_confirmed_pop,
          values = us_case_map_state$confirmed_pop, 
          position = "bottomright", 
          title = "US Confirmed Cases/Population 1M",
          labFormat = labelFormat(digits = 0))
    } else if (input$choroplethmap_type == 4) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
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
            direction = "auto")) %>%
        leaflet::addLegend(
          data = us_case_map_state,
          pal = pal_deaths_pop,
          values = us_case_map_state$deaths_pop,
          position = "bottomright",
          title = "US Deaths Cases/Population 1M",
          labFormat = labelFormat(digits = 0))
    }
    
    
  })


    # summary table reactive functions

  state_daily_master_filtered <- reactive({
    
    if (input$start_date == 1) {
      us_evolution %>%
        filter(Province_State == input$state_filter) %>%
        group_by(Province_State,date) %>%
        summarise(confirmed = sum(cum_confirmed,na.rm = T),
                  deaths = sum(cum_deaths,na.rm = T),
                  daily_confirmed = sum(daily_confirmed,na.rm = T),
                  daily_deaths = sum(daily_deaths,na.rm = T)) %>%
        filter(confirmed > 0)
    } else if (input$start_date == 2) {
      us_evolution %>%
        filter(Province_State == input$state_filter) %>%
        group_by(Province_State,date) %>%
        summarise(confirmed = sum(cum_confirmed,na.rm = T),
                  deaths = sum(cum_deaths,na.rm = T),
                  daily_confirmed = sum(daily_confirmed,na.rm = T),
                  daily_deaths = sum(daily_deaths,na.rm = T)) %>%
        filter(confirmed >= 100)
    } else if (input$start_date == 3) {
      us_evolution %>%
        filter(Province_State == input$state_filter) %>%
        group_by(Province_State,date) %>%
        summarise(confirmed = sum(cum_confirmed,na.rm = T),
                  deaths = sum(cum_deaths,na.rm = T),
                  daily_confirmed = sum(daily_confirmed,na.rm = T),
                  daily_deaths = sum(daily_deaths,na.rm = T)) %>%
        filter(deaths >= 100)
    }
  })
  
  
  # Heatmap reactive functions
  
  
  us_case_map_city_filtered <- reactive({
    us_evolution %>%
      filter(date == input$Date_Slider) %>%
      filter(Lat != 0.00000) %>%
      group_by(Combined_Key,Lat,Long_) %>%
      summarise("confirmed" = sum(cum_confirmed,na.rm = T),
                "deaths" = sum(cum_deaths,na.rm = T),
                "population" = sum(Population,na.rm = T)) %>%
      mutate(confirmed_pop = round(confirmed/population,2),
             deaths_pop = round(deaths/population,2))%>%
      filter(confirmed > 0)
    
  })
  
  
  output$heatmap = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",group = "light mode") %>%
      addProviderTiles("CartoDB.DarkMatter",group = "dark mode") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("light mode", "dark mode"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    
    if (input$heatmap_type == 1) {
      leafletProxy("heatmap", data = us_case_map_city_filtered()) %>%
        clearHeatmap() %>%
        addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~confirmed/100,
                   radius = 10,blur = 10)
    } else if (input$heatmap_type == 2) {
      leafletProxy("heatmap", data = us_case_map_city_filtered()) %>%
        clearHeatmap() %>%
        addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~deaths/100,
                   radius = 10,blur = 10)
    } else if (input$heatmap_type == 3) {
      leafletProxy("heatmap", data = us_case_map_city_filtered()) %>%
        clearHeatmap() %>%
        addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~confirmed_pop * 1000000,
                   radius = 10,blur = 10)
    } else if (input$heatmap_type == 4) {
      leafletProxy("heatmap", data = us_case_map_city_filtered()) %>%
        clearHeatmap() %>%
        addHeatmap(lng = ~Long_,lat = ~Lat,intensity = ~deaths_pop * 1000000,
                   radius = 10,blur = 10)
    }
  })
  
  # Heatmap reactive functions
  
  # Key Figures outputs
  
  output$date_selected = renderUI({
    tagList(
      helpText(paste0("As of: ",input$Date_Slider)),
      br(),
      valueBox(us_evolution_us$daily_confirmed[us_evolution_us$date == input$Date_Slider], "New Cases", 
               icon = icon("ambulance", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(us_evolution_us$cum_confirmed[us_evolution_us$date == input$Date_Slider], "Total Cases", 
               icon = icon("hospital", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(us_evolution_us$daily_deaths[us_evolution_us$date == input$Date_Slider], "New Deaths", 
               icon = icon("heartbeat", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(us_evolution_us$cum_deaths[us_evolution_us$date == input$Date_Slider], "Total Deaths", 
               icon = icon("first-aid", lib = "font-awesome"),
               color = "yellow",width = 3),
      br(),
      helpText(paste0("last updated: ",latest_date))
    )
  })


  # Key Figures outputs
  
  
  # Analysis plots

  output$daily_trend <- renderPlotly({
    
    plot_ly(us_evolution_us, x = ~date) %>% 
      add_bars(y = ~daily_confirmed, name = 'Daily Cases') %>%
      add_bars(y = ~(daily_deaths * -1), name = "Daily Deaths",hoverinfo = ~-daily_deaths) %>%
      layout(yaxis = list(tickformat = "0"),
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             title = "US Daily Cases Trend",
             barmode = "relative",legend = list(x = 0,y = 1))
    
  })
  
  output$cum_trend <- renderPlotly({
    if (input$log_y == FALSE ) {
      plot_ly(x = ~date) %>% 
        add_lines(data =us_evolution_us, y = ~cum_confirmed, name = "Cum Cases") %>%
        add_lines(data =us_evolution_us, y = ~cum_deaths, name = "Cum Deaths") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = "US Cumulative Cases",
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1))
      
    } else {
      plot_ly(x = ~date) %>% 
        add_lines(data =us_evolution_us, y = ~log10(cum_confirmed), name = "Log Cases") %>%
        add_lines(data =us_evolution_us, y = ~log10(cum_deaths), name = "Log Deaths") %>%
        layout(yaxis = list(tickformat = "0",title = "Log"),
               title = "US Cumulative Cases",
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1))
    }
  })
  
  output$state_trend <- renderPlotly({
    
    if (input$variable_choice == 1) {
      plot_ly(state_daily_master_filtered(),x = ~date) %>%
        add_lines(y = ~confirmed, name = "Cum Cases") %>%
        add_lines(y = ~deaths, name = "Cum Deaths") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0(input$state_filter," Cumulative Trend"),
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1))
    } else {
      
      plot_ly(state_daily_master_filtered(),x = ~date) %>%
        add_bars(y = ~daily_confirmed, name = "Daily New") %>%
        add_bars(y = ~(daily_deaths * -1), name = "Daily Deaths") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0(input$state_filter," Daily Trend"),
               paper_bgcolor='transparent', plot_bgcolor='transparent', barmode = "relative",
               legend = list(x = 0,y = 1))
    }
  })
  
  state_daily_top_confirmed <- reactive({
    state_daily_master %>%
    arrange(date,desc(confirmed),Province_State) %>%
    filter(date >= (latest_date - 30)) %>%
    group_by(date) %>%
    slice(1:input$top_n) %>%
    arrange(date,confirmed,Province_State) %>%
    filter(confirmed > 0)
      }) # reset level so that higher volume shows up first on the graph
  
  
  state_daily_top_deaths <- reactive({
    state_daily_master %>%
    arrange(date,desc(deaths),Province_State) %>%
    filter(date >= (latest_date - 30)) %>%
    group_by(date) %>%
    slice(1:input$top_n) %>%
    arrange(date,deaths,Province_State)  %>% # reset level so that higher volume shows up first on the graph
    filter(deaths > 0)
    })
  
  
  
  
  
  output$top_states <- renderPlotly({
    if (input$confirmed_deaths == 1) {
      state_daily_top_confirmed() %>%
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
          title = paste0("Top ", input$top_n ," Confirmed by States - Last 30 Days"),
          paper_bgcolor='transparent', plot_bgcolor='transparent'
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Date ")
        )
      
    } else {
      state_daily_top_deaths() %>%
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
          title = paste0("Top ", input$top_n ," Deaths by States - Last 30 Days"),
          paper_bgcolor='transparent', plot_bgcolor='transparent'
          
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Date ")
        )
    }
    
  })
  
}
# Analysis plots

