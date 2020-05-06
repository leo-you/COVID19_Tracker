source("Main.R")

server <- function(input, output) {
  

  
  # summary table reactive functions
  # input$Date_Slider
  us_master_filtered <- reactive({

    state_daily_master %>%
      filter(date <= input$Date_Slider & date >= input$Date_Slider-1) %>%
      mutate(confirmed_growth = cum_confirmed/lag(cum_confirmed) - 1,
             deaths_growth = cum_deaths/lag(cum_deaths) - 1) %>%
      filter(date == input$Date_Slider & cum_confirmed > 0 & population > 0) %>%
      mutate("deaths_rate" = cum_deaths/cum_confirmed,
             "confirmed_pop" = round(cum_confirmed * 1000000 / population,2),
             "deaths_pop" = round(cum_deaths* 1000000/ population,2),
             population = round(population/1000000,2),
             "positive_rate" = round(positive/total_tested,2)) %>%
      arrange(desc(cum_confirmed)) %>%
      select(!date)

    })

  output$table_case = DT::renderDataTable(isolate(

    datatable(us_master_filtered()[,c(1,2,20,3,21,22)],rownames = FALSE,
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
        'cum_confirmed',
        background = styleColorBar(us_master_filtered()$cum_confirmed, color = "lightblue"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'cum_deaths',
        background = styleColorBar(us_master_filtered()$cum_deaths, color = "lightgray"),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  ))


  proxy_table_case = dataTableProxy('table_case')

  observe({
    replaceData(proxy_table_case, us_master_filtered()[,c(1,2,20,3,21,22)], resetPaging = FALSE,clearSelection = "all",rownames = FALSE)
  })



  output$table_population = renderDT({isolate(

    datatable(us_master_filtered()[,c(1,6,23,24)],rownames = FALSE,
              colnames = c('State', 'Population/(M)', 'Confirmed Cases/Population (1M)', 'Deaths Cases/Population (1M)'),
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
    replaceData(proxy_table_population, us_master_filtered()[,c(1,6,23,24)], resetPaging = FALSE,clearSelection = "all",rownames = FALSE)
  })
  

  brks <- reactive({quantile(us_master$positive_rate, probs = seq(.05, .95, .05), na.rm = TRUE)})
  clrs <- reactive({
    round(seq(255, 40, length.out = length(brks()) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    })
  
  
  output$table_tested = renderDT({isolate(
    
    datatable(us_master_filtered()[,c(1,15,7,8,25)],rownames = FALSE,
              colnames = c('State', 'Total Tested', 'Positive', 'Negative',"Positive %"),
              extensions = c('FixedHeader',"Scroller"),
              options = list(pageLength = nrow(us_master_filtered()),
                             deferRender = TRUE,
                             scrollY = 400,
                             scroller = TRUE
              )
    ) %>%
      formatPercentage(c('positive_rate'),0) %>%
      formatStyle(
        'positive_rate',
        backgroundColor = styleInterval(brks(), clrs())
      ) 
  )
  })
  
  proxy_table_tested = dataTableProxy('table_tested')
  
  observe({
    replaceData(proxy_table_tested, us_master_filtered()[,c(1,15,7,8,25)], resetPaging = FALSE,clearSelection = "all",rownames = FALSE)
  })
  
  
  # summary table reactive functions
  



  output$full_table = renderDT({full_table})
  
  
  output$choropleth_map = renderLeaflet({
    if (input$choroplethmap_type == 1) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
          fillColor = ~pal_confirmed(us_case_map_state$cum_confirmed),
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
          values = us_case_map_state$cum_confirmed,
          position = "bottomright",
          title = "US Confirmed Cases",
          labFormat = labelFormat(digits = 0))
    } else if (input$choroplethmap_type == 2) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
          fillColor = ~pal_deaths(us_case_map_state$cum_deaths),
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
          values = us_case_map_state$cum_deaths,
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
          title = "US Confirmed Cases/<br>Population 1M",
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
          title = "US Deaths Cases/<br>Population 1M",
          labFormat = labelFormat(digits = 0))
    } else if (input$choroplethmap_type == 5) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addPolygons(
          data = us_case_map_state,
          fillColor = ~pal_tested(us_case_map_state$total_tested),
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
          label = us_tested_labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        leaflet::addLegend(
          data = us_case_map_state,
          pal = pal_tested,
          values = us_case_map_state$total_tested,
          position = "bottomright",
          title = "US Tested Cases",
          labFormat = labelFormat(digits = 0))
    }


  })


  state_daily_master_filtered <- reactive({

    if (input$start_date == 1) {
      state_daily_master %>%
        filter(Province_State == input$state_filter) %>%
        filter(cum_confirmed > 0)
    } else if (input$start_date == 2) {
      state_daily_master %>%
        filter(Province_State == input$state_filter) %>%
        filter(cum_confirmed >= 100)
    } else if (input$start_date == 3) {
      state_daily_master %>%
        filter(Province_State == input$state_filter) %>%
        filter(cum_deaths >= 100)
    }
  })
  
  state_daily_master_multi_selection <- reactive({
    state_daily_master %>%
      filter(Province_State %in% input$Multi_state_filter)
    
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
      addProviderTiles("CartoDB.Positron",group = "Light Mode") %>%
      addProviderTiles("CartoDB.DarkMatter",group = "Dark Mode") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("Light Mode", "Dark Mode"),
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
  
  output$key_figures = renderUI({
    tagList(
      helpText(paste0("As of: ",input$Date_Slider)),
      br(),
      valueBox(HTML(paste(us_evolution_us$cum_confirmed[us_evolution_us$date == input$Date_Slider],br(),
                          h5(paste0("(+",us_evolution_us$daily_confirmed[us_evolution_us$date == input$Date_Slider]," New)")))),
               "Confirmed Cases",
               icon = icon("ambulance", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(HTML(paste(us_evolution_us$cum_deaths[us_evolution_us$date == input$Date_Slider],br(),
                          h5(paste0("(+",us_evolution_us$daily_deaths[us_evolution_us$date == input$Date_Slider]," New)")))),
               "Deaths Cases",
               icon = icon("heartbeat", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(HTML(paste(us_evolution_us$total_tested[us_evolution_us$date == input$Date_Slider],br(),
                          h5(paste0(" Positive %: ",paste0(round(us_evolution_us$positive_rate[us_evolution_us$date == input$Date_Slider],2) * 100,"%")
                                    )))),
               "Tested",
               icon = icon("first-aid", lib = "font-awesome"),
               color = "yellow",width = 3),
      valueBox(HTML(paste(us_evolution_us$hospitalized[us_evolution_us$date == input$Date_Slider],br(),
                          h5(paste0(" In ICU: ",us_evolution_us$icu[us_evolution_us$date == input$Date_Slider])))),
               "Hospitalized",
               icon = icon("hospital", lib = "font-awesome"),
               color = "yellow",width = 3),
      br(),
      helpText(paste0("last updated: ",latest_date))
    )
  })


  # Key Figures outputs
  
  
  # Analysis plots

  output$daily_trend <- renderPlotly({

    plot_ly(us_evolution_us, x = ~date, hoverinfo = "text",
            text = ~paste('<b>Date:</b> ', date, '<br><b>Daily Confirmed:</b> ', daily_confirmed,
                          '<br><b>Daily Deaths:</b> ', daily_deaths,'<br><b>Daily Recovered:</b> ', daily_recovered)) %>%
      add_bars(y = ~daily_confirmed, name = 'Daily Cases') %>%
      add_bars(y = ~(daily_deaths * -1), name = "Daily Deaths") %>%
      add_bars(y = ~(daily_recovered * -1), name = "Daily Recovered") %>%
      layout(yaxis = list(tickformat = "0",title = "Count"),
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             title = "US Daily Cases Trend",
             barmode = "relative",legend = list(x = 0,y = 1)) %>%
      config(displayModeBar = F)
      

  })
  
  output$cum_trend <- renderPlotly({
    if (input$log_y == FALSE ) {
      plot_ly(x = ~date) %>%
        add_lines(data =us_evolution_us, y = ~cum_confirmed, name = "Cum Cases") %>%
        add_lines(data =us_evolution_us, y = ~cum_deaths, name = "Cum Deaths") %>%
        add_lines(data =us_evolution_us, y = ~recovered, name = "Cum Recovered") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = "US Cumulative Cases",
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)

    } else {
      plot_ly(x = ~date) %>%
        add_lines(data =us_evolution_us, y = ~log10(cum_confirmed), name = "Log Cases") %>%
        add_lines(data =us_evolution_us, y = ~log10(cum_deaths), name = "Log Deaths") %>%
        add_lines(data =us_evolution_us, y = ~log10(recovered), name = "Log Recovered") %>%
        layout(yaxis = list(tickformat = "0",title = "Log"),
               title = "US Cumulative Cases",
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)
    }
  })
  
  output$cum_bar <- renderPlotly(({
    plot_ly(us_evolution_us, x = ~date, hoverinfo = "text",
            text = ~paste('<b>Date:</b> ', date, '<br><b>Cum Confirmed:</b> ', cum_confirmed,
                          '<br><b>Cum Deaths:</b> ', cum_deaths,'<br><b>Cum Recovered:</b> ', recovered)
            ) %>%
      add_bars(y = ~cum_confirmed, name = 'Cum Cases') %>%
      add_bars(y = ~(cum_deaths * -1), name = "Cum Deaths") %>%
      add_bars(y = ~(recovered * -1), name = "Cum Recovered") %>%
      layout(yaxis = list(tickformat = "0",title = "Count"),
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             title = "US Cumulative Cases Barplot",
             barmode = "relative",legend = list(x = 0,y = 1)) %>%
      config(displayModeBar = F)
  }))
  
  
  output$fatality <- renderPlotly(({
    plot_ly(filter(us_evolution_us,us_evolution_us$cum_deaths > 100), x = ~date) %>%
      add_lines(y = ~death_rate, name = 'Fatality') %>%
      layout(yaxis = list(tickformat = ".1%",title = "Fatality"),
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             title = "US Fatality",legend = list(x = 0,y = 1)) %>%
      config(displayModeBar = F)
  }))
  
  
  
  
  output$testing_trend <- renderPlotly({
    plot_ly(us_evolution_us, x = ~date) %>%
      add_bars(y = ~positive, name = 'Positive') %>%
      add_bars(y = ~negative, name = 'Negative') %>%
      add_lines(data = filter(us_evolution_us,total_tested > 10000),
                y = ~(positive/total_tested),name = "Positive %",yaxis = "y2") %>%
      layout(yaxis = list(tickformat = "0",title = "Count"),
             margin = list(r = 50),
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             title = "Testing Data",
             barmode = "stack",legend = list(x = 0,y = 1),
             yaxis2 = list(
               range = c(0, 1),
               tickformat = "%",
               overlaying = "y",
               side = "right",
               title = "positive %",
               hoverformat = "%",
               showgrid = FALSE
             )
      ) %>%
      config(displayModeBar = F)
  })
  
  output$hospitalized_data <- renderPlotly({
    plot_ly(data = filter(us_evolution_us,us_evolution_us$hospitalized > 0), x = ~date) %>%
      add_lines(y = ~hospitalized, name = "Hospitalized") %>%
      add_lines(y = ~icu, name = "In ICU") %>%
      add_lines(y = ~ventilator, name = "On Ventilator") %>%
      layout(yaxis = list(tickformat = "0",title = "Count"),
             title = "US Hospitalized Patients",
             paper_bgcolor='transparent', plot_bgcolor='transparent',
             legend = list(x = 0,y = 1)) %>%
      config(displayModeBar = F)
  })
  
  output$correlation <- renderPlotly({
    plot_ly(data = filter(state_daily_master,date == latest_date), x = ~cum_confirmed, y = ~cum_deaths, 
            type = 'scatter', mode = 'markers', color = ~(positive/total_tested),
            hoverinfo = 'text',
            text = ~paste('<b>State:</b> ', Province_State, '<br><b>Confirmed:</b> ', cum_confirmed,
                          '<br><b>Deaths:</b> ', cum_deaths,'<br><b>Total Tested:</b> ', total_tested,
                          '<br><b>Positive %:</b> ', paste0(round(positive/total_tested,2)*100,"%")
            ),
            marker = list(size = ~total_tested/20000, opacity = 0.5)) %>%
      layout(yaxis = list(tickformat = "0",title = "Cumulative Deaths"),
             xaxis = list(tickformat = "0",title = "Cumulative Confirmed"),
             title = "",
             paper_bgcolor='transparent', plot_bgcolor='transparent'
      ) %>%
      colorbar(title = "<b>Positive %</b>") %>%
      config(displayModeBar = F)
    
  })
  
  
  output$state_trend <- renderPlotly({

    if (input$variable_choice == 1) {
      plot_ly(state_daily_master_filtered(),x = ~date) %>%
        add_lines(y = ~cum_confirmed, name = "Cum Cases") %>%
        add_lines(y = ~cum_deaths, name = "Cum Deaths") %>%
        add_lines(y = ~recovered, name = "Cum Recovered") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0(input$state_filter," Cumulative Trend"),
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)
    } else {

      plot_ly(state_daily_master_filtered(),x = ~date, hoverinfo = "text",
              text = ~paste('<b>Date:</b> ', date, '<br><b>Daily Confirmed:</b> ', daily_confirmed,
                            '<br><b>Daily Deaths:</b> ', daily_deaths,'<br><b>Daily Recovered:</b> ', daily_recovered)
              ) %>%
        add_bars(y = ~daily_confirmed, name = "Daily New") %>%
        add_bars(y = ~(daily_deaths * -1), name = "Daily Deaths") %>%
        add_bars(y = ~(daily_recovered * -1), name = "Daily Recovered") %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0(input$state_filter," Daily Trend"),
               paper_bgcolor='transparent', plot_bgcolor='transparent', barmode = "relative",
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)
    }
  })
  
  output$state_comparison <- renderPlotly({
    if (input$variable_choice2 == 1) {
      plot_ly(state_daily_master_multi_selection(),x = ~date) %>%
        add_lines(y = ~cum_confirmed, color = ~Province_State, colors = mycolors) %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0("State Confirmed Comparison"),
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)
    } else {
      
      plot_ly(state_daily_master_multi_selection(),x = ~date) %>%
        add_lines(y = ~cum_deaths, color = ~Province_State, colors = mycolors) %>%
        layout(yaxis = list(tickformat = "0",title = "Count"),
               title = paste0("State Deaths Comparison"),
               paper_bgcolor='transparent', plot_bgcolor='transparent',
               legend = list(x = 0,y = 1)) %>%
        config(displayModeBar = F)
    }

  })
  

  state_daily_top_confirmed <- reactive({
    state_daily_master %>%
    arrange(date,desc(cum_confirmed),Province_State) %>%
    filter(date >= (latest_date - 30)) %>%
    group_by(date) %>%
    slice(1:input$top_n) %>%
    arrange(date,cum_confirmed,Province_State) %>%
    filter(cum_confirmed > 0)
      }) # reset level so that higher volume shows up first on the graph
  
  
  state_daily_top_deaths <- reactive({
    state_daily_master %>%
    arrange(date,desc(cum_deaths),Province_State) %>%
    filter(date >= (latest_date - 30)) %>%
    group_by(date) %>%
    slice(1:input$top_n) %>%
    arrange(date,cum_deaths,Province_State)  %>% # reset level so that higher volume shows up first on the graph
    filter(cum_deaths > 0)
    })


  output$top_states <- renderPlotly({
    if (input$confirmed_deaths == 1) {
      state_daily_top_confirmed() %>%
        plot_ly(x = ~cum_confirmed, y = ~Province_State, frame = ~ as.factor(date),
                type = 'bar', orientation = 'h', showlegend = FALSE,
                marker = list(color = ~cum_confirmed,
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
                       categoryarray = ~cum_confirmed),
          title = paste0("Top ", input$top_n ," Confirmed by States - Last 30 Days"),
          paper_bgcolor='transparent', plot_bgcolor='transparent'
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Date ")
        ) %>%
        config(displayModeBar = F)

    } else {
      state_daily_top_deaths() %>%
        plot_ly(x = ~cum_deaths, y = ~Province_State, frame = ~ as.factor(date),
                type = 'bar', orientation = 'h', showlegend = FALSE,
                marker = list(color = ~cum_deaths,
                              line = list(color = "rgb(20, 20, 20)",
                                          width = 1))
        ) %>%
        layout(
          xaxis = list(
            tickformat = "0"
          ),
          yaxis = list(title = "",
                       categoryorder = "array",
                       categoryarray = ~cum_deaths),
          title = paste0("Top ", input$top_n ," Deaths by States - Last 30 Days"),
          paper_bgcolor='transparent', plot_bgcolor='transparent'

        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Date ")
        ) %>%
        config(displayModeBar = F)
    }

  })
  
}
# Analysis plots

