# If All was selected from dropdown, then render a leaflet map with circle markers
# and popup weather LABEL for all five cities

# If just one specific city was selected, then render a leaflet map with one marker
# on the map and a popup with DETAILED_LABEL displayed

# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction_rf.R")

test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(city_weather_bike_df)
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  
  # Then render output plots with an id defined in ui.R
  observeEvent(input$city_dropdown, {
    
    if(input$city_dropdown == 'All') {
      
      output$temp_line <- renderPrint({ cat("") })
      output$bike_date_output <- renderPrint({ cat("") })
      output$bike_line <- renderPrint({ cat("") })
      output$humidity_pred_chart <- renderPrint({ cat("") })
      
      # Define color factor
      color_levels <- colorFactor(
        c("green", "yellow", "red"),
        levels = c("small", "medium", "large")
      )
      
      # ❌ Original radius function returns only one value
      # ✅ FIXED: vectorised version
      radius_pal <- function(levels) {
        case_when(
          levels == "small"  ~ 6,
          levels == "medium" ~ 10,
          levels == "large"  ~ 12,
          TRUE               ~ 6
        )
      }
      
      cities_max_bike <- city_weather_bike_df %>%
        group_by(CITY_ASCII, LNG, LAT) %>%
        summarize(
          MAX_BIKE_PREDICTION = max(BIKE_PREDICTION, na.rm = TRUE),
          BIKE_PREDICTION_LEVEL = BIKE_PREDICTION_LEVEL[which.max(BIKE_PREDICTION)],
          .groups = "drop"
        )
      
      output$city_bike_map <- renderLeaflet({
        leaflet(data = cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = cities_max_bike$LNG,
            lat = cities_max_bike$LAT,
            color = ~color_levels(BIKE_PREDICTION_LEVEL),
            radius = radius_pal(cities_max_bike$BIKE_PREDICTION_LEVEL),
            fillOpacity = 1,
            popup = ~CITY_ASCII
          )
      })
      
    } else {
      
      selected_data <- city_weather_bike_df %>%
        filter(CITY_ASCII == input$city_dropdown)
      
      output$city_bike_map <- renderLeaflet({
        leaflet(data = selected_data) %>%
          addTiles() %>%
          addMarkers(
            lng = ~LNG,
            lat = ~LAT,
            popup = ~DETAILED_LABEL
          )
      })
      
      # -----------------------------
      # Temperature plot (ROUNDED)
      # -----------------------------
      output$temp_line <- renderPlot({
        ggplot(
          selected_data,
          aes(
            x = seq_len(nrow(selected_data)),
            y = TEMPERATURE
          )
        ) +
          geom_line(color = "yellow", linewidth = 1.5) +
          geom_point(color = "black") +
          geom_text(
            aes(label = round(TEMPERATURE, 1)),
            vjust = -0.6,
            size = 3
          ) +
          labs(
            title = paste("Temperature Trend for", input$city_dropdown),
            x = "Time (3 hours ahead)",
            y = "Temperature (°C)"
          ) +
          theme_minimal()
      })
      
      selected_data$FORECASTDATETIME_new <- as.POSIXct(
        selected_data$FORECASTDATETIME,
        format = "%Y-%m-%d %H:%M:%S"
      )
      
      # -----------------------------
      # Bike prediction plot (ROUNDED)
      # -----------------------------
      output$bike_line <- renderPlot({
        ggplot(
          selected_data,
          aes(x = FORECASTDATETIME_new, y = BIKE_PREDICTION)
        ) +
          geom_line(
            color = "lightgreen",
            linetype = "dashed",
            linewidth = 1.5
          ) +
          geom_point(color = "red") +
          geom_text(
            aes(label = scales::comma(round(BIKE_PREDICTION))),
            vjust = -0.6,
            size = 3
          ) +
          labs(
            title = paste(
              "Bike-Sharing Demand Prediction for",
              input$city_dropdown
            ),
            x = "Forecast DateTime",
            y = "Bike Prediction"
          ) +
          theme_minimal()
      })
      
      # -----------------------------
      # Click output (ROUNDED)
      # -----------------------------
      output$bike_date_output <- renderPrint({
        req(input$plot_click)
        click <- input$plot_click
        
        nearest_point <- nearPoints(
          selected_data,
          click,
          threshold = 5,
          maxpoints = 1
        )
        
        if (nrow(nearest_point) > 0) {
          cat(
            "Bike-Sharing Prediction:",
            scales::comma(round(nearest_point$BIKE_PREDICTION)),
            "\n"
          )
          cat(
            "DateTime:",
            nearest_point$FORECASTDATETIME,
            "\n"
          )
        } else {
          cat("No point selected.\n")
        }
      })
      
      # -----------------------------
      # Humidity vs Bike Prediction
      # -----------------------------
      output$humidity_pred_chart <- renderPlot({
        ggplot(
          selected_data,
          aes(x = HUMIDITY, y = BIKE_PREDICTION)
        ) +
          geom_point(color = "black") +
          geom_smooth(
            method = "lm",
            formula = y ~ poly(x, 4),
            color = "red"
          ) +
          labs(
            title = paste(
              "Humidity vs Bike-Sharing Demand for",
              input$city_dropdown
            ),
            x = "Humidity (%)",
            y = "Bike Prediction"
          ) +
          theme_minimal()
      })
    }
  })
})
