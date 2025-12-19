############################################################
# RANDOM FOREST BIKE DEMAND FORECAST (FULL PIPELINE)
############################################################

library(tidyverse)
library(httr)
library(tidymodels)
library(ranger)

############################################################
# 1. Fetch weather forecast data
############################################################
get_weather_forecast_by_cities <- function(city_names) {
  
  api_url <- "https://api.openweathermap.org/data/2.5/forecast"
  api_key <- "62fae4ff8743899280ff8abd50e6ee8d"
  
  results <- list()
  
  for (city in city_names) {
    
    response <- GET(api_url, query = list(q = city, appid = api_key, units = "metric"))
    if (status_code(response) != 200) next
    
    json <- content(response, as = "parsed", simplifyVector = FALSE)
    
    city_df <- map_dfr(json$list, function(x) {
      
      dt <- as.POSIXct(x$dt_txt)
      hour  <- as.integer(format(dt, "%H"))
      month <- as.integer(format(dt, "%m"))
      
      season <- ifelse(month %in% 3:5, "SPRING",
                       ifelse(month %in% 6:8, "SUMMER",
                              ifelse(month %in% 9:11, "AUTUMN", "WINTER")))
      
      rainfall <- if (!is.null(x$rain$`3h`)) x$rain$`3h` else 0
      snowfall <- if (!is.null(x$snow$`3h`)) x$snow$`3h` else 0
      
      tibble(
        CITY_ASCII = city,
        TEMPERATURE = x$main$temp,
        HUMIDITY = x$main$humidity,
        WIND_SPEED = x$wind$speed,
        VISIBILITY = x$visibility %||% 0,
        RAINFALL = rainfall,
        SNOWFALL = snowfall,
        HOURS = hour,
        SEASONS = season,
        FORECASTDATETIME = x$dt_txt,
        
        DETAILED_LABEL = paste0(
          "<b>", city, "</b><br>",
          "Temperature: ", round(x$main$temp,1), " °C<br>",
          "Visibility: ", x$visibility %||% 0, "m<br>",
          "Humidity: ", x$main$humidity, "%<br>",
          "Wind Speed: ", round(x$wind$speed,2), " m/s<br>",
          "Datetime: ", x$dt_txt
        )
      )
    })
    
    results[[city]] <- city_df
  }
  
  bind_rows(results)
}

############################################################
# 2. Feature engineering (MATCH RF TRAINING FEATURES)
############################################################
engineer_weather_features <- function(df) {
  
  df %>%
    mutate(
      `1`  = as.integer(HOURS == 1),
      `2`  = as.integer(HOURS == 2),
      `3`  = as.integer(HOURS == 3),
      `4`  = as.integer(HOURS == 4),
      `5`  = as.integer(HOURS == 5),
      `6`  = as.integer(HOURS == 6),
      `7`  = as.integer(HOURS == 7),
      `8`  = as.integer(HOURS == 8),
      `9`  = as.integer(HOURS == 9),
      `10` = as.integer(HOURS == 10),
      `11` = as.integer(HOURS == 11),
      `12` = as.integer(HOURS == 12),
      `13` = as.integer(HOURS == 13),
      `14` = as.integer(HOURS == 14),
      `15` = as.integer(HOURS == 15),
      `16` = as.integer(HOURS == 16),
      `17` = as.integer(HOURS == 17),
      `18` = as.integer(HOURS == 18),
      `19` = as.integer(HOURS == 19),
      `20` = as.integer(HOURS == 20),
      `21` = as.integer(HOURS == 21),
      `22` = as.integer(HOURS == 22),
      `23` = as.integer(HOURS == 23),
      
      AUTUMN = as.integer(SEASONS == "AUTUMN"),
      SPRING = as.integer(SEASONS == "SPRING"),
      SUMMER = as.integer(SEASONS == "SUMMER"),
      WINTER = as.integer(SEASONS == "WINTER")
    )
}

############################################################
# 3. Predict bike demand using Random Forest (FIXED)
############################################################
predict_bike_demand_rf <- function(weather_fe_df,
                                   model_path = "rf_bike_model.rds") {
  
  model <- readRDS(model_path)
  
  preds <- predict(model, new_data = weather_fe_df)$.pred
  preds[preds < 0] <- 0
  round(preds, 0)
}

############################################################
# 4. Demand level
############################################################
calculate_bike_prediction_level <- function(preds) {
  case_when(
    preds <= 1000 ~ "small",
    preds < 3000  ~ "medium",
    TRUE          ~ "large"
  )
}

############################################################
# 5. FULL PIPELINE
############################################################
generate_city_weather_bike_data <- function() {
  
  cities_df <- read_csv("selected_cities.csv", show_col_types = FALSE)
  
  weather_raw <- get_weather_forecast_by_cities(cities_df$CITY_ASCII)
  weather_fe  <- engineer_weather_features(weather_raw)
  
  preds <- predict_bike_demand_rf(weather_fe)
  
  weather_raw %>%
    mutate(
      BIKE_PREDICTION = preds,
      BIKE_PREDICTION_LEVEL = calculate_bike_prediction_level(preds)
    ) %>%
    left_join(cities_df, by = "CITY_ASCII")
}

############################################################
# 6. RUN
############################################################
bike_forecast_df <- generate_city_weather_bike_data()
View(bike_forecast_df)
