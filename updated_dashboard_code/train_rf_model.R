############################################################
# FAST RANDOM FOREST BIKE DEMAND MODEL (DEPLOYMENT)
############################################################

library(tidymodels)
library(tidyverse)
library(ranger)

############################################################
# 1. Load dataset
############################################################
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"

bike_df <- read_csv(dataset_url, show_col_types = FALSE) %>%
  select(-DATE, -FUNCTIONING_DAY)

############################################################
# 2. Train / Test split
############################################################
set.seed(1234)
split <- initial_split(bike_df, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)

############################################################
# 3. Random Forest specification (FAST & STRONG)
############################################################
rf_spec <- rand_forest(
  mode  = "regression",
  trees = 500,      # strong but fast
  mtry  = 8,        # good default
  min_n = 10
) %>%
  set_engine("ranger", importance = "impurity")

############################################################
# 4. Model formula (hour dummies 1–23)
############################################################
rf_formula <- RENTED_BIKE_COUNT ~
  TEMPERATURE +
  HUMIDITY +
  WIND_SPEED +
  VISIBILITY +
  RAINFALL +
  SNOWFALL +
  
  `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` +
  `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` +
  `18` + `19` + `20` + `21` + `22` + `23` +
  
  AUTUMN + SPRING + SUMMER + WINTER

############################################################
# 5. Train model (SECONDS)
############################################################
rf_model <- rf_spec %>%
  fit(rf_formula, data = train_data)

############################################################
# 6. Evaluate
############################################################
preds <- predict(rf_model, test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

preds$.pred[preds$.pred < 0] <- 0

cat("\nModel performance:\n")
print(rmse(preds, truth, .pred))
print(rsq(preds, truth, .pred))

############################################################
# 7. Save model
############################################################
saveRDS(rf_model, "rf_bike_model.rds")
cat("\n✅ Random Forest model saved as rf_bike_model.rds\n")
