
# Refine the Baseline Regression Models

# Load the required libraries
library("tidymodels")
library("tidyverse")
library("stringr")
library("gridExtra")

# Load dataset from URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

write.csv(bike_sharing_df, "seoul_bike_sharing_converted_normalized.csv", row.names = FALSE)

# Drop unnecessary columns
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

# Define a linear regression model specification.
lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

# Split the data into training and testing datasets.
set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

# TASK: Add polynomial terms

# For example, the correlation between RENTED_BIKE_COUNT and TEMPERATURE does not look like linear:
ggplot(data = train_data, aes(y = RENTED_BIKE_COUNT, x = TEMPERATURE)) + 
  geom_point()

# One solution to handle such nonlinearity is using polynomial regression by adding polynomial terms. As shown before,
# higher order polynomials are better than the first order polynomial.
# Plot the higher order polynomial fits
ggplot(data=train_data, aes(y = RENTED_BIKE_COUNT, x = TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

#--------------------------------------------------------------------------------------------------------------------------------

# TODO: Fit a linear regression model lm_poly with higher order polynomial terms on 
# the important variables (larger coefficients) found in the baseline model

# Create polynomial linear regression model
lm_poly <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ poly(RAINFALL, 3) + poly(HUMIDITY, 3) + poly(TEMPERATURE, 3) + 
        WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SNOWFALL + 
        `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + 
        `20` + `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + 
        AUTUMN + SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY, data = train_data)

# Print model summary
summary(lm_poly$fit)


# TODO: Make predictions on test dataset using the lm_poly models
model_all_test_results <- lm_poly %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(model_all_test_results)

#Convert all negative prediction results to zero, because we can not have negative rented bike counts
model_all_test_results[model_all_test_results$.pred < 0, ".pred"] <- 0

# Calculating R_squared and RMSE
rsq_all <- rsq(model_all_test_results, truth = truth,
               estimate = .pred)

rmse_all <- rmse(model_all_test_results, truth = truth,
                 estimate = .pred)

rsq_all
rmse_all

#--------------------------------------------------------------------------------------------------------------------------------

# TASK: Add interaction terms

# Create polynomial linear regression model with interaction terms
lm_poly_with_interactions <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ poly(RAINFALL, 3)*poly(HUMIDITY, 3) + 
        WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SNOWFALL + 
        `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + 
        `20` + `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + 
        AUTUMN*poly(TEMPERATURE, 3) + SPRING*poly(TEMPERATURE, 3) + SUMMER*poly(TEMPERATURE, 3) + WINTER*poly(TEMPERATURE, 3) + 
        HOLIDAY + NO_HOLIDAY
         , data = train_data)

# Print model summary
summary(lm_poly_with_interactions$fit)

# TODO: Make predictions on test dataset using the lm_poly models
model_all_test_results_interactions <- lm_poly_with_interactions %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(model_all_test_results_interactions)

#Convert all negative prediction results to zero, because we can not have negative rented bike counts
model_all_test_results_interactions[model_all_test_results_interactions$.pred < 0, ".pred"] <- 0

# Calculating R_squared and RMSE
rsq_all_interactions <- rsq(model_all_test_results_interactions, truth = truth,
               estimate = .pred)

rmse_all_interactions <- rmse(model_all_test_results_interactions, truth = truth,
                 estimate = .pred)

rsq_all_interactions
rmse_all_interactions

#--------------------------------------------------------------------------------------------------------------------------------

# TASK: Add regularization

# TODO: Define a linear regression model specification glmnet_spec using glmnet engine

# Create a Recipe:
recipe <-
  recipe(RENTED_BIKE_COUNT ~ ., data = train_data)

# Define the ridge model
tune_spec <- linear_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet")

# Create workflow
ridge_wf <- workflow() %>%
  add_recipe(recipe)

# Define cross validation to resample the data
cvfolds <- vfold_cv(train_data)

# Set up the grid using grid_regular()
# Note: The levels are how many values to use and in penalty() you can specify the range of values to use. By default, the range values are inverse log transformed. 
# This means that −3 is really 10^−3 and 0.3 is really  10^0.3
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

# Tune the grid, use tune_grid() and include the lambda grid just specified
ridge_grid <- tune_grid(
  ridge_wf %>% add_model(tune_spec), 
  resamples = cvfolds, 
  grid = lambda_grid)

# View best results for RMSE and RSQ
show_best(ridge_grid, metric = "rmse")
show_best(ridge_grid, metric = "rsq")

# Train the model with ridge model which has best penalty value 
ridge_spec <- linear_reg(penalty = 0.001, mixture = 0) %>%
  set_engine("glmnet")
ridge_wf <- workflow() %>%
  add_recipe(recipe)
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

# Test the model with ridge model which has best penalty value
model_all_test_results_ridge <- ridge_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Calculating R_squared and RMSE
rsq_all_ridge <- rsq(model_all_test_results_ridge, truth = truth,
                            estimate = .pred)

rmse_all_ridge <- rmse(model_all_test_results_ridge, truth = truth,
                              estimate = .pred)

rsq_all_ridge
rmse_all_ridge

#--------------------------------------------------------------------------------------------------------------------------------

# TASK: Experiment to search for improved models

# Build at least five different models using polynomial terms, interaction terms, and regularizations.

############################################################
# Model-1 (Polynomial Linear Regression)
############################################################

# Train the model with training set
lm_poly <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ poly(RAINFALL, 6) + poly(HUMIDITY, 6) + poly(TEMPERATURE, 6) + 
        WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SNOWFALL + 
        `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + 
        `20` + `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + 
        AUTUMN + SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY, data = train_data)

# Test the model with testing set
lm_poly_test_results <- lm_poly %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

#Convert all negative prediction results to zero, because we can not have negative rented bike counts
lm_poly_test_results[lm_poly_test_results$.pred < 0, ".pred"] <- 0

# Calculating R_squared and RMSE
rsq_lm_poly <- rsq(lm_poly_test_results, truth = truth,
               estimate = .pred)
rmse_lm_poly <- rmse(lm_poly_test_results, truth = truth,
                 estimate = .pred)

rsq_lm_poly
rmse_lm_poly


############################################################
# Model-2 (Polynomial Linear Regression with interaction terms)
############################################################

# Train the model with training dataset
lm_poly_with_interactions <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ poly(RAINFALL, 6)*poly(HUMIDITY, 6) + 
        WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SNOWFALL + 
        `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + 
        `20` + `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + 
        AUTUMN*poly(TEMPERATURE, 6) + SPRING*poly(TEMPERATURE, 6) + SUMMER*poly(TEMPERATURE, 6) + WINTER*poly(TEMPERATURE, 6) + 
        HOLIDAY + NO_HOLIDAY
      , data = train_data)

# Test the model with testing dataset
model_all_test_results_interactions <- lm_poly_with_interactions %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

#Convert all negative prediction results to zero, because we can not have negative rented bike counts
model_all_test_results_interactions[model_all_test_results_interactions$.pred < 0, ".pred"] <- 0

# Calculating R_squared and RMSE
rsq_all_interactions <- rsq(model_all_test_results_interactions, truth = truth,
                            estimate = .pred)
rmse_all_interactions <- rmse(model_all_test_results_interactions, truth = truth,
                              estimate = .pred)

rsq_all_interactions
rmse_all_interactions


############################################################
# Model-3 (Ridge Regularization with grid search)
############################################################

# Create a Recipe:
recipe <-
  recipe(RENTED_BIKE_COUNT ~ ., data = train_data)

# Define the ridge model
tune_spec <- linear_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet")

# Create workflow
ridge_wf <- workflow() %>%
  add_recipe(recipe)

# Define cross validation to resample the data
cvfolds <- vfold_cv(train_data)

# Set up the grid using grid_regular()
# Note: The levels are how many values to use and in penalty() you can specify the range of values to use. By default, the range values are inverse log transformed. 
# This means that −3 is really 10^−3 and 0.3 is really  10^0.3
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

# Tune the grid, use tune_grid() and include the lambda grid just specified
ridge_grid <- tune_grid(
  ridge_wf %>% add_model(tune_spec), 
  resamples = cvfolds, 
  grid = lambda_grid)

# View best results for RMSE and RSQ
show_best(ridge_grid, metric = "rmse")
show_best(ridge_grid, metric = "rsq")

# Train the model with ridge model which has best penalty value 
ridge_spec <- linear_reg(penalty = 0.001, mixture = 0) %>%
  set_engine("glmnet")
ridge_wf <- workflow() %>%
  add_recipe(recipe)
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

# Test the model with ridge model which has best penalty value
model_all_test_results_ridge <- ridge_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Calculating R_squared and RMSE
rsq_all_ridge <- rsq(model_all_test_results_ridge, truth = truth,
                     estimate = .pred)

rmse_all_ridge <- rmse(model_all_test_results_ridge, truth = truth,
                       estimate = .pred)

rsq_all_ridge
rmse_all_ridge


############################################################
# Model-4 (Lasso Regularization with grid search)
############################################################

# Create a Recipe:
recipe <-
  recipe(RENTED_BIKE_COUNT ~ ., data = train_data)

# Define the lasso model
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# Create workflow
lasso_wf <- workflow() %>%
  add_recipe(recipe)

# Define cross validation to resample the data
cvfolds <- vfold_cv(train_data)

# Set up the grid using grid_regular()
# Note: The levels are how many values to use and in penalty() you can specify the range of values to use. By default, the range values are inverse log transformed. 
# This means that −3 is really 10^−3 and 0.3 is really  10^0.3
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

# Tune the grid, use tune_grid() and include the lambda grid just specified
lasso_grid <- tune_grid(
  lasso_wf %>% add_model(tune_spec), 
  resamples = cvfolds, 
  grid = lambda_grid)

# View best results for RMSE and RSQ
show_best(lasso_grid, metric = "rmse")
show_best(lasso_grid, metric = "rsq")

# Train the model with ridge model which has best penalty value 
lasso_spec <- linear_reg(penalty = 0.001, mixture = 1) %>%
  set_engine("glmnet")
lasso_wf <- workflow() %>%
  add_recipe(recipe)
lasso_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

# Test the model with ridge model which has best penalty value
model_all_test_results_lasso <- ridge_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Calculating R_squared and RMSE
rsq_all_lasso <- rsq(model_all_test_results_lasso, truth = truth,
                     estimate = .pred)

rmse_all_lasso <- rmse(model_all_test_results_lasso, truth = truth,
                       estimate = .pred)

rsq_all_lasso
rmse_all_lasso


############################################################
# Model-5 (Elastic Net Regularization with grid search)
############################################################

# Create a Recipe:
recipe <-
  recipe(RENTED_BIKE_COUNT ~ ., data = train_data)

# Define the ridge model
tune_spec <- linear_reg(penalty = tune(), mixture = 0.3) %>% 
  set_engine("glmnet")

# Create workflow
Elastic_Net_wf <- workflow() %>%
  add_recipe(recipe)

# Define cross validation to resample the data
cvfolds <- vfold_cv(train_data)

# Set up the grid using grid_regular()
# Note: The levels are how many values to use and in penalty() you can specify the range of values to use. By default, the range values are inverse log transformed. 
# This means that −3 is really 10^−3 and 0.3 is really  10^0.3
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

# Tune the grid, use tune_grid() and include the lambda grid just specified
Elastic_Net_grid <- tune_grid(
  Elastic_Net_wf %>% add_model(tune_spec), 
  resamples = cvfolds, 
  grid = lambda_grid)

# View best results for RMSE and RSQ
show_best(Elastic_Net_grid, metric = "rmse")
show_best(Elastic_Net_grid, metric = "rsq")

# Train the model with ridge model which has best penalty value 
Elastic_Net_spec <- linear_reg(penalty =  1.71, mixture = 0.3) %>%
  set_engine("glmnet")
Elastic_Net_wf <- workflow() %>%
  add_recipe(recipe)
Elastic_Net_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

# Test the model with ridge model which has best penalty value
model_all_test_results_Elastic_Net <- Elastic_Net_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Calculating R_squared and RMSE
rsq_all_Elastic_Net <- rsq(model_all_test_results_Elastic_Net, truth = truth,
                     estimate = .pred)

rmse_all_Elastic_Net <- rmse(model_all_test_results_Elastic_Net, truth = truth,
                       estimate = .pred)

rsq_all_Elastic_Net
rmse_all_Elastic_Net


############################################################
# Model-6 (Random Forest Regression)
############################################################

rf_spec <- rand_forest(
  mode  = "regression", trees = 500, mtry  = 8, min_n = 10 ) %>%
  set_engine("ranger", importance = "impurity")

rf_formula <- RENTED_BIKE_COUNT ~
  TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + RAINFALL + SNOWFALL +
  
  `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + `11` + `12` + 
  `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + `21` + `22` + `23` + `0`+
  
  AUTUMN + SPRING + SUMMER + WINTER

rf_fit <- rf_spec %>%
  fit(rf_formula, data = train_data)

rf_test_results <- rf_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

rf_test_results$.pred[rf_test_results$.pred < 0] <- 0

rsq_rf <- rsq(rf_test_results, truth = truth, estimate = .pred)
rmse_rf <- rmse(rf_test_results, truth = truth, estimate = .pred)

rsq_rf
rmse_rf

#--------------------------------------------------------------------------------------------------------------------------------

# TODO: Visualize the saved RMSE and R-squared values using a grouped barchart

# Create Data Frame for the 5 models' R_squared and RMSE values
model <- c(
  'lm_poly',
  'lm_poly_with_interactions',
  'ridge',
  'lasso',
  'Elastic_Net',
  'Random_Forest'
)

rsq <- c(
  rsq_lm_poly$.estimate,
  rsq_all_interactions$.estimate,
  rsq_all_ridge$.estimate,
  rsq_all_lasso$.estimate,
  rsq_all_Elastic_Net$.estimate,
  rsq_rf$.estimate
)

rmse <- c(
  rmse_lm_poly$.estimate,
  rmse_all_interactions$.estimate,
  rmse_all_ridge$.estimate,
  rmse_all_lasso$.estimate,
  rmse_all_Elastic_Net$.estimate,
  rmse_rf$.estimate
)

models_df <- data.frame(model, rsq, rmse)
models_df


# R-squared plot
plot_rsq <- ggplot(models_df, aes(x = rsq, y = reorder(model, rsq))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(rsq, 3)), hjust = -0.1) +
  labs(title = "Model Comparison (R-squared)", x = "R-squared", y = "Models") +
  theme_minimal() +
  xlim(0, max(models_df$rsq) * 1.1)

# RMSE plot
plot_rmse <- ggplot(models_df, aes(x = rmse, y = reorder(model, rsq))) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = round(rmse, 2)), hjust = -0.1) +
  labs(title = "Model Comparison (RMSE)", x = "RMSE", y = "Models") +
  theme_minimal() +
  xlim(0, max(models_df$rmse) * 1.1)

# Arrange plots side by side
p1 <- grid.arrange(plot_rsq, plot_rmse, ncol = 2)

# Save the plot
ggsave("bar_chart.png", p1, width = 10, height = 6)

#--------------------------------------------------------------------------------------------------------------------------------

# Plotting QQ plot for Best model, which is Model-2(Ploynomial linear regression with interaction terms)

head(model_all_test_results_interactions)

ggplot(model_all_test_results_interactions) +
  stat_qq(aes(sample=truth), color='green') +
  stat_qq(aes(sample=.pred), color='red') +
  labs(title = "QQ Plot of the best model") +
  theme_minimal()

#--------------------------------------------------------------------------------------------------------------------------------

# Feature Importance for Random Forest

library(dplyr)
library(ggplot2)

imp <- rf_fit %>%
  extract_fit_engine() %>%
  ranger::importance()

imp_df <- tibble(
  feature = names(imp),
  importance = as.numeric(imp)
)

# Keep ALL features (including zero)
ggplot(imp_df, aes(x = importance, y = reorder(feature, importance))) +
  geom_col(fill = "steelblue") +
  labs(title = "Random Forest Feature Importance",
       x = "Importance (Impurity Decrease)",
       y = "Feature")

#--------------------------------------------------------------------------------------------------------------------------------

# Extract model coefficients using broom's tidy function
model_coefficients <- tidy(lm_poly_with_interactions)

# View the coefficients
print(model_coefficients, n = 104)

RENTED_BIKE_COUNT ~ poly(RAINFALL, 6)*poly(HUMIDITY, 6) + WIND_SPEED + 
                    VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SNOWFALL + 
                    `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + 
                    `20` + `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + 
                    AUTUMN*poly(TEMPERATURE, 6) + SPRING*poly(TEMPERATURE, 6) + SUMMER*poly(TEMPERATURE, 6) + 
                    WINTER*poly(TEMPERATURE, 6) + HOLIDAY + NO_HOLIDAY
                    




















