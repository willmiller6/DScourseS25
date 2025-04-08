#DS for Econ PS9 Will Miller

#load the necessary libraries
library(tidyverse)
library(tidymodels)
library(glmnet)

#load uci housing data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

#set seed for reproducibility (from PSet)
set.seed(123456)

#create train and test sets for ENet CV
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

#Create a recipe for the model (from text of Pset)
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # Convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # Convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # Create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  # Create square terms of some continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, 
            lstat, dis, nox, degree = 6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# Create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)


#Eestimate a lasso model using 6-fold CV

tune_lasso <- linear_reg(
  penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

#Create a grid of values for the penalty parameter
lambda_grid <- grid_regular(penalty(), levels = 50)

#6-fold CV
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

#workflow
lasso_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_lasso)

#tune the model
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = rec_folds,
  grid = lambda_grid
)
#plot the results
lasso_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_line(color="red", alpha=0.5) +
  geom_point(color="red", alpha=0.5) +
  scale_x_log10() +
  labs(title = "Lasso CV RMSE vs. Penalty",
       x = "Lambda (log scale)",
       y = "RMSE")+
  theme_minimal()

#select the best penalty
lasso_best <- lasso_tune %>%
  select_best()
lasso_best


#fit the lasso model to the training data
lasso_fit <- lasso_wf %>%
  finalize_workflow(lasso_best) %>%
  fit(data = housing_train_prepped)

#in-sample RMSE
lasso_fit %>%
  predict(new_data = housing_train_prepped) %>%
  bind_cols(housing_train_y) %>%
  rmse(truth = medv, estimate = .pred)


#predict on the test data
lasso_pred <- lasso_fit %>%
  predict(new_data = housing_test_prepped) %>%
  bind_cols(housing_test_y)

#out-of-sample RMSE
lasso_pred %>%
  rmse(truth = medv, estimate = .pred)

#plot the predictions
lasso_pred %>%
  ggplot(aes(x = medv, y = .pred)) +
  geom_point(color="red", alpha=0.5) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Lasso Predictions vs. Actual",
       x = "Actual Medv",
       y = "Predicted Medv") +
  theme_minimal()


#ridge regression
tune_ridge <- linear_reg(
  penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

#workflow
ridge_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_ridge)

#tune the model
ridge_tune <- tune_grid(
  ridge_wf,
  resamples = rec_folds,
  grid = lambda_grid,
  control = control_grid(save_pred = TRUE)
)

#plot the results
ridge_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_line(color="skyblue", alpha=0.7) +
  geom_point(color="skyblue", alpha=0.7) +
  scale_x_log10() +
  labs(title = "Ridge CV RMSE vs. Penalty",
       x = "Lambda (log scale)",
       y = "RMSE") +
  theme_minimal()

#select the best penalty
ridge_best <- ridge_tune %>%
  select_best()
ridge_best

#fit the ridge model to the training data
ridge_fit <- ridge_wf %>%
  finalize_workflow(ridge_best) %>%
  fit(data = housing_train_prepped)

#in-sample RMSE
ridge_fit %>%
  predict(new_data = housing_train_prepped) %>%
  bind_cols(housing_train_y) %>%
  rmse(truth = medv, estimate = .pred)

#predict on the test data
ridge_pred <- ridge_fit %>%
  predict(new_data = housing_test_prepped) %>%
  bind_cols(housing_test_y)

#out-of-sample RMSE
ridge_pred %>%
  rmse(truth = medv, estimate = .pred)

#plot the predictions
ridge_pred %>%
  ggplot(aes(x = medv, y = .pred)) +
  geom_point(color="skyblue", alpha=0.7) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Ridge Predictions vs. Actual",
       x = "Actual Medv",
       y = "Predicted Medv") +
  theme_minimal()

#elastic net - just for fun
tune_enet <- linear_reg(
  penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# Create a grid of values for the penalty parameter
enet_grid <- grid_regular(penalty(), mixture(), levels = 50)

#workflow
enet_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_enet)

#tune the model
enet_tune <- tune_grid(
  enet_wf,
  resamples = rec_folds,
  grid = enet_grid,
  control = control_grid(save_pred = TRUE))

#plot the results
enet_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_line(color="green", alpha=0.5) +
  geom_point(color="green", alpha=0.5) +
  labs(title = "Elastic Net CV RMSE vs. Penalty",
       x = "Lambda (log scale)",
       y = "RMSE") +
  theme_minimal()

#select the best penalty
enet_best <- enet_tune %>%
  select_best()
enet_best

#fit the elastic net model to the training data
enet_fit <- enet_wf %>%
  finalize_workflow(enet_best) %>%
  fit(data = housing_train_prepped)
#in-sample RMSE
enet_fit %>%
  predict(new_data = housing_train_prepped) %>%
  bind_cols(housing_train_y) %>%
  rmse(truth = medv, estimate = .pred)


#predict on the test data
enet_pred <- enet_fit %>%
  predict(new_data = housing_test_prepped) %>%
  bind_cols(housing_test_y)
#out-of-sample RMSE
enet_pred %>%
  rmse(truth = medv, estimate = .pred)

#plot the predictions
enet_pred %>%
  ggplot(aes(x = medv, y = .pred)) +
  geom_point(color="green", alpha=0.5) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Elastic Net Predictions vs. Actual",
       x = "Actual Medv",
       y = "Predicted Medv") +
  theme_minimal()
