# DATA PREPARATION - CUSTOMER CHURN
# 

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(stringr)
library(forcats)
library(recipes)
library(stats)
library(ggplot2)

source("00_Scripts/plot_hist_facet.R")
source("00_Scripts/plot_cor.R")

# Load data

path_train <- "00_Data/WA_Fn-UseC_-Telco-Customer-Churn.csv"
raw_tbl <- read_csv(path_train, col_names = TRUE)

train_indices <- createDataPartition(y = raw_tbl[["Churn"]],
                                     p = 0.7,
                                     list = FALSE)

train_raw_tbl <- raw_tbl[train_indices,]

test_raw_tbl <- raw_tbl[-train_indices,]

glimpse(train_raw_tbl)

# Convert character data into factors
train_raw_tbl %>%
  select_if(is.character) %>%
  glimpse()

train_raw_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- train_raw_tbl %>%
  select(-customerID) %>%
  mutate_if(is.character, as.factor)

data_processed_tbl %>%
  select_if(is.factor) %>%
  map(levels)

# Processing Pipeline ----

train_readable_tbl <- train_raw_tbl %>%
  select(-customerID) %>%
  mutate_if(is.character, as.factor)

test_readable_tbl <- test_raw_tbl %>%
  select(-customerID) %>%
  mutate_if(is.character, as.factor)

train_readable_tbl %>%
  relocate(Churn) %>%
  plot_hist_facet(bins = 10, ncol = 5)

# Data preprocessing with recipes ----

# Plan
# 1. Impute for missing data or remove features with zero variance
# 2. Transformations
# 3. Discretise - binning can hurt correlation analysis and should be avoided for regression analysis
# 4. Dummy Variables
# 5. Interaction variable
# 6. Normalisation
# 7. Multivariate Transformations

# 1. Zero variance features ----
# Using recipe() from the purrr package

recipe_obj <- recipe(Churn ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj


prep_train_readable_tbl <- recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(where(is.numeric)) %>%
  plot_hist_facet(bins = 10, ncol = 5)

# Numerical senior citizen data is categorical

# 2. Transformations ----

skewed_feature_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = T) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.8) %>%
  filter(!key %in% c("SeniorCitizen")) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()

factor_names <- c("SeniorCitizen")

factor_names

recipe_obj <- recipe(Churn ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>%
  bake(train_readable_tbl) %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()

# 3. Centre/Scaling ----

train_readable_tbl %>%
  select_if(is.numeric) %>%
  plot_hist_facet()

recipe_obj <- recipe(Churn ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

prepared_recipe <- recipe_obj %>% 
  prep() 

# Inspect the contents of the 4th object in the list that contains the output of the step_center() fn

#Before prep()
recipe_obj$steps[[4]]
# After prep()
prepared_recipe$steps[[4]]

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select_if(is.numeric) %>%
  plot_hist_facet()

# 4. Dummy Variables ----
# step_dummy() expands the categorical factor data into columns with numerical flags of 1s and 0s.
# The number of dummy variables is one less that the distinct number of categories because
# the absence of all flags signifies the 

dummied_recipe_obj <- recipe(Churn ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal())

dummied_recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(contains("SeniorCitizen")) %>%
  plot_hist_facet(ncol = 3)

train_readable_tbl %>%
  select(SeniorCitizen) %>%
  distinct() %>%
  arrange(SeniorCitizen)

## Final Recipe ----

recipe_obj <- recipe(Churn ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, train_readable_tbl)

train_tbl %>% glimpse()

test_tbl <- bake(recipe_obj, test_readable_tbl)

# THe tidy() function returns a dataframe containing information about the recipe
# When adding the argument number = ... to the tidy() function a tidy method is executed for that 
# operation in the recipe.  For example, if the recipe step 4 is step_center(all.numeric()) then
# recipe_obj %>% tidy(number = 4) will return the centres for each numeric.

recipe_obj  %>%
  tidy()


recipe_obj %>%
  tidy(number = 6)

# KNN Impute : The following code applies a K-Nearest Neighbors imputation to only the numeric features but not the target?
# step_knnimpute(all_numeric(), -all_outcomes())
# YeoJohnson: The followingstep function works best for transforming skewed numeric data of any positive or negative real-numbers?
# step_YeoJohnson
# Normalisation: 
# step_center() %>% step_scale()


# Correlation Analysis ----

data <- train_tbl
glimpse(train_tbl)

train_tbl %>%
  get_cor(Churn_Yes,
          use = "pairwise.complete.obs",
          fct_reorder = T,
          fct_rev = T)

train_tbl %>%
  select(-Churn_No) %>%
  plot_cor(Churn_Yes, fct_reorder = TRUE, fct_rev = FALSE)

# Correlation Evaluation ----

# Identify highly correlated features

train_tbl %>%
  select(Churn_Yes, contains("Security"), contains("Contract"), contains("Charges"), 
         contains("Internet"), contains("Payment"),  contains("Billing"), tenure,
         contains("Online")) %>%
  plot_cor(Churn_Yes, fct_reorder = TRUE, fct_rev = FALSE)

# Notice that no internet service precludes have a range of features.
# No internet service appears to be predictive

