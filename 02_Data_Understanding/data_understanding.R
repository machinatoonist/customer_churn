# DATA UNDERSTANDING ----

# renv::init()  # Use to initialise renv.lock file
# renv::status()
# renv::history() # Use to get id for previous commit
# renv::snapshot() # Use to update renv.lock file

# Libraries

library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)
library(recipes)
library(caret)

# Load data

path_train <- "00_Data/WA_Fn-UseC_-Telco-Customer-Churn.csv"
raw_tbl <- read_csv(path_train, col_names = TRUE)

train_indices <- createDataPartition(y = raw_tbl[["Churn"]],
                                     p = 0.7,
                                     list = FALSE)

train_data_tbl <- raw_tbl[train_indices,]

test_data_tbl <- raw_tbl[-train_indices,]

glimpse(train_raw_tbl)

# Exploratory Data Analysis (EDA) ----

#Step 1: Data Summarisation ----

skim(train_raw_tbl)

# Character Data Type

train_raw_tbl %>%
  select_if(is.character) %>%
  glimpse()

train_raw_tbl %>%
  select_if(is.character) %>%
  map(unique)

# Gauge the relative frequency of each categorical variable
train_raw_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric Data
train_raw_tbl %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())

train_raw_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  filter(value < 10)

# There are only 2 unique variables for senior citizens so may be better treated as a categorical variable

train_raw_tbl %>%
  select(SeniorCitizen) %>%
  unique()
  

# Numeric values that have less than 10 unique values may be discrete values that may be better
# to treat as categorical data.  Unique counts over 10 are more likely to be continuous variable

# Step 2: Data Visualisation ----


train_raw_tbl %>%
  select(Churn, Partner, Dependents, SeniorCitizen) %>%
  ggpairs()

# Demographic relationships with Churn
train_raw_tbl %>%
  select(Churn, Partner, Dependents, SeniorCitizen) %>%
  ggpairs(aes(color = Churn), lower = "blank", legend = 1,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Billina relationships with Churn
train_raw_tbl %>%
  select(Churn, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, Contract) %>%
  ggpairs(aes(color = Churn), lower = "blank", legend = 1,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Service level relationships with Churn
# No internet service is predictive of no churn but may not be best customer 

train_raw_tbl %>%
  select(Churn, PhoneService, MultipleLines, InternetService) %>%
  ggpairs(aes(color = Churn), lower = "blank", legend = 1,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Internet Service relationships with Churn
# 
train_raw_tbl %>%
  select(Churn, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingMovies, StreamingTV) %>%
  ggpairs(aes(color = Churn), lower = "blank", legend = 1,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

data <- train_raw_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)

plot_ggpairs <- function(data, 
                         colour = NULL,
                         densityAlpha = 0.5){
  colour_expr <- enquo(colour)
  
  if(rlang::quo_is_null(colour_expr)){
    
    g <- data %>%
      ggpairs(lower = "blank")
    
    } else {
      
      colour_name <- quo_name(colour_expr)
      g <- data %>%
        ggpairs(mapping = aes_string(colour = colour_name),
            lower = "blank", legend = 1,
            diag = list(continuous = wrap("densityDiag", 
                                          alpha = densityAlpha))) +
        theme(legend.position = "bottom")
    }
  
  return(g)
  
}

