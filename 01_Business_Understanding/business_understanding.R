# BUSINESS UNDERSTANDING ----
# Libraries  ----

library(forcats)
library(tidyverse)
library(tidyquant)
# library(readxl)
library(stringr)
library(readr)
library(glue)

# Load Data
# Source of data: https://www.kaggle.com/blastchar/telco-customer-churn

path_train <- "00_Data/WA_Fn-UseC_-Telco-Customer-Churn.csv"
raw_tbl <- read_csv(path_train, col_names = TRUE)

glimpse(raw_tbl)

inspect_tbl <- raw_tbl %>%
  select(customerID, gender, Dependents, # demographic
         PaperlessBilling, PaymentMethod, MonthlyCharges,# billing
         StreamingTV, tenure, DeviceProtection, OnlineBackup, #engagement
         Churn)

# 1. Business Science Problem Framework ----

# 1A. View Business As A Machine ----

# Define Objectives: Retain Good Customers
# Assess Outcomes: 

inspect_tbl %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

# 1B. Understand The Drivers ----
# Investigate Objectives: 
# Synthesise Outcomes: High counts and high percentages
# Hypothesise Drivers: Billing process, engagement

# Billing Process ----
inspect_tbl %>%
  
  group_by(PaperlessBilling, Churn) %>%     
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(PaperlessBilling) %>%                
  mutate(pct = n/sum(n))

# Engagement ----
inspect_tbl %>%
  
  group_by(PaperlessBilling, StreamingTV, DeviceProtection, Churn) %>%     
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(PaperlessBilling, StreamingTV, DeviceProtection) %>%                
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  
  filter(Churn %in% c("Yes"))

# Notice that having Device Protection appears to be correlated with lower 
# probability of churn
  
# 1C. Measure the Drivers ----

# Collect information on customer churn

# Develop KPI's: Industry KPI for monthly wireless customer churn 1.9 percent or 2.1 percent per month. 
# Annual churn rates for telecommunications companies average between 21 percent. 
# (Source: https://www.statista.com/statistics/816735/customer-churn-rate-by-industry-us/) accessed 15/01/2021
# The churn in the kaggle dataset is significantly greater than this for one month.  
# Will assume that this dataset has been annualised
# Feature investigation

glimpse(inspect_tbl)
glimpse(raw_tbl)

# Feature categories:
# 1) Demographic
# 2) Billing method
# 3) Engagement
# 4) Price
# 5) Service

# Additional data that would be recommended to enhance the predictive power of the modeling
# - Adding survey type data to gauge customer satisfaction
# - Time series data to understand seasonality and any life cycle effects

inspect_tbl %>%
  
  group_by(PaperlessBilling, StreamingTV, DeviceProtection, Churn) %>%    
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(PaperlessBilling, StreamingTV, DeviceProtection) %>%               
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  
  filter(Churn %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.21 ~ "Yes",
      TRUE ~ "No"
    )
  )

# 1D; Uncover Problems and Opportunities ----

# Estimate Lifetime Customer Value
# See Ash Maurya - Running Lean

calculate_churn_cost <- function(
  
  avg_annual_subscription_revenue = 1000,
  
  # Direct Costs	
  acquisition_cost = 500,
  disconnection_fees = 50,
  annual_subscriber_cost = 20
) {
  
  # Direct Costs
  direct_costs <- sum(acquisition_cost, disconnection_fees)
  
  # Lost Revenue Costs
  lost_revenue_cost <- avg_annual_subscription_revenue
  
  # Savings of Subscriber cost
  subscriber_cost_reduction <- annual_subscriber_cost
  
  total_cost <- direct_costs + lost_revenue_cost - subscriber_cost_reduction
  
  return(total_cost)

}
calculate_churn_cost()

# Calculate Cost By Category ----

inspect_tbl %>%
  
  group_by(PaperlessBilling, PaymentMethod, Churn) %>%    
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(PaperlessBilling, PaymentMethod) %>%               
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  
  filter(Churn %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.21 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  mutate(
    cost = calculate_churn_cost()
  )

# Workflow of Churn ----
count_to_pct <- function(data, ..., col = n){
  
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%   
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr)/sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}

inspect_tbl %>%
  
  count(PaperlessBilling, PaymentMethod, Churn) %>%
  
  count_to_pct(PaperlessBilling, PaymentMethod) %>%
  
  filter(Churn %in% c("Yes")) %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.21 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  mutate(
    cost = calculate_churn_cost() 
  )



assess_churn <- function(data, churn_col, churn_value, baseline_pct){
  
  churn_col_expr <- enquo(churn_col)
  
  data %>% 
    filter((!! churn_col_expr) %in% churn_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
  
}

# Summary table utilising functions ====

inspect_tbl %>%
  
  count(PaperlessBilling, PaymentMethod, Churn) %>%
  count_to_pct(PaperlessBilling, PaymentMethod) %>%
  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.21) %>%
  mutate(
    cost_of_churn= calculate_churn_cost()
  ) 
  
# Visualisation of Churn Cost ----

inspect_tbl %>%
  
  count(PaperlessBilling, PaymentMethod, Churn) %>%
  count_to_pct(PaperlessBilling, PaymentMethod) %>%
  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.21) %>%
  mutate(
    cost_of_churn= calculate_churn_cost()
  )  %>%
  
  # Data manipulation
  mutate(name = glue("{PaperlessBilling} Paperless: {PaymentMethod}") %>% as_factor()) %>%
  # pull(name) %>%
  # levels()
  mutate(name = fct_reorder(name, cost_of_churn)) %>%
  mutate(cost_text = str_c("$",format(cost_of_churn / 1e3, digits = 2), "k", sep = "")) %>%
  
  # Plotting
  
  ggplot(aes(x = cost_of_churn, y = name)) + 
  geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
  geom_point(aes(size = cost_of_churn), color = palette_light()[[1]]) +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_churn),
             hjust = "inward", color = palette_light()[[1]]) +
  theme_tq() +
  scale_size(range = c(3,5)) +
  labs(title = "Estimated Cost of Churn: By Payment Type and Delivery",
       y = "", x = "Cost of Churn") +
  theme(legend.position = "none")
  

plot_churn <- function(data, ..., .value,
                           fct_reorder = TRUE,
                           fct_rev = FALSE,
                           include_lbl = TRUE,
                           color = palette_light()[[1]],
                           units = c("0", "K", "M")){
  
  # Inputs
  
  group_vars_expr <- quos(...)
  if(length(group_vars_expr) == 0)
    group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
  
  value_expr <- enquo(.value)
  value_name <- quo_name(value_expr)
  
  units_val <- switch(units[[1]],
                      "M" = 1e6,
                      "K" = 1e3,
                      "0" = 1)
  if(units[[1]] == "0") units <- ""
                    
  
  # Data Manipulation
  # Function factory application using scales library
  usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)

  data_manipulated <- data %>%
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
    mutate(value_text = str_c(usd(!! value_expr / units_val), units[[1]], sep = ""))
  
  if(fct_reorder) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
      arrange(name)
    
  }
  
  if(fct_rev) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_rev(name)) %>%
      arrange(name)
  }
  
  # Visualisation
  
  g <- data_manipulated %>%
    ggplot(aes_string(x = value_name, y = "name")) + 
    geom_segment(aes(xend = 0, yend = name), color = color) +
    geom_point(aes_string(size = value_name), color = color) +
    scale_x_continuous(labels = scales::dollar) +
    theme_tq() +
    scale_size(range = c(3,5)) +
    theme(legend.position = "none")
  
  if(include_lbl){
    
    g <- g + geom_label(aes_string(label = "value_text", size = value_name),
               hjust = "inward", color = color)
    
  }
  
  return(g)
  
}


inspect_tbl %>%
  count(PaperlessBilling, PaymentMethod, Churn) %>%
  count_to_pct(PaperlessBilling, PaymentMethod) %>%
  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.21) %>%
  mutate(
    cost_of_churn = calculate_churn_cost()
  ) %>%
  plot_churn(PaperlessBilling, .value = cost_of_churn, units = "K") +
  labs(title = "Estimated Cost of Customer Churn: By Payment Type and Delivery",
       y = "", x = "Cost of Churn",
       subtitle = "Drivers of Churn Cost by Payment Type and Delivery")
  
