# PREDICTING CHURN WITH H2O AND LIME ----

library(tidyverse)
library(tidyquant)
library(forcats)
library(stringr)

# Function to convert counts to percentages. Works well with dplyr::count()
count_to_pct <- function(data, ..., col = n) {
    
    grouping_vars_expr <- quos(...)
    col_expr <- enquo(col)
    
    ret <- data %>%
        group_by(!!! grouping_vars_expr) %>%
        mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
        ungroup()
    
    return(ret)
    
}

# Function to assess churn versus a baseline
assess_churn <- function(data, churn_col, churn_value, baseline_pct) {
    
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

# Function to plot churn
plot_churn <- function(data, ..., .value, 
                           fct_reorder = TRUE, 
                           fct_rev = FALSE, 
                           include_lbl = TRUE, 
                           color = palette_light()[[1]], 
                           units = c("0", "K", "M")) {
    
    
    # Inputs
    
    group_vars_expr <- quos(...)
    if (length(group_vars_expr) == 0) 
        group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
    
    value_expr <- enquo(.value)
    value_name <- quo_name(value_expr)
    
    units_val <- switch(units[[1]],
                        "M" = 1e6,
                        "K" = 1e3,
                        "0"  = 1)
    if (units[[1]] == "0") units <- ""
    
    
    # Data Manipulation
    usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
    
    data_manipulated <- data %>%
        mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>% 
        mutate(value_text = str_c(usd(!! value_expr / units_val), 
                                  units[[1]], sep = ""))
    
    
    if (fct_reorder) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
            arrange(name)
    }
    
    if (fct_rev) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_rev(name)) %>%
            arrange(name)
    }
    
    # Visualization
    
    g <- data_manipulated %>%
        ggplot(aes_string(x = value_name, y = "name")) +
        geom_segment(aes(xend = 0, yend = name), color = color) +
        geom_point(aes_string(size = value_name), color = color) +
        scale_x_continuous(labels = scales::dollar) +
        theme_tq() +
        scale_size(range = c(3, 5)) +
        theme(legend.position = "none")
    
    
    if (include_lbl) {
        g <- g +
            geom_label(aes_string(label = "value_text", size = value_name), 
                       hjust = "inward", color = color) 
    }
    
    return(g)
    
}