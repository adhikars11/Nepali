# Shisham Adhikari, UC Davis
# Aug 15, 2024

# Clear environment
rm(list=ls()) 

# Install and load packages
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, readxl, readr, ggforce, lubridate, janitor, zoo, plm, foreign, haven, parallel, data.table)

base <- "/Users/shishamadhikari/Desktop/temperature_shocks"

###############################################################################################
# 1. Data preprocessing
###############################################################################################

# Read and preprocess the data
nepal_year <- read_csv("data/GMD.csv") %>% 
  clean_names() %>%
  filter(iso3 == "NPL") %>%
  filter(year > 1959, year <= 2023)

# Reshape the data to long format so each variable becomes a facet
nepal_data <- nepal_year %>% 
  select(year, where(is.numeric)) %>% 
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")

###############################################################################################
# 2. Basic Summary Statistics & Missing Value Visualization
###############################################################################################

# Summary statistics using base R
print(summary(nepal_year))

# Using skimr for a detailed summary
skimr::skim(nepal_year)

###############################################################################################
# 3. Exploratory Data Analysis for Distributions
###############################################################################################

# For non-numeric variables (e.g., countryname, iso3), view their frequency
nepal_year %>% 
  select(where(is.character)) %>% 
  map(~table(.)) %>% 
  print()

# For numeric variables, visualize distributions using histograms
df_long <- nepal_year %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Suppose you want 9 plots per page
p <- ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap_paginate(~ variable, scales = "free", ncol = 3, nrow = 3, page = 1) +
  theme_minimal()

# Number of pages needed
num_pages <- ceiling(length(unique(df_long$variable)) / 9)

# Loop through each page
for (i in seq_len(num_pages)) {
  p_page <- p + facet_wrap_paginate(~ variable, scales = "free", ncol = 3, nrow = 3, page = i)
  print(p_page)
}

###############################################################################################
# 4. Time-Series Plots for Numeric Variables
###############################################################################################

# Select numeric columns along with year
nepal_numeric <- nepal_year %>% 
  select(year, where(is.numeric))

df_long <- nepal_year %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")

# Create a base plot
# 1) The geom_rect() call goes before geom_line() so that the grey box is behind the line
# 2) Set inherit.aes = FALSE so that the rect doesn't try to use x = year, y = value
p <- ggplot(df_long, aes(x = year, y = value)) +
  geom_rect(
    aes(xmin = 2008, xmax = 2014, ymin = -Inf, ymax = Inf),
    fill = "grey", alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_line(color = "blue") +
  facet_wrap_paginate(~ variable, scales = "free_y", ncol = 3, nrow = 3, page = 1) +
  labs(title = "Time Series of Numeric Variables (Paginated)",
       x = "Year", y = "Value") +
  theme_minimal()

# Determine how many pages you need
num_vars <- length(unique(df_long$variable))
plots_per_page <- 3 * 3  # ncol * nrow
num_pages <- ceiling(num_vars / plots_per_page)

# Loop through each page
for (i in seq_len(num_pages)) {
  p_page <- p + facet_wrap_paginate(~ variable, scales = "free_y", 
                                    ncol = 3, nrow = 3, page = i)
  print(p_page)  # Each print displays a different set of variables
}

###############################################################################################
# 5. IMF data
###############################################################################################

# Define the path to your Excel file
imf_nepal <-  read_excel("/Users/shishamadhikari/Desktop/temperature_shocks/data/imf-nepal.xls") %>%
  janitor::clean_names() # Update this path

# Select numeric columns along with year
df_long <- imf_nepal %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")

# Create a base plot
# 1) The geom_rect() call goes before geom_line() so that the grey box is behind the line
# 2) Set inherit.aes = FALSE so that the rect doesn't try to use x = year, y = value
p <- ggplot(df_long, aes(x = year, y = value)) +
  geom_rect(
    aes(xmin = 2008, xmax = 2014, ymin = -Inf, ymax = Inf),
    fill = "grey", alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_line(color = "blue") +
  facet_wrap_paginate(~ variable, scales = "free_y", ncol = 3, nrow = 3, page = 1) +
  labs(title = "Time Series of Numeric Variables (Paginated)",
       x = "Year", y = "Value") +
  theme_minimal()

# Determine how many pages you need
num_vars <- length(unique(df_long$variable))
plots_per_page <- 3 * 3  # ncol * nrow
num_pages <- ceiling(num_vars / plots_per_page)

# Loop through each page
for (i in seq_len(num_pages)) {
  p_page <- p + facet_wrap_paginate(~ variable, scales = "free_y", 
                                    ncol = 3, nrow = 3, page = i)
  print(p_page)  # Each print displays a different set of variables
}

colnames(imf_nepal)


###############################################################################################
# 5. Local Projection
###############################################################################################
# 1) Create an indicator for Nepal being grey-listed from 2008 to 2014
imf_nepal$grey_list <- as.integer(imf_nepal$year >= 2008 & imf_nepal$year <= 2014)

# 2) (Optional) Sort imf_nepal by year if not already
imf_nepal <- imf_nepal[order(imf_nepal$year), ]

imf_nepal <- imf_nepal %>%
  mutate(log_gdp = log(gdp_current_prices_billions_of_u_s_dollars)) %>%
  mutate(log_net_lend = log(primary_net_lending_borrowing_also_referred_as_primary_balance_percent_of_gdp))

# Local Projection Function
run_local_projections <- function(df, y_var, event_var, control_vars = NULL, max_h = 3) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function to work. Please install it.")
  }
  library(dplyr)
  
  results_list <- list()
  controls_formula <- if (!is.null(control_vars) && length(control_vars) > 0) {
    paste(control_vars, collapse = " + ")
  } else {
    "1"
  }
  
  for (h in 0:max_h) {
    # Create a new variable that is the lead of the outcome variable
    y_lead_var <- paste0(y_var, "_lead", h)
    df[[y_lead_var]] <- dplyr::lead(df[[y_var]], n = h)
    
    # Build the regression formula
    formula_str <- paste0(y_lead_var, " ~ ", event_var, " + ", controls_formula)
    model <- lm(as.formula(formula_str), data = df)
    results_list[[paste0("h=", h)]] <- summary(model)
  }
  
  return(results_list)
}

# Identify outcome variables:
# We select numeric columns and exclude 'year' and 'grey_list'
numeric_vars <- names(imf_nepal)[sapply(imf_nepal, is.numeric)]
outcome_vars <- setdiff(numeric_vars, c("year", "grey_list"))

# Set maximum horizon for the local projection
max_h <- 5

# Create an empty data frame to store all IRF results
all_irfs <- data.frame()

# Loop over each numeric variable
for (var in outcome_vars) {
  # Run local projection for current variable with no additional controls
  lp_res <- run_local_projections(
    df = imf_nepal,
    y_var = var,
    event_var = "grey_list",
    control_vars = NULL,
    max_h = max_h
  )
  
  # Create a temporary data frame for IRF for this variable
  df_irf <- data.frame(
    variable = var,
    horizon = 0:max_h,
    coef = NA,
    se = NA
  )
  
  # Extract the coefficient and standard error for 'grey_list' at each horizon
  for (h in 0:max_h) {
    model_summary <- lp_res[[paste0("h=", h)]]
    coef_h <- model_summary$coefficients["grey_list", "Estimate"]
    se_h   <- model_summary$coefficients["grey_list", "Std. Error"]
    
    df_irf[df_irf$horizon == h, "coef"] <- coef_h
    df_irf[df_irf$horizon == h, "se"] <- se_h
  }
  
  # Calculate 95% confidence intervals
  df_irf <- df_irf %>%
    mutate(lower = coef - 1.96 * se,
           upper = coef + 1.96 * se)
  
  # Append the current variable's results to the overall IRF data frame
  all_irfs <- rbind(all_irfs, df_irf)
}

# ------------------------------------------------------------------------------
# 4. Visualize the IRFs for All Variables
# ------------------------------------------------------------------------------

ggplot(all_irfs, aes(x = horizon, y = coef)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Horizon (years)",
    y = "Coefficient on grey_list",
    title = "Impulse Response Functions for All Numeric Variables via Local Projections"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 4. Visualize 4 IRFs at a time
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)

# Get unique variables from your IRF data
unique_vars <- unique(all_irfs$variable)
n_vars <- length(unique_vars)
group_size <- 4  # number of variables per composite plot
num_groups <- ceiling(n_vars / group_size)

# Create an empty list to store each composite plot (page)
plot_pages <- list()

# Loop over groups of 4 variables
for (i in 1:num_groups) {
  
  # Determine indices for the current group
  start_idx <- (i - 1) * group_size + 1
  end_idx   <- min(i * group_size, n_vars)
  
  # Select variables for this group
  selected_vars <- unique_vars[start_idx:end_idx]
  
  # Create individual plots for each selected variable
  plot_list <- lapply(selected_vars, function(v) {
    df_subset <- filter(all_irfs, variable == v)
    ggplot(df_subset, aes(x = horizon, y = coef)) +
      geom_line(color = "blue") +
      geom_point(color = "blue", size = 2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
      labs(title = v, x = "Horizon (years)", y = "Coefficient on grey_list") +
      theme_minimal()
  })
  
  # Arrange the four plots in a 2x2 grid using ggarrange
  combined_plot <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2)
  plot_pages[[i]] <- combined_plot
}

# Display all composite plots (pages)
for (page in plot_pages) {
  print(page)
}





