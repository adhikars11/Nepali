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
library(lpirfs)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggplotify)  # For as.ggplot()
library(grid)       # For grid.grabExpr()

# ------------------------------------------------------------------------------
# 1. Data Preparation
# ------------------------------------------------------------------------------

# Create an indicator for Nepal being grey-listed from 2008 to 2014
imf_nepal$grey_list <- as.integer(imf_nepal$year >= 2008 & imf_nepal$year <= 2014)

# Sort imf_nepal by year if not already
imf_nepal <- imf_nepal[order(imf_nepal$year), ]

# Optionally, create logged variables (ensure values are positive)
imf_nepal <- imf_nepal %>%
  mutate(log_gdp = log(gdp_current_prices_billions_of_u_s_dollars),
         log_net_lend = log(primary_net_lending_borrowing_also_referred_as_primary_balance_percent_of_gdp))

# ------------------------------------------------------------------------------
# 2. Helper Function Using lp_lin()
# ------------------------------------------------------------------------------

# This function subsets the data to two columns (outcome, shock) and runs lp_lin().
run_lp_lin_simple <- function(data, outcome_var, shock_var, horizon = 3, lags = 1) {
  # Subset the required columns and ensure it is a data.frame
  endog_data <- data[, c(outcome_var, shock_var)]
  endog_data <- as.data.frame(endog_data)
  
  # Rename columns so that the first is the outcome and the second is the shock.
  colnames(endog_data) <- c(outcome_var, shock_var)
  
  # Run lp_lin() with a trend, 95% confidence bands, and a unit shock.
  res <- lp_lin(
    endog_data     = endog_data,
    lags_endog_lin = lags,
    hor            = horizon,
    trend          = TRUE,
    confint        = 1.96,
    shock_type     = 1
  )
  
  return(res)
}

# ------------------------------------------------------------------------------
# 3. Run Local Projections for Each Outcome Variable
# ------------------------------------------------------------------------------

# Identify numeric variables and exclude "year" and "grey_list"
numeric_vars <- names(imf_nepal)[sapply(imf_nepal, is.numeric)]
outcome_vars <- setdiff(numeric_vars, c("year", "grey_list"))

# Loop over each outcome variable and store the lp_lin() result in a list.
lp_results_list <- list()
for (v in outcome_vars) {
  lp_obj <- tryCatch({
    run_lp_lin_simple(
      data        = imf_nepal,
      outcome_var = v,
      shock_var   = "grey_list",
      horizon     = 3,
      lags        = 1
    )
  }, error = function(e) {
    message("Error for variable ", v, ": ", e$message)
    return(NULL)
  })
  lp_results_list[[v]] <- lp_obj
}

# Remove any outcomes where estimation failed
lp_results_list <- lp_results_list[!sapply(lp_results_list, is.null)]

for (var_name in names(lp_results_list)) {
  obj <- lp_results_list[[var_name]]
  
  obj_irf <- plot(obj)
  
  print(obj_irf)  # display each plot
}

