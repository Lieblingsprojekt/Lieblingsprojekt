# Load packages
library(dplyr)
library(lubridate)
library(modelsummary)

# Combine weekly pre-treatment data
pretrend_data <- bind_rows(
  prags_weekly %>%
    mutate(
      region = "Pragser Tal",
      treatment = 1
    ),
  sexten_weekly %>%
    mutate(
      region = "Sexten",
      treatment = 0
    )
)

# Create trend and month variables
pretrend_data <- pretrend_data %>%
  arrange(week) %>%
  mutate(
    trend = as.numeric(factor(week, levels = sort(unique(week)))),
    month = factor(month(week))
  )

# Model 1: Simple pre-trends regression
pretrend_model_1 <- lm(
  traffic ~ treatment + trend + treatment:trend,
  data = pretrend_data
)

# Model 2: Pre-trends regression with month controls
pretrend_model_2 <- lm(
  traffic ~ treatment + trend + treatment:trend + month,
  data = pretrend_data
)

# Show model summaries in console
summary(pretrend_model_1)
summary(pretrend_model_2)

# Create combined regression table
modelsummary(
  list(
    "Model 1: Basic" = pretrend_model_1,
    "Model 2: Month controls" = pretrend_model_2
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "markdown"
)

# Save regression table as Excel
modelsummary(
  list(
    "Model 1" = pretrend_model_1,
    "Model 2" = pretrend_model_2
  ),
  output = "pretrend_regression_table.xlsx"
)