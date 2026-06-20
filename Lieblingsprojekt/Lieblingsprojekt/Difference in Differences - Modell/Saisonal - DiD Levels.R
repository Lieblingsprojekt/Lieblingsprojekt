# Load packages
library(dplyr)
library(lubridate)
library(modelsummary)
library(sandwich)
library(lmtest)

# Define dates
start_date <- as.Date("2020-07-18")
end_date <- as.Date("2025-02-25")
treatment_date <- as.Date("2022-07-10")

# Prepare Pragser Tal weekly on-season data
prags_weekly_onseason <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "Pragser Tal",
    treatment = 1
  )

# Prepare Sexten station 34 weekly on-season data
sexten_34_weekly_onseason <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (verso S. Stefano di Cadore)",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 91 weekly on-season data
sexten_91_weekly_onseason <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (Discendente)",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Combine Sexten stations
sexten_weekly_onseason <- bind_rows(
  sexten_34_weekly_onseason,
  sexten_91_weekly_onseason
) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(traffic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "Sexten",
    treatment = 0
  )

# Combine regions and create DiD variables
did_data_onseason_levels <- bind_rows(
  prags_weekly_onseason,
  sexten_weekly_onseason
) %>%
  filter(
    week >= start_date,
    week <= end_date
  ) %>%
  mutate(
    post = if_else(week >= treatment_date, 1, 0),
    did = treatment * post,
    month = factor(month(week)),
    year = factor(year(week))
  )

# Estimate on-season DiD models in levels
did_onseason_model_1_levels <- lm(
  traffic ~ treatment + post + did,
  data = did_data_onseason_levels
)

did_onseason_model_2_levels <- lm(
  traffic ~ treatment + post + did + month,
  data = did_data_onseason_levels
)

did_onseason_model_3_levels <- lm(
  traffic ~ treatment + post + did + month + year,
  data = did_data_onseason_levels
)

# Show robust results in console
coeftest(
  did_onseason_model_1_levels,
  vcov = vcovHC(did_onseason_model_1_levels, type = "HC1")
)

coeftest(
  did_onseason_model_2_levels,
  vcov = vcovHC(did_onseason_model_2_levels, type = "HC1")
)

coeftest(
  did_onseason_model_3_levels,
  vcov = vcovHC(did_onseason_model_3_levels, type = "HC1")
)

# Export regression table to Excel
modelsummary(
  list(
    "Model 1: Baseline" = did_onseason_model_1_levels,
    "Model 2: Month FE" = did_onseason_model_2_levels,
    "Model 3: Month + Year FE" = did_onseason_model_3_levels
  ),
  vcov = list(
    vcovHC(did_onseason_model_1_levels, type = "HC1"),
    vcovHC(did_onseason_model_2_levels, type = "HC1"),
    vcovHC(did_onseason_model_3_levels, type = "HC1")
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "did_onseason_levels_results.xlsx"
)

# Export regression table to HTML
modelsummary(
  list(
    "Model 1: Baseline" = did_onseason_model_1_levels,
    "Model 2: Month FE" = did_onseason_model_2_levels,
    "Model 3: Month + Year FE" = did_onseason_model_3_levels
  ),
  vcov = list(
    vcovHC(did_onseason_model_1_levels, type = "HC1"),
    vcovHC(did_onseason_model_2_levels, type = "HC1"),
    vcovHC(did_onseason_model_3_levels, type = "HC1")
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "did_onseason_levels_results.html"
)

# Check number of observations
did_data_onseason_levels %>%
  count(region, post)