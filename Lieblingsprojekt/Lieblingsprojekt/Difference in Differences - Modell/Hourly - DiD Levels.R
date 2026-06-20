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

# Prepare Pragser Tal hourly on-season data
prags_hourly_onseason <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    hour = hour(datetime)
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(date, hour) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "Pragser Tal",
    treatment = 1
  )

# Prepare Sexten station 34 hourly on-season data
sexten_34_hourly_onseason <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    hour = hour(datetime)
  ) %>%
  filter(
    Ort == "Marcia (verso S. Stefano di Cadore)",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(date, hour) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 91 hourly on-season data
sexten_91_hourly_onseason <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    hour = hour(datetime)
  ) %>%
  filter(
    Ort == "Marcia (Discendente)",
    date >= start_date,
    date <= end_date,
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  group_by(date, hour) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Combine Sexten stations
sexten_hourly_onseason <- bind_rows(
  sexten_34_hourly_onseason,
  sexten_91_hourly_onseason
) %>%
  group_by(date, hour) %>%
  summarise(
    traffic = sum(traffic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "Sexten",
    treatment = 0
  )

# Combine regions and create hourly DiD variables
did_data_hourly_levels <- bind_rows(
  prags_hourly_onseason,
  sexten_hourly_onseason
) %>%
  mutate(
    post = if_else(date >= treatment_date, 1, 0),
    restricted_hour = if_else(hour %in% 10:15, 1, 0),
    hour9 = if_else(hour == 9, 1, 0),
    year = factor(year(date)),
    hour_fe = factor(hour),
    did = treatment * post,
    treatment_restricted = treatment * restricted_hour,
    post_restricted = post * restricted_hour,
    hourly_did = treatment * post * restricted_hour
  )

# Check observations
did_data_hourly_levels %>%
  count(region, post, restricted_hour)

# Model 1: Basic hourly DiD
hourly_model_1_levels <- lm(
  traffic ~ treatment + post + restricted_hour +
    did + treatment_restricted + post_restricted + hourly_did,
  data = did_data_hourly_levels
)

# Model 2: Hour fixed effects
hourly_model_2_levels <- lm(
  traffic ~ treatment + post +
    did + treatment_restricted + post_restricted + hourly_did +
    hour_fe,
  data = did_data_hourly_levels
)

# Model 3: Hour and year fixed effects
hourly_model_3_levels <- lm(
  traffic ~ treatment +
    did + treatment_restricted + post_restricted + hourly_did +
    hour_fe + year,
  data = did_data_hourly_levels
)

# Show robust results in console
coeftest(
  hourly_model_1_levels,
  vcov = vcovHC(hourly_model_1_levels, type = "HC1")
)

coeftest(
  hourly_model_2_levels,
  vcov = vcovHC(hourly_model_2_levels, type = "HC1")
)

coeftest(
  hourly_model_3_levels,
  vcov = vcovHC(hourly_model_3_levels, type = "HC1")
)

# Export hourly DiD results to Excel
modelsummary(
  list(
    "Model 1: Basic" = hourly_model_1_levels,
    "Model 2: Hour FE" = hourly_model_2_levels,
    "Model 3: Hour + Year FE" = hourly_model_3_levels
  ),
  vcov = list(
    vcovHC(hourly_model_1_levels, type = "HC1"),
    vcovHC(hourly_model_2_levels, type = "HC1"),
    vcovHC(hourly_model_3_levels, type = "HC1")
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "did_hourly_levels_results.xlsx"
)

# Export hourly DiD results to HTML
modelsummary(
  list(
    "Model 1: Basic" = hourly_model_1_levels,
    "Model 2: Hour FE" = hourly_model_2_levels,
    "Model 3: Hour + Year FE" = hourly_model_3_levels
  ),
  vcov = list(
    vcovHC(hourly_model_1_levels, type = "HC1"),
    vcovHC(hourly_model_2_levels, type = "HC1"),
    vcovHC(hourly_model_3_levels, type = "HC1")
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "did_hourly_levels_results.html"
)