# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(writexl)

# Define dates
treatment_date <- as.Date("2022-07-10")
end_date <- as.Date("2025-02-25")

# Prepare Pragser Tal daily data
prags_daily <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime)
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    date >= as.Date("2020-07-18"),
    date <= end_date
  ) %>%
  group_by(date) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = "Pragser Tal")

# Prepare Sexten station 34 daily data
sexten_34_daily <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime)
  ) %>%
  filter(
    Ort == "Marcia (verso S. Stefano di Cadore)",
    date >= as.Date("2020-07-18"),
    date <= end_date
  ) %>%
  group_by(date) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 91 daily data
sexten_91_daily <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime)
  ) %>%
  filter(
    Ort == "Marcia (Discendente)",
    date >= as.Date("2020-07-18"),
    date <= end_date
  ) %>%
  group_by(date) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Combine Sexten stations
sexten_daily <- bind_rows(
  sexten_34_daily,
  sexten_91_daily
) %>%
  group_by(date) %>%
  summarise(
    traffic = sum(traffic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = "Sexten")

# Combine regions and define periods
season_data <- bind_rows(
  prags_daily,
  sexten_daily
) %>%
  mutate(
    treatment_period = if_else(
      date < treatment_date,
      "Pre-Treatment",
      "Post-Treatment"
    ),
    season = if_else(
      (month(date) == 7 & day(date) >= 10) |
        month(date) == 8 |
        (month(date) == 9 & day(date) <= 10),
      "On-Season",
      "Off-Season"
    )
  )

# Summary table
season_summary <- season_data %>%
  group_by(region, treatment_period, season) %>%
  summarise(
    mean_daily_traffic = mean(traffic, na.rm = TRUE),
    median_daily_traffic = median(traffic, na.rm = TRUE),
    number_of_days = n(),
    .groups = "drop"
  )

# Display summary table
print(season_summary)

write_xlsx(
  season_changes,
  "season_changes.xlsx"
)

# Calculate percentage changes from Pre to Post
season_changes <- season_summary %>%
  select(region, treatment_period, season, mean_daily_traffic) %>%
  tidyr::pivot_wider(
    names_from = treatment_period,
    values_from = mean_daily_traffic
  ) %>%
  mutate(
    percent_change = (`Post-Treatment` / `Pre-Treatment` - 1) * 100
  )

# Display percentage changes
print(season_changes)

# Create percentage change plot
p_season_change <- ggplot(
  season_changes,
  aes(
    x = season,
    y = percent_change,
    fill = region
  )
) +
  geom_col(
    position = "dodge"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = paste0(round(percent_change, 1), "%")
    ),
    position = position_dodge(width = 0.9),
    vjust = ifelse(season_changes$percent_change >= 0, -0.4, 1.2),
    size = 4
  ) +
  labs(
    title = "Seasonal Change in Average Daily Traffic",
    subtitle = "Percentage change from pre-treatment to post-treatment period",
    x = "Season",
    y = "Change in Average Daily Traffic (%)",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14)

# Display plot
print(p_season_change)

# Save plot
ggsave(
  filename = "season_percentage_change_prags_sexten.png",
  plot = p_season_change,
  width = 10,
  height = 7,
  dpi = 300
)

# Display plot
print(p_season)

# Save plot
ggsave(
  filename = "season_comparison_prags_sexten.png",
  plot = p_season,
  width = 12,
  height = 7,
  dpi = 300
)