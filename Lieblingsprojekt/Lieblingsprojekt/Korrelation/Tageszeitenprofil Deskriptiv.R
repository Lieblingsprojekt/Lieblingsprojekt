# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Prepare data
prags_hourly <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    hour = hour(datetime)
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    date >= as.Date("2020-07-18"),
    date <= as.Date("2025-02-25"),
    (month(date) == 7 & day(date) >= 10) |
      month(date) == 8 |
      (month(date) == 9 & day(date) <= 10)
  ) %>%
  mutate(
    treatment_period = if_else(
      date < as.Date("2022-07-10"),
      "Pre-Treatment",
      "Post-Treatment"
    )
  )

# Calculate daily totals
daily_totals <- prags_hourly %>%
  group_by(date) %>%
  summarise(
    daily_total = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate hourly share of daily traffic
hourly_share <- prags_hourly %>%
  left_join(
    daily_totals,
    by = "date"
  ) %>%
  mutate(
    traffic_share = Insgesamt / daily_total * 100
  ) %>%
  group_by(
    treatment_period,
    hour
  ) %>%
  summarise(
    mean_share = mean(
      traffic_share,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Create relative profile plot
p_share <- ggplot(
  hourly_share,
  aes(
    x = hour,
    y = mean_share,
    color = treatment_period
  )
) +
  annotate(
    "rect",
    xmin = 9.5,
    xmax = 16,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 0:23
  ) +
  labs(
    title = "Relative Hourly Traffic Profile",
    subtitle = "Share of daily traffic by hour (Pragser Tal)",
    x = "Hour of Day",
    y = "Share of Daily Traffic (%)",
    color = "Period"
  ) +
  theme_minimal(
    base_size = 14
  )

# Display plot
print(p_share)

# Save plot
ggsave(
  filename = "prags_relative_hourly_profile.png",
  plot = p_share,
  width = 12,
  height = 7,
  dpi = 300
)

# Calculate hourly percentage changes
hourly_change <- hourly_share %>%
  select(
    treatment_period,
    hour,
    mean_share
  ) %>%
  tidyr::pivot_wider(
    names_from = treatment_period,
    values_from = mean_share
  ) %>%
  mutate(
    percent_change =
      (`Post-Treatment` - `Pre-Treatment`) /
      `Pre-Treatment` * 100
  )

# Display table
print(hourly_change)

# Create percentage change plot
p_change <- ggplot(
  hourly_change,
  aes(
    x = factor(hour),
    y = percent_change
  )
) +
  annotate(
    "rect",
    xmin = 10,
    xmax = 17,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15
  ) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Hourly Change After Treatment",
    subtitle = "Percentage change in traffic share by hour",
    x = "Hour of Day",
    y = "Change (%)"
  ) +
  theme_minimal(
    base_size = 14
  )

# Display plot
print(p_change)

# Save plot
ggsave(
  filename = "prags_hourly_percentage_change.png",
  plot = p_change,
  width = 12,
  height = 7,
  dpi = 300
)