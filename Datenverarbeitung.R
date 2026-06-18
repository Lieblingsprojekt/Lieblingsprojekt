# Step 1: Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Step 2: Check the imported dataset
View(Daten_Prags)
names(Daten_Prags)
str(Daten_Prags)

# Step 3: Convert the time variable into a proper date-time format
Daten_Prags <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    hour = hour(datetime)
  )

# Step 4: Filter the data for the relevant direction
prags_direction <- Daten_Prags %>%
  filter(Richtung == "SS49 - Rotatoria di Braies")

# Step 5: Aggregate hourly traffic data to daily traffic data
prags_weekly <- prags_direction %>%
  mutate(
    week = floor_date(date, unit = "week")
  ) %>%
  group_by(week) %>%
  summarise(
    weekly_traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Step 6: Check the aggregated dataset
View(prags_daily)
summary(prags_daily)

# Step 7: Plot the daily traffic development
season_periods <- data.frame(
  start = as.Date(c("2020-07-10", "2021-07-10", "2022-07-10", "2023-07-10", "2024-07-10")),
  end   = as.Date(c("2020-09-10", "2021-09-10", "2022-09-10", "2023-09-10", "2024-09-10"))
)

p <- ggplot(prags_weekly, aes(x = week, y = weekly_traffic)) +
  geom_rect(
    data = season_periods,
    inherit.aes = FALSE,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    alpha = 0.15
  ) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date("2022-07-10"),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Wöchentliche Verkehrszaehldaten Prags",
    subtitle = "Richtung: SS49 - Rotatoria di Braies",
    x = "Datum",
    y = "Fahrzeuge pro Woche"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  )
  theme_minimal(base_size=14)
  ggsave(
    "prags_weekly_traffic.png",
    plot = p,
    width = 16,
    height = 7,
    dpi = 300
  )
