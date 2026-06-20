# Step 1: Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Step 2: Prepare dataset 34
daten_34_clean <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(Ort == "Marcia (verso S. Stefano di Cadore )") %>%
  group_by(week) %>%
  summarise(
    weekly_traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    station = "Daten_34"
  )

# Step 3: Prepare dataset 91
daten_91_clean <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(Ort == "Marcia (Discendente)") %>%
  group_by(week) %>%
  summarise(
    weekly_traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    station = "Daten_91"
  )

# Step 4: Combine both datasets
sexten_weekly <- bind_rows(
  daten_34_clean,
  daten_91_clean
)

# Step 4b: Create combined weekly traffic line
sexten_combined <- sexten_weekly %>%
  group_by(week) %>%
  summarise(
    weekly_traffic = sum(weekly_traffic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    station = "Daten_34 + Daten_91"
  )

sexten_weekly_three_lines <- bind_rows(
  sexten_weekly,
  sexten_combined
)
# Step 5: Create season periods
season_periods <- data.frame(
  start = as.Date(c(
    "2020-07-10",
    "2021-07-10",
    "2022-07-10",
    "2023-07-10",
    "2024-07-10"
  )),
  end = as.Date(c(
    "2020-09-10",
    "2021-09-10",
    "2022-09-10",
    "2023-09-10",
    "2024-09-10"
  ))
)

# Step 6: Create the plot
p <- ggplot(
  sexten_weekly_three_lines,
  aes(x = week, y = weekly_traffic, color = station)
) +
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
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Weekly Traffic Development in Sexten",
    subtitle = "Stations 34, 91, and combined traffic",
    x = "Date",
    y = "Number of Vehicles per Week",
    color = "Series"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p
# Step 7: Save the plot as PNG
ggsave(
  filename = "sexten_weekly_traffic_34_91.png",
  plot = p,
  width = 16,
  height = 7,
  dpi = 300
)