# Step 1: Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Step 2: Prepare the dataset
daten_49_clean <- Daten_49_Kopie %>%
  mutate(
    date = dmy(Datum_neu),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(Zählstelle == "Mühlen in Taufers")

# Step 3: Check that all years are present
daten_49_clean %>%
  count(year = year(date))

# Step 4: Aggregate daily data to weekly data
daten_49_weekly <- daten_49_clean %>%
  group_by(week) %>%
  summarise(
    weekly_traffic =
      sum(`Täglich vorbeifahrende Fahrzeuge`,
          na.rm = TRUE),
    .groups = "drop"
  )

# Step 5: Inspect weekly traffic values
summary(daten_49_weekly$weekly_traffic)

max(daten_49_weekly$weekly_traffic)

# Step 6: Remove extreme outliers
daten_49_weekly_plot <- daten_49_weekly %>%
  filter(weekly_traffic <= 150000)

# Step 7: Create seasonal periods
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

# Step 8: Create the plot
p <- ggplot(
  daten_49_weekly_plot,
  aes(
    x = week,
    y = weekly_traffic
  )
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
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = as.Date("2022-07-10"),
    linetype = "dashed",
    linewidth = 1
  ) +
  
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  
  scale_y_continuous(
    labels = comma
  ) +
  
  coord_cartesian(
    ylim = c(0, 150000)
  ) +
  
  labs(
    title = "Weekly Traffic Development",
    subtitle = "Counting Station: Mühlen in Taufers",
    x = "Date",
    y = "Number of Vehicles per Week"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p)

ggsave(
  filename = "ahrntal_station_49_weekly_traffic.png",
  plot = p,
  width = 16,
  height = 7,
  dpi = 300
)