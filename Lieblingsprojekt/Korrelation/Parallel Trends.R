### Deskriptive Veranschaulichung ###########################
# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Define pre-treatment period
pre_start <- as.Date("2020-07-18")
pre_end <- as.Date("2022-07-09")

# Prepare Pragser Tal
prags_weekly <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    week >= pre_start,
    week <= pre_end
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 34
sexten_34 <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (verso S. Stefano di Cadore)",
    week >= pre_start,
    week <= pre_end
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 91
sexten_91 <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (Discendente)",
    week >= pre_start,
    week <= pre_end
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Combine Sexten stations
sexten_weekly <- bind_rows(
  sexten_34,
  sexten_91
) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(traffic, na.rm = TRUE),
    .groups = "drop"
  )

# Create regional dataset
pre_data <- bind_rows(
  prags_weekly %>% mutate(region = "Pragser Tal"),
  sexten_weekly %>% mutate(region = "Sexten")
)

# Index each region to its own pre-treatment average
pre_data_indexed <- pre_data %>%
  group_by(region) %>%
  mutate(
    pre_mean = mean(traffic, na.rm = TRUE),
    index = traffic / pre_mean * 100
  ) %>%
  ungroup()

# Create plot
p <- ggplot(
  pre_data_indexed,
  aes(
    x = week,
    y = index,
    color = region
  )
) +
  geom_line(linewidth = 1) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Pre-Treatment Traffic Trends",
    subtitle = "Indexed weekly traffic, pre-treatment average = 100",
    x = "Date",
    y = "Traffic Index",
    color = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Display plot
print(p)

# Save plot
ggsave(
  filename = "parallel_trends_prags_vs_sexten.png",
  plot = p,
  width = 16,
  height = 7,
  dpi = 300
)

### Pearson-Korrelationskoeffizient #############################
# Merge weekly traffic data
correlation_data <- prags_weekly %>%
  rename(prags_traffic = traffic) %>%
  inner_join(
    sexten_weekly %>%
      rename(sexten_traffic = traffic),
    by = "week"
  )

# Pearson correlation test
correlation_result <- cor.test(
  correlation_data$prags_traffic,
  correlation_data$sexten_traffic,
  method = "pearson"
)

# Display correlation results
print(correlation_result)

# Extract correlation coefficient
correlation_result$estimate

###Scatterplot#################################################
# Create scatterplot with regression line
p_scatter <- ggplot(
  correlation_data,
  aes(
    x = sexten_traffic,
    y = prags_traffic
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Pragser Tal vs Sexten Traffic",
    subtitle = "Pre-treatment period (18 Jul 2020 - 09 Jul 2022)",
    x = "Weekly Traffic in Sexten",
    y = "Weekly Traffic in Pragser Tal"
  ) +
  theme_minimal(base_size = 14)

# Display plot
print(p_scatter)

# Save plot
ggsave(
  filename = "prags_sexten_scatterplot.png",
  plot = p_scatter,
  width = 10,
  height = 7,
  dpi = 300
)