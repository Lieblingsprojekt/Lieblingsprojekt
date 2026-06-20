### Parallel Trends: Prags vs. Ahrntal ###########################

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Pre-treatment period
pre_start <- as.Date("2020-07-18")
pre_end   <- as.Date("2022-07-09")

# Prepare Pragser Tal
prags_weekly <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`,
                          format = "%Y-%m-%d %H:%M:%S"),
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

# Clean Ahrntal date variable
Daten_49_clean <- Daten_49 %>%
  mutate(
    Datum_trim = trimws(Datum),
    date = NA_Date_
  )

# Convert Excel date numbers
idx_num <- grepl("^[0-9]+$", Daten_49_clean$Datum_trim)

Daten_49_clean$date[idx_num] <- as.Date(
  as.numeric(Daten_49_clean$Datum_trim[idx_num]),
  origin = "1899-12-30"
)

# Convert normal date strings
Daten_49_clean$date[!idx_num] <- dmy(
  Daten_49_clean$Datum_trim[!idx_num]
)

# Prepare Ahrntal
ahrntal_weekly <- Daten_49_clean %>%
  mutate(
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Zählstelle == "Mühlen in Taufers",
    week >= pre_start,
    week <= pre_end
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(`Täglich vorbeifahrende Fahrzeuge`, na.rm = TRUE),
    .groups = "drop"
  )

# Check number of weeks
nrow(ahrntal_weekly)

# Create regional dataset
pre_data_ahrntal <- bind_rows(
  prags_weekly %>% mutate(region = "Pragser Tal"),
  ahrntal_weekly %>% mutate(region = "Ahrntal")
)

# Index each region to its own pre-treatment average
pre_data_ahrntal_indexed <- pre_data_ahrntal %>%
  group_by(region) %>%
  mutate(
    pre_mean = mean(traffic, na.rm = TRUE),
    index = traffic / pre_mean * 100
  ) %>%
  ungroup()

# Reihenfolge der Legende festlegen
pre_data_ahrntal_indexed$region <- factor(
  pre_data_ahrntal_indexed$region,
  levels = c("Pragser Tal", "Ahrntal")
)

### Parallel trends plot ########################################

p_ahrntal <- ggplot(
  pre_data_ahrntal_indexed,
  aes(
    x = week,
    y = index,
    color = region
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    breaks = c("Pragser Tal", "Ahrntal"),
    values = c(
      "Pragser Tal" = "#F8766D",
      "Ahrntal" = "#00BFC4"
    )
  ) +
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

print(p_ahrntal)

ggsave(
  filename = "parallel_trends_prags_vs_ahrntal.png",
  plot = p_ahrntal,
  width = 16,
  height = 7,
  dpi = 300
)

### Pearson correlation #########################################

correlation_data_ahrntal <- prags_weekly %>%
  rename(prags_traffic = traffic) %>%
  inner_join(
    ahrntal_weekly %>%
      rename(ahrntal_traffic = traffic),
    by = "week"
  )

correlation_result_ahrntal <- cor.test(
  correlation_data_ahrntal$prags_traffic,
  correlation_data_ahrntal$ahrntal_traffic,
  method = "pearson"
)

print(correlation_result_ahrntal)
correlation_result_ahrntal$estimate

### Scatterplot #################################################

p_scatter_ahrntal <- ggplot(
  correlation_data_ahrntal,
  aes(
    x = ahrntal_traffic,
    y = prags_traffic
  )
) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Pragser Tal vs. Ahrntal Traffic",
    subtitle = "Pre-treatment period (18 Jul 2020 - 09 Jul 2022)",
    x = "Weekly Traffic in Ahrntal",
    y = "Weekly Traffic in Pragser Tal"
  ) +
  theme_minimal(base_size = 14)

print(p_scatter_ahrntal)

ggsave(
  filename = "prags_ahrntal_scatterplot.png",
  plot = p_scatter_ahrntal,
  width = 10,
  height = 7,
  dpi = 300
)

### Pre-trend regression ########################################

reg_data_ahrntal <- bind_rows(
  prags_weekly %>% mutate(region = 1),
  ahrntal_weekly %>% mutate(region = 0)
) %>%
  arrange(week) %>%
  mutate(
    time = as.numeric(week - min(week))
  )

pretrend_model_ahrntal <- lm(
  traffic ~ time + region + time:region,
  data = reg_data_ahrntal
)

summary(pretrend_model_ahrntal)

# Extract key results
correlation_result_ahrntal$estimate