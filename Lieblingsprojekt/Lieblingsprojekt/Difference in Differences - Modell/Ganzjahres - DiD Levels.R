# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(modelsummary)
library(sandwich)
library(lmtest)
library(scales)

# Define dates
start_date <- as.Date("2020-07-18")
end_date <- as.Date("2025-02-25")
treatment_date <- as.Date("2022-07-10")

# Prepare Pragser Tal weekly data
prags_weekly_all <- Daten_Prags %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Richtung == "SS49 - Rotatoria di Braies",
    date >= start_date,
    date <= end_date
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

# Prepare Sexten station 34 weekly data
sexten_34_weekly_all <- Daten_34 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (verso S. Stefano di Cadore)",
    date >= start_date,
    date <= end_date
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare Sexten station 91 weekly data
sexten_91_weekly_all <- Daten_91 %>%
  mutate(
    datetime = as.POSIXct(`Tag und Stunde`, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(datetime),
    week = floor_date(date, unit = "week")
  ) %>%
  filter(
    Ort == "Marcia (Discendente)",
    date >= start_date,
    date <= end_date
  ) %>%
  group_by(week) %>%
  summarise(
    traffic = sum(Insgesamt, na.rm = TRUE),
    .groups = "drop"
  )

# Combine Sexten stations
sexten_weekly_all <- bind_rows(
  sexten_34_weekly_all,
  sexten_91_weekly_all
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
did_data_levels <- bind_rows(
  prags_weekly_all,
  sexten_weekly_all
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

# Estimate level DiD models
did_model_1_levels <- lm(
  traffic ~ treatment + post + did,
  data = did_data_levels
)

did_model_2_levels <- lm(
  traffic ~ treatment + post + did + month,
  data = did_data_levels
)

did_model_3_levels <- lm(
  traffic ~ treatment + post + did + month + year,
  data = did_data_levels
)

# Show robust results in console
coeftest(did_model_1_levels, vcov = vcovHC(did_model_1_levels, type = "HC1"))
coeftest(did_model_2_levels, vcov = vcovHC(did_model_2_levels, type = "HC1"))
coeftest(did_model_3_levels, vcov = vcovHC(did_model_3_levels, type = "HC1"))

# Export regression table to Excel
modelsummary(
  list(
    "Model 1: Baseline" = did_model_1_levels,
    "Model 2: Month FE" = did_model_2_levels,
    "Model 3: Month + Year FE" = did_model_3_levels
  ),
  vcov = list(
    vcovHC(did_model_1_levels, type = "HC1"),
    vcovHC(did_model_2_levels, type = "HC1"),
    vcovHC(did_model_3_levels, type = "HC1")
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "did_levels_results.xlsx"
)

# Create empirical indexed DiD plot
did_indexed <- did_data_levels %>%
  group_by(region) %>%
  mutate(
    pre_mean = mean(traffic[post == 0], na.rm = TRUE),
    traffic_index = traffic / pre_mean * 100
  ) %>%
  ungroup()

p_empirical_did <- ggplot(
  did_indexed,
  aes(
    x = week,
    y = traffic_index,
    color = region
  )
) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = treatment_date,
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Empirical Difference-in-Differences Plot",
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

print(p_empirical_did)

ggsave(
  filename = "empirical_did_indexed_levels.png",
  plot = p_empirical_did,
  width = 16,
  height = 7,
  dpi = 300
)

# Create textbook-style DiD means
did_means <- did_data_levels %>%
  mutate(
    period = if_else(post == 0, "Pre-Treatment", "Post-Treatment")
  ) %>%
  group_by(region, treatment, period, post) %>%
  summarise(
    mean_traffic = mean(traffic, na.rm = TRUE),
    .groups = "drop"
  )

sexten_pre <- did_means %>%
  filter(region == "Sexten", period == "Pre-Treatment") %>%
  pull(mean_traffic)

sexten_post <- did_means %>%
  filter(region == "Sexten", period == "Post-Treatment") %>%
  pull(mean_traffic)

prags_pre <- did_means %>%
  filter(region == "Pragser Tal", period == "Pre-Treatment") %>%
  pull(mean_traffic)

prags_post <- did_means %>%
  filter(region == "Pragser Tal", period == "Post-Treatment") %>%
  pull(mean_traffic)

sexten_change <- sexten_post - sexten_pre
prags_counterfactual_post <- prags_pre + sexten_change
did_effect <- prags_post - prags_counterfactual_post

textbook_did_data <- tibble(
  period = factor(
    c(
      "Pre-Treatment",
      "Post-Treatment",
      "Pre-Treatment",
      "Post-Treatment",
      "Post-Treatment"
    ),
    levels = c("Pre-Treatment", "Post-Treatment")
  ),
  group = c(
    "Sexten",
    "Sexten",
    "Pragser Tal observed",
    "Pragser Tal observed",
    "Pragser Tal counterfactual"
  ),
  traffic = c(
    sexten_pre,
    sexten_post,
    prags_pre,
    prags_post,
    prags_counterfactual_post
  )
)

# Create textbook-style DiD plot
p_textbook_did <- ggplot(
  textbook_did_data,
  aes(
    x = period,
    y = traffic,
    group = group,
    linetype = group
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_segment(
    aes(
      x = 2.08,
      xend = 2.08,
      y = prags_counterfactual_post,
      yend = prags_post
    ),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2.18,
    y = (prags_post + prags_counterfactual_post) / 2,
    label = paste0(
      "DiD effect = ",
      comma(round(did_effect, 0))
    ),
    hjust = 0,
    size = 4
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Textbook Difference-in-Differences Illustration",
    subtitle = "Observed Pragser Tal trend compared to counterfactual trend based on Sexten",
    x = "Period",
    y = "Average Weekly Traffic",
    linetype = "Group"
  ) +
  theme_minimal(base_size = 14)

print(p_textbook_did)

ggsave(
  filename = "textbook_did_levels.png",
  plot = p_textbook_did,
  width = 12,
  height = 7,
  dpi = 300
)

# Display key DiD quantities
did_summary <- tibble(
  quantity = c(
    "Sexten Pre",
    "Sexten Post",
    "Pragser Tal Pre",
    "Pragser Tal Post",
    "Pragser Tal Counterfactual Post",
    "DiD Effect"
  ),
  value = c(
    sexten_pre,
    sexten_post,
    prags_pre,
    prags_post,
    prags_counterfactual_post,
    did_effect
  )
)

print(did_summary)