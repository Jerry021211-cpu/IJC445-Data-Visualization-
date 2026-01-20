# Install required packages 
install.packages(c("tidyverse", "lubridate", "readr"))

# Load packages
library(tidyverse)
library(lubridate)


# Define file paths 
file_paths <- c(
  "Mong Kok_1.csv",
  "Mong Kok_4.csv",
  "Mong Kok_7.csv",
  "Mong Kok_10.csv",
  "Tai Po_1.csv",
  "Tai Po_4.csv",
  "Tai Po_7.csv",
  "Tai Po_10.csv"
)


# Batch read and merge files
all_data <- map_dfr(file_paths, function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  site <- str_extract(file, "^[^_]+")
  month <- str_extract(file, "(?<=_)\\d+")
  df %>% 
    mutate(
      site = site,
      month = month
    )
})


# Clean data and filter PM2.5
pm25_data <- all_data %>%
  filter(parameter == "pm25") %>%
  mutate(
    datetime = ymd_hms(datetimeLocal),
    date = date(datetime)
  ) %>%
  filter(value > 0 & value <= 150)


# Calculate daily average PM2.5
pm25_daily <- pm25_data %>%
  group_by(site, month, date) %>%
  summarise(pm25_mean = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    month_name = case_when(
      month == "1" ~ "Winter (Jan)",
      month == "4" ~ "Spring (Apr)",
      month == "7" ~ "Summer (Jul)",
      month == "10" ~ "Autumn (Oct)"
    ),
    month_name = factor(
      month_name,
      levels = c("Winter (Jan)", "Spring (Apr)", "Summer (Jul)", "Autumn (Oct)"),
      ordered = TRUE
    )
  )


# Plot grouped bar chart
ggplot(pm25_daily, aes(x = month_name, y = pm25_mean, fill = site)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.8), width = 0.7) +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "2025 Hong Kong Seasonal PM2.5: Urban vs Suburban",
    x = "Season",
    y = "Daily Average PM2.5 (µg/m³)",
    fill = "Site",
    caption = "Data Source: OpenAQ | Note: Urban=Mong Kok, Suburban=Tai Po"
  ) +
  scale_fill_manual(values = c("Mong Kok" = "#E74C3C", "Tai Po" = "#3498DB")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )