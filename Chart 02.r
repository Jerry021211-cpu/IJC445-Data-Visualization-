# Install required packages
install.packages(c("tidyverse", "lubridate"))

# Load core libraries
library(tidyverse)
library(lubridate)


#Define file paths
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


#Batch read and merge all CSV files
all_data <- map_dfr(file_paths, function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Extract site and month from filename
  site <- str_extract(file, "^[^_]+")
  month <- str_extract(file, "(?<=_)\\d+")
  
  # Add site and month columns and return
  df %>% 
    mutate(
      site = site,
      month = month
    )
})


#Clean data and filter NO2 measurements
no2_data <- all_data %>%
  filter(parameter == "no2") %>%
  mutate(
    datetime = ymd_hms(datetimeLocal),
    hour = hour(datetime)
  ) %>%
  
  # Remove abnormal values
  filter(value > 0 & value <= 200) %>%
  drop_na()


#Calculate hourly average NO2 by site
no2_hourly <- no2_data %>%
  group_by(site, hour) %>%
  
  # Calculate mean NO2 concentration per hour per site
  summarise(
    no2_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    site_type = case_when(
      site == "Mong Kok" ~ "Urban (Mong Kok)",
      site == "Tai Po" ~ "Suburban (Tai Po)"
    )
  )


#Create dual line chart
ggplot(no2_hourly, aes(x = hour, y = no2_mean, color = site_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  
  # Basic chart labels
  labs(
    title = "24-Hour NO₂ Concentration: Urban vs Suburban Hong Kong",
    x = "Hour of Day (Local Time)",
    y = "Average NO₂ Concentration (µg/m³)",
    color = "Site Category",
    caption = "Data Source: OpenAQ | Time Period: Jan/Apr/Jul/Oct 2025"
  ) +
  
  # X-axis configuration
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  
  # Color
  scale_color_manual(
    values = c(
      "Urban (Mong Kok)" = "#E74C3C",
      "Suburban (Tai Po)" = "#3498DB"
    )
  ) +
  
  
  # Mac-compatible theme 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    legend.position = "top",
    panel.grid = element_line(color = "gray90") # Keep grid background
  )

