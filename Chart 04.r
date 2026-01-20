# Install required packages
install.packages(c("ggplot2", "dplyr", "lubridate"))
library(ggplot2)
library(dplyr)
library(lubridate)

# File paths
file_paths <- c(
  "Mong Kok_1.csv",
  "Mong Kok_4.csv",
  "Mong Kok_7.csv",
  "Mong Kok_10.csv"
)


# Read and merge data
all_data <- data.frame()
for (file in file_paths) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df_processed <- df %>%
    
    # Keep valid NO2/PM2.5 data
    filter(parameter %in% c("no2", "pm25"), value > 0) %>% 
    mutate(
      datetime = ymd_hms(datetimeLocal, tz = "Asia/Hong_Kong"),  # HK time zone
      wday = wday(datetime, week_start = 1),
      
      # Classify days
      day_type = ifelse(wday %in% 1:5, "Weekday", "Weekend"),  
      
      # Standardize labels
      pollutant = ifelse(parameter == "pm25", "PM2.5", "NO2")
    ) %>%
    select(pollutant, day_type, value) %>%
    na.omit(). # Remove NAs
  
  all_data <- rbind(all_data, df_processed)
}


# Calculate average concentration & reduction percentage
summary_data <- all_data %>%
  group_by(pollutant, day_type) %>%
  summarise(avg_concentration = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(pollutant) %>%
  mutate(
    reduction_pct = round((1 - avg_concentration[day_type == "Weekend"]/avg_concentration[day_type == "Weekday"]) * 100, 1),
    reduction_label = paste0("↓", reduction_pct, "%")
  ) %>%
  ungroup()



# Create bar plot
p <- ggplot(summary_data, aes(x = pollutant, y = avg_concentration, fill = day_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  
  # Add concentration labels on bars
  geom_text(
    aes(label = sprintf("%.1f µg/m³", avg_concentration)),
    position = position_dodge(width = 0.8), vjust = -0.5, size = 4, fontface = "bold"
  ) +
  
  # Add reduction labels above weekend bars
  geom_text(
    data = filter(summary_data, day_type == "Weekend"),
    aes(label = reduction_label),
    position = position_dodge(width = 0.8), vjust = -1.8, size = 4.5, color = "#2CA02C", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Weekday" = "#FF7F0E", "Weekend" = "#2CA02C"), name = "Day Type") +
  scale_y_continuous(
    name = "Average Concentration (µg/m³)",
    expand = c(0, 0),
    limits = c(0, max(summary_data$avg_concentration) * 1.3)
  ) +  # Avoid label cutoff
  
  labs(
    title = "2025 Urban Air Quality: Weekday vs Weekend (Mong Kok)",
    subtitle = "PM2.5 and NO2 Concentration Comparison",
    x = "Pollutant",
    caption = "Data Source: OpenAQ | Data from 4 months (Jan, Apr, Jul, Oct) "
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.position = "top",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Display plot
print(p)
