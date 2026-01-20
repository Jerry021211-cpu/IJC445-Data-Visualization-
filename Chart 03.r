# Install required packages
install.packages(c("tidyverse", "lubridate", "repr"))
library(tidyverse)
library(lubridate)
library(repr)  

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


#Read and merge data
all_data <- map_dfr(file_paths, function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  site <- str_extract(file, "^[^_]+")
  month <- str_extract(file, "(?<=_)\\d+")
  df %>% 
    mutate(
      site = site,
      month = month,
      datetime = ymd_hms(datetimeLocal),
      hour = hour(datetime)
    )
})


# Calculate hourly averages 
 
  #Urban NO2 
no2_hourly <- all_data %>%
  filter(parameter == "no2" & site == "Mong Kok") %>%
  filter(value > 0) %>%
  drop_na() %>%
  group_by(hour) %>%
  summarise(no2_mean = mean(value, na.rm = TRUE), .groups = "drop")

  #Urban O3 
o3_urban <- all_data %>%
  filter(parameter == "o3" & site == "Mong Kok") %>%
  filter(value > 0) %>%
  drop_na() %>%
  group_by(hour) %>%
  summarise(o3_urban = mean(value, na.rm = TRUE), .groups = "drop")

  #Suburban O3 
o3_suburban <- all_data %>%
  filter(parameter == "o3" & site == "Tai Po") %>%
  filter(value > 0) %>%
  drop_na() %>%
  group_by(hour) %>%
  summarise(o3_suburban = mean(value, na.rm = TRUE), .groups = "drop")


# Merge data and fill NA 
o3_hourly <- full_join(o3_urban, o3_suburban, by = "hour") %>%
  left_join(no2_hourly, by = "hour") %>%
  replace_na(list(o3_urban = 0, o3_suburban = 0, no2_mean = 0))


#Dynamic Y-axis range
max_o3_value <- max(o3_hourly$o3_suburban, na.rm = TRUE)
y_axis_max <- max_o3_value * 1.1  # Add 10% margin to avoid cutoff


#Create chart
  #Set plot size 
options(repr.plot.width = 14, repr.plot.height = 10) 
p <- ggplot(o3_hourly, aes(x = hour)) +
  
  geom_area(aes(y = o3_suburban), fill = "#3498DB", alpha = 0.6) +
  geom_area(aes(y = o3_urban), fill = "#E74C3C", alpha = 0.6) +
  
  # O3 outline lines
  geom_line(aes(y = o3_suburban, color = "Suburban O3 (Tai Po)"), linewidth = 1.2) +
  geom_line(aes(y = o3_urban, color = "Urban O3 (Mong Kok)"), linewidth = 1.2) +
  
  # Urban NO2 line 
  geom_line(aes(y = no2_mean / 2), 
            color = "#FFA500", linewidth = 1.5, alpha = 0.7) +
  geom_point(aes(y = no2_mean / 2), 
             color = "#FFA500", size = 2, alpha = 0.7) +
  
  # Axis configuration, SHOW ALL HOURS
  scale_x_continuous(
    breaks = seq(0, 23, 1),  
    limits = c(0, 23),
    expand = c(0, 0),
    name = "Hour of Day (Local Time)"
  ) +
  scale_y_continuous(
    name = "O3 Concentration (µg/m³)",
    sec.axis = sec_axis(~ . * 2, name = "NO2 Concentration (µg/m³)"),
    limits = c(0, y_axis_max),  # Dynamic Y-axis
    expand = c(0, 0)
  ) +
  
  # Color
  scale_color_manual(
    values = c(
      "Urban O3 (Mong Kok)" = "#E74C3C",
      "Suburban O3 (Tai Po)" = "#3498DB"
    ),
    name = "O3 - Site Type"
  ) +
  
  # Chart labels 
  labs(
    title = "24-Hour O3 Concentration (Urban vs Suburban Hong Kong)",
    subtitle = "Area Chart with Urban NO2 Trend Reference (Orange Line)",
    caption = "Data Source: OpenAQ | Time Period: Jan/Apr/Jul/Oct 2025"
  ) +
  
  # Theme 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.title.y.right = element_text(color = "#FFA500"),  # NO2 axis label 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),  
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    legend.box = "horizontal",
    panel.grid = element_line(color = "gray90"),
    plot.margin = margin(10, 10, 10, 10)  
  )

# Force plot display
print(p)

