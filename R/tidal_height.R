
library(DT)
library(ggplot2)
library(readr)
library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data
library(rmarkdown)

library(htmltools)
library(RColorBrewer)
library(stringr)
library(tidyr)

library(paletteer)
library(ggview)


tidal_data <- readRDS("data/water_level_data.rds") %>% 
  mutate(station = case_when(
    station == "Bedford Institute"~ "2 - Bedford Institute",
    station == "Port Hawkesbury"~ "1 - Canso Causeway / Port Hawkesbury",
    station == "Yarmouth"~ "3 - Yarmouth")) %>% 
  filter(timestamp_utc >= as.Date("2022-08-15 00:00:00") & 
           timestamp_utc <= as.Date("2022-09-05 00:00:00")) 

tidal_data <- na.omit(tidal_data)


tidal_data %>% ggplot(aes(x=timestamp_utc, y=water_level_m)) +
  geom_line(linewidth =0.8) +
  scale_x_datetime(date_breaks = "1 day", date_labels = " %Y %b %d") +
  facet_wrap(~station, ncol = 1, shrink = FALSE, scales = "free_y") +
  theme_light() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_text(size = 10),
        strip.text = element_text(colour = "black", size = 10)
  ) +
  labs(
    x = "Time",
    y = "Water level (meters)") +
  canvas(15, 10, units = "in", dpi = 500)


#ggsave("tidal_height_2022.png", width = 15, height = 10, units = "in")
