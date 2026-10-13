
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


data <- readRDS("data/climate_data_12_hours.rds") %>% 
  filter(timestamp_utc >= as.Date("2022-05-15 00:00:00") & 
           timestamp_utc <= as.Date("2022-10-15 00:00:00")) %>% 
filter(variable == "pressure_kpa")


# plotting the data
data %>% 
  ggplot(aes(x=timestamp_utc, y=value)) +
  geom_line(linewidth =0.8) +
  facet_wrap(~station, ncol = 1, shrink = FALSE) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%Y %b %d") +
  theme_light() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_text(size = 10),
        strip.text = element_text(colour = "black", size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  labs(
    x = "Time",
    y = "Air pressure (kpa)") +
  canvas(15, 10, units = "in", dpi = 500)

#ggsave("air_pressure_2022.png", width = 15, height = 10, units = "in")