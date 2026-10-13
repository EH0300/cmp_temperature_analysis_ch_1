
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


data_temps <- readRDS("data/air_and_water_temp_24_hours.rds") %>% 

filter(ts_round >= as.Date("2022-05-15 00:00:00") & 
         ts_round <= as.Date("2022-10-15 00:00:00")) %>% 
  pivot_longer(
    cols = c(depth_2, depth_5, depth_10, avg.air.temp),
    names_to = "Variable",
    values_to = "value") %>% 
  mutate(Variable = case_when(
    Variable == "depth_2" ~ "02 m",
    Variable == "depth_5" ~ "05 m",
    Variable == "depth_10" ~ "10 m",
    Variable == "avg.air.temp" ~ "Average air temperature"
  ))

data_temps <- data_temps %>% drop_na(value)



# plotting the data
data_temps %>%
  ggplot(aes(x=ts_round, y=value, colour = Variable)) +
  geom_line(linewidth =0.8) +
  scale_colour_viridis_d(direction = -1) +
  facet_wrap(~station, ncol = 1, shrink = FALSE) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%Y %b %d") +
  theme_light()+
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
    y = "Temperature (degrees C)") +
  canvas(15, 10, units = "in", dpi = 500)

#ggsave("water_temp_and_air_2022.png", width = 15, height = 10, units = "in")



