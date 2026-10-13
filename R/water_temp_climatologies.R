library(oce)
library(tidyr)
library(ggplot2)
library(sensorstrings)
library(lubridate)
library(repr)
library(plotly)
library(dplyr)
library(padr)
library(imputeTS)

library(ggview)
library(purrr)


dat <- readRDS("data/water_temp_variables.rds")

# potting thing together (climatology and observed/imp values)

data <- dat %>% 
  filter(Variables == c("climatology mean", "temperature observed")) %>% 
  filter(ts_round >= as.Date("2022-05-15 00:00:00") & 
           ts_round <= as.Date("2022-10-15 00:00:00"))


ggplot(data, aes(x=ts_round, y=values, colour = Variables)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~station_depth, ncol = 3) +
  scale_x_datetime(date_breaks = "14 days", date_labels = " %Y %b %d") + 
  theme_light()+
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10)
  ) +
  labs(x="Time",
       y="Temperature (degrees C)") +
  canvas(15,10,"in",dpi=500)

#ggsave("climatology_2022.png", width = 15, height=10, units="in")




# as a histogram of anomaly distributions ---------------------------------
data_x =  dat %>% 
  select(station, ts_round, month_day, Variables, values, station_depth) %>% 
  filter(Variables == "anomaly_value") %>% 
  filter(ts_round >= as.Date("2020-01-01 00:00:00") & 
           ts_round <= as.Date("2021-01-01 00:00:00"))


ggplot(data_x, aes(x=values, fill = Variables)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  scale_colour_viridis_c()+
  facet_wrap(~station_depth, ncol = 3) +
  
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme_light()+ 
  theme(
    legend.position = "none", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10)
  ) +
  labs(x="Temperature anomaly (degrees C)",
       y="Count")+ 
  canvas(15,10,units="in",dpi=500)

#ggsave("temp_dist_2020.png", width= 15, height= 10, units = "in")
