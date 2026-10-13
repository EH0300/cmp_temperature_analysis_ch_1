library(ggplot2)
library(dplyr)
library(tidyr)
library(here)           
library(lubridate)
library(ggview)

library(padr)
library(imputeTS)


climate_data <- readRDS("data/climate_data_12_hours.rds") %>% 
  dplyr::filter(timestamp_utc >= as.Date("2022-05-15 00:00:00") & 
                  timestamp_utc <= as.Date("2022-10-15 00:00:00")) %>% 
  mutate(station = case_when(
    station == "LUNENBURG" ~ "C - Lunenburg",
    station == "BEAVER ISLAND (AUT)" ~ "B - Beaver Island (AUT)",
    station == "YARMOUTH RCS" ~ "D - Yarmouth RCS",
    station == "HART ISLAND (AUT)" ~ "A - Hart Island (AUT)"
  ))

climate_data %>% dplyr::filter(variable == "bui_m3_s") %>% 
  mutate(upwelling = if_else(value > 0, "upwelling", "downwelling")) %>% ggplot(aes(x=timestamp_utc, y=value, fill = upwelling, col = upwelling, group = 1)) +
  geom_col() +
  geom_hline(yintercept = 0, col = "grey50", linewidth = 0.25) +
  scale_y_continuous("BUI (m^3/s per 100 km)") +
  scale_fill_manual(
    values = c("upwelling" = "#414487FF", "downwelling" = "#7AD151FF")) +
  scale_color_manual(
    values = c("upwelling" = "#414487FF", "downwelling" = "#7AD151FF")) +
  facet_wrap(~station, ncol = 1, scales = "free_y") +
  theme(legend.position = "none", strip.text = element_text(size = 12)) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%Y %b %d") +
  theme_light()+
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_text(size = 10),
        
        strip.text = element_text(colour = "black", size = 10)) +
  
  labs(x= "Time",
       y= "BUI (m^3/s per 100 km)") +
  canvas(15,10,units="in",dpi=500)

#ggsave("BUI_2022.png", width = 15, height = 10, units="in")