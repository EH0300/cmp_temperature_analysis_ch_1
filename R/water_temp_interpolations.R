library(ggplot2)
library(dplyr)
library(tidyr)
library(here)           
library(lubridate)
library(ggview)

library(padr)
library(imputeTS)
library(purrr)

#The datafile water_temp_data already has interpolatd values as yes/no (in plotting stage)
# The code below is a walkthrough on how to ----
#1. add in missing time 
#2. interpolate the values
#3. assign them yes or no based on if they were interoplated 
#4. plotting the data 


# step 0 loading in the data ----------------------------------------------
# data file data_1.0 is pretty blank. The following step will help add in and clarify some missing steps
round_unit = "24 hour"

data <- readRDS("data/data_1.0.rds") %>% 
  dplyr::filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"))

data$sensor_depth_at_low_tide_m <- as.character(data$sensor_depth_at_low_tide_m)

data <- data %>% 
  mutate(sensor_depth_at_low_tide_m = case_when(
    sensor_depth_at_low_tide_m == "2" ~ "02 m",
    sensor_depth_at_low_tide_m == "5" ~ "05 m",
    sensor_depth_at_low_tide_m == "10" ~ "10 m"
  )) %>% 
  mutate(station_depth = paste(station, sensor_depth_at_low_tide_m, sep = " - "))


ggplot(data, aes(ts_round, value_avg)) +
  geom_point() +
  facet_wrap(~station_depth, ncol = 3)


# step 1 - padding in missing time  ---------------------------------------

data <- data %>%  
  group_by(station_depth) %>%  
  padr::pad() %>%  
  ungroup()


#1.5 artifically padding in the values 

data[1417,"value_avg"] <- 19 # MP1 - 5m
data[6651, "value_avg"] <- 0.2 #CB - 5m
data[15210,"value_avg"] <- 18 #BH - 2m


# step 2 - data interpolations  -------------------------------------------
data_imp <- data %>%  
  group_split(station_depth) %>% # splits into lists based on a variable
  purrr::map(
    .f = \(x) ts(x) |> 
      na_seadec(algorithm = "interpolation", find_frequency = TRUE) |> 
      data.frame()
  ) %>%  
  list_rbind() 


data$value_imp <- data_imp$value_avg 


# step 3 - assigning the values yes/no based on status --------------------
data <- data %>%  
  mutate(Imputed = if_else(is.na(value_avg), TRUE, FALSE))

#saveRDS(data, file = "data/water_temp_data.rds)


# - -----------------------------------------------------------------------

#The datafile water_temp_data already has interpolatd values as yes/no

# step 4 - plotting the data ----------------------------------------------
data <- read.rds("data/water_temp_data.rds")

# plotting to check 
ggplot(data, aes(ts_round, value_imp, col = Imputed)) +
  geom_point() +
  facet_wrap(~station_depth, ncol = 3)


# plotting but more pretty 
ggplot(data, aes(ts_round, value_imp, col = Imputed)) +
  geom_point() +
  facet_wrap(~station_depth, ncol = 3) +
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
  guides(color = guide_legend(override.aes = list(size = 4))) +
  labs(x="Time",
       y="Temperature (degrees C)") +
  canvas(15,10,"in",dpi=500)


#ggsave("interpolated_visual.png", width = 15, height=10, units="in")
