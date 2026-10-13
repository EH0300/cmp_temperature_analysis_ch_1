


data <- readRDS("data/water_temp_data.rds") %>% 
  filter(ts_round >= as.Date("2020-05-15 00:00:00") & 
           ts_round <= as.Date("2020-10-15 00:00:00")) 

# the sampling frequency is in samples per day 
# 2 hours = 12 samples per day, 1 hour = 24 samples, 12 hours = 2 samples, etc

frequency_x = 24


# using spectrum ----------------------------------------------------------

# 1. Calculate spectra per site and depth 
spec_data <- data %>%
  #filter(depth == depth_x) %>% 
  group_by(station_depth) %>%
  do({
    
    s <- spectrum(.$value_imp, log = "yes", spans= c(21,11,5), plot = F) 
    
    data.frame(
      frequency = s$freq,
      spectral_density = s$spec
    )
  })


#2020
spec_data_2020 <- subset(spec_data, station_depth != "1 - Moose Point 1 - 05 m")
spec_data_2020 <- subset(spec_data_2020, station_depth != "5 - Wedgeport - 10 m")

spec_data_2020 <- subset(spec_data_2020, station_depth != "6 - Big Pond Point - 02 m")
spec_data_2020 <- subset(spec_data_2020, station_depth != "6 - Big Pond Point - 05 m")
spec_data_2020 <- subset(spec_data_2020, station_depth != "6 - Big Pond Point - 10 m")


spec_data_2020_2 <- subset(spec_data_2020, station_depth == c("5 - Wedgeport - 02 m",
                                                              "5 - Wedgeport - 05 m",
                                                              "1 - Moose Point 1 - 02 m", 
                                                              "1 - Moose Point 1 - 10 m"))

spec_data_2020_part <- subset(spec_data_2020, station_depth == c("2 - Center Bay - 02 m", 
                                                                 "2 - Center Bay - 05 m",
                                                                 "2 - Center Bay - 10 m",
                                                                 "3 - Spry Harbour - 02 m", 
                                                                 "3 - Spry Harbour - 05 m",
                                                                 "3 - Spry Harbour - 10 m",
                                                                 "4 - Birchy Head - 02 m", 
                                                                 "4 - Birchy Head - 05 m",
                                                                 "4 - Birchy Head - 10 m"))




#2022
spec_data_yar <- subset(spec_data, station_depth == c("6 - Big Pond Point - 10 m",
                                                      "6 - Big Pond Point - 02 m",
                                                      "6 - Big Pond Point - 05 m",
                                                      "5 - Wedgeport - 10 m",
                                                      "5 - Wedgeport - 02 m",
                                                      "5 - Wedgeport - 05 m"))


spec_data_part <- subset(spec_data, station_depth == c("1 - Moose Point 1 - 02 m", 
                                                       "1 - Moose Point 1 - 05 m",
                                                       "1 - Moose Point 1 - 10 m",
                                                       "2 - Center Bay - 02 m", 
                                                       "2 - Center Bay - 05 m",
                                                       "2 - Center Bay - 10 m",
                                                       "3 - Spry Harbour - 02 m", 
                                                       "3 - Spry Harbour - 05 m",
                                                       "3 - Spry Harbour - 10 m",
                                                       "4 - Birchy Head - 02 m", 
                                                       "4 - Birchy Head - 05 m",
                                                       "4 - Birchy Head - 10 m"
))

# 2. Plot 
ggplot(spec_data_2020_part, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )

ggsave("temp_PS_2020_rest.png", width= 15, height=10, units="in")




# plotting together -------------------------------------------------------
MP1 <- subset(spec_data, station_depth == c("1 - Moose Point 1 - 02 m", 
                                            "1 - Moose Point 1 - 05 m",
                                            "1 - Moose Point 1 - 10 m"))

CB <- subset(spec_data, station_depth == c("2 - Center Bay - 02 m", 
                                           "2 - Center Bay - 05 m",
                                           "2 - Center Bay - 10 m"))

SH <- subset(spec_data, station_depth == c("3 - Spry Harbour - 02 m", 
                                           "3 - Spry Harbour - 05 m",
                                           "3 - Spry Harbour - 10 m"))

BH <- subset(spec_data, station_depth == c("4 - Birchy Head - 02 m", 
                                           "4 - Birchy Head - 05 m",
                                           "4 - Birchy Head - 10 m"))

WP <- subset(spec_data, station_depth == c("5 - Wedgeport - 02 m", 
                                           "5 - Wedgeport - 05 m",
                                           "5 - Wedgeport - 10 m"))

BPP <-  subset(spec_data, station_depth == c("6 - Big Pond Point - 02 m", 
                                             "6 - Big Pond Point - 05 m",
                                             "6 - Big Pond Point - 10 m"))



fig.1 <- ggplot(MP1, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )


fig.2 <- ggplot(CB, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )


fig.3 <- ggplot(SH, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )


fig.4 <- ggplot(BH, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )



fig.5 <- ggplot(WP, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )


fig.6 <- ggplot(BPP, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(~station_depth, ncol = 3) +
  
  geom_textvline(xintercept =  0.0805, label = "M2", color = "blue", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "red", linetype = "dashed", size = 2, hjust = 0.8) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "green", linetype = "dashed", size = 2, hjust = 0.8) +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 10),
    strip.text = element_text(colour = "black", size = 10), strip.placement = "outside") + 
  canvas(15,10,units="in",dpi=500) +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )



# final_fig <- cowplot::plot_grid(fig.1, fig.2, fig.3, fig.4,
#                                 ncol = 1, 
#                                 align = "hv",
#                                 axis = "tblr")
#                                 
#                                 
# 
# plot(final_fig)
# ggsave("week_15.png", width= 32, height= 12, units = "in", dpi = 300)



