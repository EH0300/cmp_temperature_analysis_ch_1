library(ggplot2)
library(cowplot)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

library(tidyr)
library(plotly)
library(dplyr)

library(ggrepel)
library(geodata)

library(ggview)


# map of canada -----------------------------------------------------------
canada_map <- ne_states(country = "canada", returnclass = "sf")

ggplot() +
  geom_sf(data = canada_map)

ggplot() +
  geom_sf(data = canada_map, colour = "black",
          fill = ifelse(canada_map$name == "Nova Scotia", 'blue', 'white')) +
  coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue", linewidth = 1),
        plot.background = element_blank())

# map of nova scotia ------------------------------------------------------

map_ns <- canada_map[canada_map$name == "Nova Scotia",]

ggplot() +
  geom_sf(data = map_ns, colour = "black", fill = "white") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)




# counties map  -----------------------------------------------------------

# downloaded online from geodata package
can_adm2 <- gadm(country = "CAN", level = 2, path = tempdir())

ns_counties <- st_as_sf(can_adm2) %>%
  filter(NAME_1 == "Nova Scotia")

ggplot(data = ns_counties) +
  geom_sf(aes(fill = NAME_2), color = "white", size = 0.4) +
  theme_minimal(base_size = 12) 
# +
#   labs(
#     title = "County Map of Nova Scotia",
#     subtitle = "Administrative Subdivisions (GADM Level 2)",
#     fill = "County",
#     caption = "Source: GADM Database / geodata package"
#   )

# map with sites ----------------------------------------------------------

map_data <- readRDS("data/map_data.rds")

ggplot() +
  geom_sf(data = canada_map, colour = "grey", fill = "white") +
  geom_sf(data = ns_counties, fill = "white", color = "darkgrey") +
  
  coord_sf(xlim = c(-68, -59), ylim = c(43, 47), clip = "on") +
  
  geom_point(data = map_data, 
             aes(x = longitude, y = latitude, colour = `Station Type`), size=5) +
  
  geom_label_repel(data = map_data, aes(x=longitude, y=latitude, group=station, label=station), 
                   min.segment.length = unit(-0.3, "lines"),
                   size = 4, 
                   hjust=0, vjust=1,
                   segment.size= 1.2,
                   fill = "white",
                   colour = "black",
                   box.padding = 0.5,
                   #nudge_x = -2,
                   nudge_y = -0.15
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.20, 0.70), 
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        
        axis.text = element_text(size = 15),
        axis.title = element_blank()) +
  canvas(20, 10, units = "in", dpi = 500)


#ggsave("site_map.png")



# yarmouth inset ----------------------------------------------------------

yar <- map_data[-c(2:6, 9:12),]

ggplot() +
  geom_sf(data = canada_map, colour = "grey", fill = "white") +
  geom_sf(data = ns_counties, fill = "white", color = "darkgrey") +
  
  
  coord_sf(xlim = c(-66.5, -65.5), ylim = c(43.2, 44), clip = "on", expand = F) +
  
  geom_point(data = yar, 
             aes(x = longitude, y = latitude, colour = `Station Type`), size=5) +
  
  geom_label_repel(data = yar, aes(x=longitude, y=latitude, group=station, label=station), 
                   min.segment.length = unit(-0.3, "lines"),
                   size = 4, 
                   hjust=0.75, vjust=0,
                   segment.size= 1.2,
                   fill = "white",
                   colour = "black",
                   box.padding = 0.5,
                   nudge_x = -2,
                   nudge_y = -0.15) +
  
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = c(0.6, 0.90), 
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_text(size = 15)
  )+ canvas(7, 10, units = "in", dpi = 500)

#ggsave("yar_map.png")






