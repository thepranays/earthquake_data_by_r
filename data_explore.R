
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(ggforce)
library(deldir)
library(ggridges)
library(viridis)
library(leaflet)
library(leaflet.extras)
library(ggpubr)

earthquake_frame <- read.csv("earthquakedata.csv")




#Bar graph
ggplot(earthquake_frame, aes(x = year, fill = factor(tsunami))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("orange", "steelblue")) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(earthquake_frame$year))

# #Point plot longi vs lati
# ggplot(earthquake_frame,aes(x= latitude,
#            y = longitude,
#       
#            size = magnitude,
#            color = tsunami)) +
#   geom_point()
#  

## Calculate summary statistics for earthquake_frame on global map
dff <- earthquake_frame %>%
  group_by(country) %>%
  summarize(long = mean(longitude, na.rm = TRUE), lat = mean(latitude, na.rm = TRUE))

# Load world map data
world_map_data <- map_data("world")

ggplot() +
  geom_polygon(data = world_map_data, aes(long, lat, group = group), fill = "steelblue3", color = "steelblue4") +
  geom_point(data = earthquake_frame, aes(x = longitude, y = latitude, color = magnitude), size = 2, alpha = 0.6) +
  scale_color_viridis_c(name = "Magnitude",option="C") +  # Using scale_color_viridis_c for point color
  geom_text(data = dff, aes(x = long, y = lat, label = country), size = 3, color = "seashell", fontface = "bold") +
  coord_equal() +
  theme(panel.background = element_rect(fill = "steelblue1")) 
theme_bw()



#Box_plot with jitter ,country vs magni (mean)
earthquake_frame %>%
  group_by(country) %>%
  mutate(mean_by_country = mean(magnitude)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, mean_by_country)) %>%
  ggplot(aes(country, magnitude, colour = country,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 4,
              alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 4, show.legend = F) +
  geom_hline(aes(yintercept = mean(magnitude)),
             colour = "gray70",
             size = 0.9) +
  geom_segment(aes(x = country, xend = country,
                   y = mean(magnitude), yend = mean_by_country),
               size = 2, show.legend = F) +
  labs(title = "Magnitude of earthquakes wrt countries",
       x = "Counteries",
       y = "Magnitude of earthquake") +
  theme(legend.position = "none") +
  theme_bw()



#Density dmin , depth vs country
earthquake_frame<-earthquake_frame %>%
  group_by(country) %>%
  mutate(mean_depth = mean(depth)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, mean_depth))

ggplot(earthquake_frame, aes(x = mean_depth, y = country, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Magnitude", option = "C") +
  labs(title = 'Depth of earthquakes over different countries') +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank())

## correlation graph depth vs magni
ggscatter(earthquake_frame, x = "magnitude", y = "depth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Magntiude", ylab = "Depth")


##  heatmap depth vs magni
ggplot(earthquake_frame, aes(x = depth, y = magnitude)) +
  geom_tile(stat = "bin2d", aes(fill = ..count..), width = 10) +
  scale_fill_viridis_c(name = "Counts",option="C") +
  labs(x = "Depth", y = "Magnitude", title = "Magnitude v/s Depth Heatmap")




  # Convert magnitude to colors using the Viridis color palette
  color_palette <- inferno(length(earthquake_frame$magnitude))
  color_by_magnitude <- color_palette[cut(earthquake_frame$magnitude, length(color_palette))]
  
   #  Leaflet Global heatmap 
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)  # Set initial view to show the whole world
  
  # Add dotted point layer for earthquake magnitudes with Viridis color scale
  map <- map %>%
    addCircleMarkers(data = earthquake_frame,
                     lng = ~longitude,
                     lat = ~latitude,
                     radius = ~sqrt(magnitude) * 2,  # Adjust radius based on magnitude
                     color = color_by_magnitude,    # Set color based on magnitude
                     fillOpacity = 0.8,
                     popup = paste("Magnitude: ", earthquake_frame$magnitude))
  
  # Display the map
  map
  
  
