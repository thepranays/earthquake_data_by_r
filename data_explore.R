
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(ggforce)
library(deldir)
library(ggridges)
library(viridis)
earthquake_frame <- read.csv("earthquakedata.csv")


#Pie year to no. of earthquakes percentage
# ggplot(earthquake_frame, aes(x="", y="", fill=year)) +
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0)
# earthquake_frame %>% 
#   group_by(year) %>% 
#   mutate(cnt = n(),
#          pct = percent(cnt / nrow(.), accuracy = 1)) %>%
#   unique %>% ungroup %>% 
#   mutate(place = cumsum(cnt) - cnt/2) %>% 
#   ggplot(
#          aes(x = factor(1), weight = cnt, fill = factor(year))) + 
#   geom_bar(position = "stack") + 
#   scale_y_continuous(breaks = seq(0, length(year), length(year)/4), 
#                      labels = c("0", "25%", "50%", "75%", "100%")) + 
#   coord_polar(theta='y') +
#   geom_text(aes(x = 1.1, y = place, label = pct)) +
#   theme(axis.text.y = element_blank(), 
#         axis.title.y = element_blank(), 
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank()) +
  # labs(fill = year)

#Bar graph
ggplot(earthquake_frame, aes(x = year, fill = factor(tsunami))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(earthquake_frame$year))

#Point plot longi vs lati
ggplot(earthquake_frame,aes(x= latitude,
           y = longitude,
      
           size = magnitude,
           color = tsunami)) +
  geom_point()
 

#Density dmin , depth vs country
earthquake_frame %>%
  group_by(country) %>%
  mutate(mean_depth = mean(depth)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, mean_depth)) %>%
  ggplot(aes(x = mean_depth, y = country, fill = dmin)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               alpha = 5) +
  scale_fill_viridis(name = "dmin", limits = range(earthquake_frame$dmin)) +
  labs(title = 'Depth of earthquakes over different countries') +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


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
  labs(title = "Magnitude of earthquakes wrt counteries",
       x = "Counteries",
       y = "Magnitude of earthquake") +
  theme(legend.position = "none") +
  theme_bw()
