#Case (Trial exam) - Bergen Bysykkel

library(tidyverse)
library(ggplot2)
library(ggmap)

#Reading in the datasets
september <- read_csv("09.csv")
oktober <-  read_csv("10.csv")
november <-  read_csv("11.csv")

#Binding the df's together
bysykkel_data <- september %>% 
  bind_rows(oktober, november)

#Creating a separate df for the stations and summarizing duplicate values. 
#It now contains ID's, name, description and coordinates.
stations <- bysykkel_data %>% 
  transmute(start_station_id, start_station_name, start_station_description, start_station_latitude, 
            start_station_longitude) %>% 
  group_by(start_station_id, start_station_name, start_station_description, start_station_latitude, 
           start_station_longitude) %>% 
  summarize()

stations %>% 
  filter(duplicated(.[["start_station_id"]]))

#Creating a separate df for all the individual trips with from location id and destination location id
sykkelturer <- bysykkel_data %>% 
  transmute(start_station_id, end_station_id) 


#Adding my google maps API key
ggmap::register_google(key = "AIzaSyAW_0hXLh_9llGZAJuQCDz2_YgB12AcXHw")


#Creating a map of downtown bergen of the type roadmap with colors
bergen_center_map <- ggmap(get_googlemap(center = c(lon = 5.33, lat = 60.39),
                    zoom = 13, scale =2,
                    maptype = "roadmap",
                    color = "color" )) 

#Adding all the different stations to the map
bergen_center_map +
  geom_point(data = stations,
               aes(x = start_station_longitude, y = start_station_latitude),
                   size = 1.5) +
               theme(legend.position="bottom")
  

geom_point(aes(x = lon_deg, y = lat_deg, colour = water_source), 
           data = tan_data) +


stations %>% 
get_googlemap(center = c(lon = "start_station_latitude", lat = "start_station_longitude"))

?get_map
?get_googlemap
?geom_point

ggmap(get_googlemap)
  geom_sf(aes(geometry = geometry), fill = "#00000050") +
  geom_point(aes(x = lon_deg, y = lat_deg, colour = water_source), 
             data = tan_data) +
  xlab("") +
  ylab("") +
  labs(colour = "") +
  ggtitle("Water sources in Tanzania") +
  # theme_fira() +
  # scale_colour_fira(na.value = "darkred") +
  theme(axis.line = element_blank(),
        panel.grid.major = element_line(colour = "#00000020"), 
        axis.text = element_text(colour = "#00000050"))



