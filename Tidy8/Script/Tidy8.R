#Tidy Tuesday 8
#05/07/2021
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

#bring in data
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
view(water)

#let's look at Ethiopia only
ethiopia_data <- water %>%
  filter(country_name == "Ethiopia")
view(ethiopia_data)

#let's contrast drilled boreholes and natural springs
ethiopia_water <- ethiopia_data %>%
  filter(water_source == c("Borehole", "Protected Spring"))
view(ethiopia_water)

#and plot them onto a map
countries<-map_data("world")
view(countries)

ethiopia_map <- countries %>%
  filter(region == "Ethiopia")
view(ethiopia_map)

#make a ggplot map
ethipoia_water_map<-ggplot()+
  geom_polygon(data = ethiopia_map, 
               aes(x = long, 
                   y = lat,
                   group = group,
                   fill = region))+ 
  geom_point(shape=21, data = ethiopia_water, 
             aes(x = lon_deg, 
                 y = lat_deg,
                 color = water_source))+
  theme_dark()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ #removes grid lines
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_sf(xlim = c(30, 48), ylim = c(3, 15))+
  scale_colour_viridis_d()+
  xlab("Long (dd)")+
  ylab("Lat (dd)")
  ggsave(here("Tidy8", "Output", "ethiopia_water_map.png"),width = 5, height = 4)
ethipoia_water_map
