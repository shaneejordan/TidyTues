#Tidy Tuesday 7
#04/23/2021
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(here)
library(janitor)
library(maps)
library(mapdata)
library(mapproj)

#load data
post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')
view(post_offices) #this is huuuge

#Nevada is rather depopulated, I wonder how a map of post office locations would look...
nevada_pt <- post_offices %>%
  filter(state == "NV")
view(nevada)

#Get data for counties
counties<-map_data("county")
counties<-as.data.frame(counties)
nevada_map<-counties %>%
  filter(region == "nevada") %>%
  rename(County = subregion) %>%
  mutate_if(is.character, str_to_upper) #this makes the county names in ALL CAPS
view(nevada_map)

#make a ggplot map
NVmap<-ggplot()+
  geom_polygon(data = nevada_map, 
               aes(x = long, 
                   y = lat,
                   group = group,
                   color = County))+ #colors county lines
  geom_point(shape=21, color = "Dodger Blue",data = nevada_pt, 
             aes(x = longitude, 
                 y = latitude))+
  theme_bw()+ #has a nice line all around the plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ #removes grid lines
  xlab("Long (dd)")+
  ylab("Lat (dd)")+
  ggsave(here("Tidy7", "Output", "NVmap.png"),width =7, height = 6)
NVmap
#Everything looks pretty evenly scattered out
#Not what I expected...ok

#Let's try the same thing for Utah, an even more unusual state...
utah_pt <- post_offices %>%
  filter(state == "UT")
view(utah_pt)

#Get data for counties
counties<-map_data("county")
counties<-as.data.frame(counties)
utah_map<-counties %>%
  filter(region == "utah") %>%
  rename(County = subregion) %>%
  mutate_if(is.character, str_to_upper) #this makes the county names in ALL CAPS
view(utah_map)

#make a ggplot map
UTmap<-ggplot()+
  geom_polygon(data = utah_map, 
               aes(x = long, 
                   y = lat,
                   group = group,
                   fill = County))+ #fills in counties by color
  geom_point(shape=21,data = utah_pt, 
             aes(x = longitude, 
                 y = latitude))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Long (dd)")+
  ylab("Lat (dd)")+
  ggsave(here("Tidy7", "Output", "UTmap.png"),width =7, height = 5)
UTmap
#This is kind of interesting. You can see how populations are clustered around 
#the I-15 corridor, whereas the regions to the east & west are rather desolate,
#especially around the Colorado River