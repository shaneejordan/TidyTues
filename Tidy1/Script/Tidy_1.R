#Tidy Tuesday 1
#03/04/2020
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(here)

#call data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
view(youtube)

#make ggplot
youtube %>%
  filter(year == "2010") %>%
  ggplot(aes(x = view_count, y = like_count, color = brand)) + #color code by brand
  geom_point(shape = 19, size = 4, show.legend = TRUE)+ #make  scatter plot
  #facet_wrap(~year)+ #facet wrap by year
  labs(x = "View Count",  
       y = "Like Count")+ #label axes
  theme_classic()+ #set up background theme
  theme(axis.title = element_text(size=13, color="black"))+
  ggsave(here("Tidy_Tues","Output","youtube.png"),
         width = 7, height = 7) #saves plot to output folder