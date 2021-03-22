#Tidy Tuesday 3
#03/18/2020
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(here)
library(ggplot2)
library(tidyr)
library(dplyr)

#bring in data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

#lots to see here
#let's filter out some things
games2020 <- games %>% 
  filter(year == "2020", avg >= 50000, peak >= 100000)
games2020 #now we have less than 100 data points

#make ggplot
#let's see how peak plays correlate with plays gained and specify average plays per game
games2020 %>%
  ggplot(aes(x=gain, y=peak, fill = avg, color = avg))+
  theme_grey()+
  geom_point(show.legend = TRUE)+
  labs(x="Plays Gained", y="Peak Plays")+
  geom_vline(xintercept=0, linetype="dashed", color = "DodgerBlue")+ #show where plays gained cuts off from plays lost
  ggsave(here("Tidy 3","Output","gameplays.png"))