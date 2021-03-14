#Tidy Tuesday 2
#03/10/2020
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
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
view(raw_bechdel)

#make mean rating data from the raw Bechdel test stats
#thinking of plotting mean rating by year here
mean_rating <- raw_bechdel %>%
  group_by(year) %>%
  summarize(mean = mean(rating, na.rm=TRUE))

#make a barplot in ggplot 
  ggplot(data = mean_rating, aes(x = year, y = mean, fill = year))+
  theme_gray() + 
  theme(axis.text.x=element_text(color="black", size=12), 
        axis.text.y=element_text(color="black", size=12))+
  geom_bar(colour="black", fill="DodgerBlue", width=1, stat="identity")+
  labs(x = "",  y = "Mean Rating Score", face ="bold", color="black", size=20)+
  scale_y_continuous(limits = c(0,3))+
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020))+
  ggsave(here("Tidy2","Output","bechdel_barplot.png"))

#or make a scatter plot with trend-line and confidence intervals
  ggplot(data = mean_rating, aes(x = year, y = mean, fill = year))+
  theme_classic() + 
  theme(axis.text.x=element_text(face="bold", color="black", size=12),
          axis.text.y=element_text(face = "bold", color="black", size=12))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method = "lm", show.legend = FALSE)+
  labs(x = "",  y = "Mean Rating Score", face ="bold", color="black", size=12)+
  scale_y_continuous(limits = c(0,3))+
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020))+
  ggsave(here("Tidy2","Output","bechdel_scatterplot.png"))
  
#try some stats?
#make a linear model
model1<-lm(mean~year, data=mean_rating)
summary(model1) 
#a significant + effect of year on rating, 
#although I never checked the lm assumptions, 
#with p so low it might not matter
  