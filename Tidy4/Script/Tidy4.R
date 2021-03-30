#Tidy Tuesday 4
#03/26/2020
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(vegan)
library(here)

#at first I worked with the UN data but it wasn't going anywhere
#so instead I'm gonna use the plastics data from a few weeks back
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
view(plastics)

#there are many brands so I will pick the a few of the most widely distributed
plas1 <- plastics %>%
  filter(parent_company == c("Nestle", "The Coca-Cola Company",
                             "PepsiCo", "Solo Cup Co.", "Welch's"))
view(plas1) #where is Welch's?

#get rid of everything except brand and plastic type
plas1<-plas1[,-c(1,2,12,13,14)]
view(plas1)
plasdata<-na.omit(plas1) #remove NAs 
view(plasdata) #why only coke and nestle? oh well I'm gonna move ahead with just two brands...

#make an nMDS ordination plot that compares plastic similarity between coke and nestle
#first specify matrix
ord<-metaMDS(plasdata[,-1],k=2, distance='bray', trymax = 100) 
#using a bray-curtis dissimilarity matrix to show distance
#didn't work with 0 iterations but 100 iterations reached a solution
ord$stress #stress < 0.3 is ok

#two ways to make a plot: ggplot or baseplot:

#ggplot
#make ord points into a data frame
ordpoints<-data.frame(ord$points)

#make ord means to label by brand
ordmeans<-ordpoints %>%
  bind_cols(plasdata) %>%
  group_by(parent_company) %>%
  summarise(meanx = mean(MDS1), meany = mean(MDS2)) #need these means for labels in ggplot

#make ggplot
ordpoints %>%
  bind_cols(plasdata) %>%
  ggplot(aes(x = MDS1, y = MDS2, color = parent_company))+
  geom_point()+
  geom_text(data=ordmeans, aes(x=meanx, y=meany, label=parent_company))+ #add labels
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ #ditch the gridlines
  scale_color_manual(values=c("dodger blue", "red"))+ #customize colors
  stat_ellipse(aes(x = MDS1, y = MDS2, color=parent_company))+ #add hulled ellipses
  geom_vline(xintercept=0, linetype="dashed", color = "black")+ 
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ #shows zero nMDS value on x and y axes 
  labs(x="nMDS1", y="nMDS2")
  ggsave(here("Tidy4/Output/plasticMDS.png"),
       width = 5, height = 5)

#or make base plot
ordiplot(ord, type = 'text', xlab='nMDS1', 
         ylab='nMDS2') #numbers are samples and text are plastic types
ordiellipse(ord, groups=plasdata$parent_company, label=F, kind='ehull', border=c('dodger blue', 'red'), 
            lwd=2, lty= "dashed", draw ='polygon')
legend('topleft',legend=c('Nestle', 'Coca-Cola'),
       col=c('dodger blue', 'red'), pch=19, bty='n')
