#Tidy Tuesday 6
#04/16/2021
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(factoextra)
library(devtools)
library(ggbiplot)
library(ggrepel)

#load data
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

#quick look at each
head(forest_area)
head(brazil_loss)
head(soybean_use)

#ok first maybe look at forest area change against time for 4 major countries
#that have lots of forest
df<-forest_area %>%
  filter(entity == c("Brazil", "Canada", "Russia", "China"))
df<-as.data.frame(df)
  rename(df, Country = entity) #why is rename() not working??
view(df)

#make ggplot
df %>%
  ggplot(aes(x = year, y = forest_area, color = entity))+
  geom_point(shape = 21, size = 4)+
  theme_minimal()+
  scale_color_hue()+
  xlab("Year")+
  ylab("Forest Area")

#hmm only brazil has decreased
#two-way anova for fun
glm<-lm(forest_area~year + Country + year:Country, data=df)
anova(glm) #interaction P = wow.. well the countries are all different sizes

#alright
#feeling like putting brazil loss into a PCA because there are lots of variables
#and they might show some interesting relationships

#little housekeeping (just want year as chr)
brazildf1<-brazil_loss[,-c(1:2)]
view(brazildf1)

#prep data for matrix analysis
brazildf2<-brazil_loss[,-c(1:3)]
# and then scale data
brazildf3<-scale(brazildf2, scale=TRUE, center=TRUE)

#let's rename the variables
head(brazildf3)
brazildf3 %>%
  rename(Disturbance = natural_disturbances, Pasture = pasture,
         Logging = selective_logging, Fire = fire, Mining = mining,
         Infrastructure = other_infrastructure, Roads = roads,
         Plantations = tree_plantations_including_palm, Clearing = 
         small_scale_clearing) #why doesn't this work!?
head(brazildf3) 

#makePCA
PCA<-princomp(brazildf3, cor=FALSE)
fviz_eig(PCA) #most variance is in PC1

#make ggplot
PCAplot<-ggbiplot(PCA, obs.scale=1, var.scale=1, repel = TRUE, 
                  varname.size=3, varname.adjust=1.2)+
  scale_color_discrete(name='') +
  geom_point(shape=21)+
  theme_classic()+
  geom_vline(xintercept=0, linetype="dashed", color = "black")+ 
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylim(-4,4)
PCAplot #54% combined variance explained 
