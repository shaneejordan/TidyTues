#Tidy Tuesday 4
#04/05/2021
#Created by Shane E. Jordan

#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(rockchalk)
library(plotly)
library(ggplot2)
library(rayshader)

#load data
shades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

#let's make some linear models with the numeric data
#want to see how lightness depends upon hue and saturation
lmhue<-lm(lightness~hue, data=shades)
lmsat<-lm(lightness~sat, data=shades)
plot(lightness~hue, data=shades) #interesting linear pattern
plot(lightness~sat, data=shades) #not much of a linear relationship
summary(lmhue)
summary(lmsat)

#let's take a multivariate approach
mulreg<-lm(lightness~hue + sat, data=shades)
summary(mulreg)

#little model selection
AIC(lmhue) #lowest AIC value, hue alone is a sufficient predictor 
AIC(lmsat)
AIC(mulreg)

#make a basic 3D plot
plotPlane(mulreg, plotx1="hue", plotx2="sat") #kind of cool 

#ok let's try something different with plotly
#seems like we need to put the numeric data into a matrix
shades1<-shades[,-c(1:10)]
shades1<-as.data.frame(shades1)
as.matrix(shades1, rownames.force = NA)

#plotly is capable of making 3D plots with matrix data
plot<-plot_ly(z = ~shades1)
plot<-plot %>% add_surface()
plot #hmm doesn't seem to work with my matrix data

plot_ly(shadesdf, x=hue, y=sat, z=lightness, type="scatter3d", mode="markers", color=lightness)
#not much luck with plotly...

#let's try a ggplot
XYZshades <-shades %>%
  ggplot(aes(x=sat, y=lightness, color=hue))+
  geom_point()+
  scale_color_viridis_c(option = "C")+
  theme_classic()+
  xlab("Saturation")+
  ylab("Lightness")+
  theme(axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"))

#now take the ggplot and make it ~interactive
plot_gg(XYZshades,multicore=TRUE,width=5,height=5,scale=250) 
render_camera(zoom = 0.7) #make it a little bigger than the default
render_snapshot("XYZplot") #saves the plot as a .png
