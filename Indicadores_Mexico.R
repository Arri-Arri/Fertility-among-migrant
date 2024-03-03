##Graficas Mexico-cdd##


rm(list=ls())
gc()
setwd("C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/ADD-II/Tabela_Multiestado")
library(tidyverse)
library(car) 
library(fdth)
library(data.table)
library(EDAWR)
library(ggplot2)
library(plyr)
library(readr)
Share <- read_csv("Grap2.csv")
View(Share)
str(Share)

 sp = ggplot(Share, aes(x = edad_ola1, y = casos, color = country)) +
  geom_point(alpha = 0.9) +
  theme_minimal()+
   labs(x = "Edad", y = "Observaciones", color = "Pais") 

  sp + scale_color_hue(l=40, c=60) 
  ggsave( filename  =  './Grafica1.jpg', width = 6, height = 4, dpi = 600)
  
  
  sp1 = ggplot(Share, aes(x = edad_ola1, y = casos, color = Esta_0_cate)) +
    geom_point(alpha = 0.9) +
    theme_minimal()+
    scale_colour_manual(values = c("#104E8B", '#008B00', "#ff6d2a")) +
    labs(x = "Edad", y = "Observaciones", color = "Estado") 
  
  sp + scale_color_hue(l=40, c=60) 
  ggsave( filename  =  './Grafica1.jpg', width = 6, height = 4, dpi = 600)
  
  Share1 <- read_csv("Grap3.csv")
  View(Share1)
  
  
  



  

http://127.0.0.1:41801/graphics/plot_zoom_png?width=423&height=335