#install.packages('rgeos', type='source')
#install.packages('rgdal', type='source')
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)
library(sf)
library(rgdal)
library(dplyr)

## Shape file of India downloaded 
# from https://drive.google.com/drive/folders/0ByLjiBJ1xOpuTVRIWThhazBLdWM
# With Telangana, Chattisgarh and J&K

#Read India shape file
shp <- readOGR('D:/Harish/R practice projects/india/NewIndiawithTel/India.shp') 

names(shp)
str(shp)
plot(shp)
head(shp, n=1)
names(shp)

#NAME_1 contains the name of states
#Fortify converts the shape file to a dataframe
shp.fort <- fortify(shp, region="NAME_1")

head(shp.fort)
#Downloade India road accident data 
#Downloaded from https://data.gov.in/catalog/total-number-road-accidents-india
#the state name in acc_facts is put as "id" to merge it with India shape file
acc_facts <- read.csv('/Harish/R practice projects/india/Accidental data 2015 1.csv')

#merged shape with accident file on state (ie id)
merged <-merge(shp.fort,acc_facts, by="id",all.x=TRUE)

head(merged)
#order the data
f.plot.india<-merged[order(merged$order), ]

head(f.plot.india)

#we want to display State name with Total deaths on map
Stnames_TDeath <- aggregate(cbind(long, lat) ~ id + TotalDeath, data=f.plot.india, FUN=function(x) mean(range(x)))

head(Stnames_TDeath)
nrow(Stnames_TDeath)

#plotting Simple India map showing Total deaths with color accross different states, with UP being the highest
ggplot()+
  geom_polygon(data = f.plot.india,aes(x = long, y = lat, group = group, fill=TotalDeath),color = "orange", size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude')


#create map with different color
ggplot()+
  geom_polygon(data = f.plot.india,aes(x = long, y = lat, group = group, fill=TotalDeath),color = "orange", size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude')+
  scale_fill_distiller(name="Total Death", palette = "Spectral")

#create map with Accident counts
ggplot()+
  geom_polygon(data = f.plot.india,aes(x = long, y = lat, 
  group = group, fill=TotalDeath),color = "orange", size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude')+
  scale_fill_distiller(name="Total Death", palette = "RdYlBu") +
    geom_text(data=Stnames_TDeath, aes(long, lat, label = TotalDeath), size=2, fontface="bold") 

#create map with State names
#create map with Accident counts and Title
ggplot()+
  geom_polygon(data = f.plot.india,aes(x = long, y = lat, 
  group = group, fill=TotalDeath),color = "orange", size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude')+
  scale_fill_distiller(name="Total Death", palette = "Accent") +
  geom_text(data=cnames, aes(long, lat, label = id), size=1.8, fontface="bold") +
  labs(title="Road Accidents death in India - 2015")


#create map to display state name and accident deaths together
ggplot()+
  geom_polygon(data = f.plot.india,aes(x = long, y = lat, 
  group = group, fill=TotalDeath),color = "orange", size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude')+
  scale_fill_distiller(name="acc_facts", palette = "YlGn") +
  geom_text(data=cnames, aes(long, lat, label = id), size=1.90, fontface="bold") +
  geom_text(data=cnames, aes(long, lat, label = TotalDeath, vjust = 2.5), size=1.90)+
  labs(title="Road Accidents death in India - 2015") 