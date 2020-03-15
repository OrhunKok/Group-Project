# Sasha's Script
# March 12th 2020
# Working on Question 3:
# We will look into which region is the worst offender for bird strikes. 
# We will convert the airport names into airport coordinates so that geospatial packages like ggmaps, maps, mapdata, sp, and ggplot will be able to process the data effectively.
# We will then colour code each state by bird strike quantity that has occurred within their borders, with each airport marked with circle size proportional to strike count. 
# To tie our questions together, we will use a second map where the regions are colour coded by the most common clade of bird to be struck in each area. 
# We will then overlay a map of the four major migration pathways of birds in North America (Atlantic, Mississippi, Central, and Pacific) on top of our bird strike frequency map.
# Around 40% of all migrating waterfowl use the Mississippi Flyway, so we hypothesize that bird strikes of waterfowl will be significantly higher along this route.

data<-read.csv("NEwDataset.csv", header=TRUE, stringsAsFactors = FALSE, row.names = "X")
head(data)
colnames(data)

# To address this question I've downloaded a Global Airport Database from https://www.partow.net/miscellaneous/airportdatabase/
# With this database I will be able to get the coordinates of each airport
airportdata<-read.table("GlobalAirportDatabase.txt", header= FALSE, sep=":",quote = "")
head(airportdata)
dim(airportdata)

# Set the column names as they're outlined on the website
columnnames<-c("ICAO Code","IATA Code","Airport Name","City/Town","Country","Latitude Degrees","Latitude Minutes","Latitude Seconds","Latitude Direction","Longitude Degrees","Longitude Minutes","Longitude Seconds","Longitude Direction","Altitude","Latitude","Longitude")
colnames(airportdata)<-columnnames
head(airportdata)

# Install libraries - do this one time
install.packages("lubridate")
install.packages("ggrepel")
install.packages("tidyverse")
install.packages("usmap")

# Load libraries - do this every time
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(usmap)

# First going to make map highlighting the number of bird strikes by state
length(unique(data$State)) # concerning there are 63 states in here, that's too many
length(unique(usmap::us_map()$abbr)) # going to filter them based on the ones here
states<-c(unique(usmap::us_map()$abbr))
datas<-subset(data, data$State %in% states)
length(unique(datas$State))
dim(datas)
dim(data)
length(unique(datas$Airport.ID))

sum(datas$State==states[1])

# Find the count of strikes in each state
counts<-rep(NA,51)
for(i in 1:51){
  counts[i]<-sum(datas$State==states[i])
}

# Make a new dataset
statestrike<-data.frame(state=states, Strikes=counts)
head(statestrike)

p<-plot_usmap(data = statestrike, values = "Strikes", color = "white") + 
  scale_fill_continuous(name = "Number of Bird Strikes", label = scales::comma) + 
  theme(legend.position = "right")
p
# Could come back to this later and scale the bird strikes by size

# Add the longitude and latitude to each observation based off the airport
head(data)
head(data,10)
head(airportdata) # I think I need to use the ICAO code since it's four letters
# Tried the code below using the original airport data, but there's missing values
# Also realized it would incorporate other worldwide airports, and just looking at the US for now
# So going to use the previously made datas
longcor<-rep(NA,length(datas$Airport.ID))
latcor<-rep(NA,length(datas$Airport.ID))
latmis<-c()
longmis<-c()
for(i in 1:length(datas$Airport.ID)){
  if(length(airportdata[grep(datas$Airport.ID[i],airportdata$`ICAO Code`),]$Longitude)==0){
    longmis<-append(longmis,datas$Airport.ID[i])
    longcor[i]<-NA
  }else{
    longcor[i]<-airportdata[grep(datas$Airport.ID[i],airportdata$`ICAO Code`),]$Longitude
  }
  if(length(airportdata[grep(datas$Airport.ID[i],airportdata$`ICAO Code`),]$Latitude)==0){
    latcor[i]<-NA
    latmis<-append(latmis,datas$Airport.ID[i])
  }else{
    latcor[i]<-airportdata[grep(datas$Airport.ID[i],airportdata$`ICAO Code`),]$Latitude
  }
}

length(unique(latmis)) #1553 airports missing from dataset, but only 1914 to begin with
length(unique(longmis))
# So unfortunately this Global Airport database doesn't contain many of the US airports
# So I've downloaded a USA specific dataset which may be better

# Load new US airport data from https://data.humdata.org/dataset/ourairports-usa#
usair<-read.csv("us-airports.csv", header=TRUE, stringsAsFactors = FALSE)
head(usair)

# Remove first row
usair<-usair[-1,]
head(usair)
dim(usair)

# Try the loop again
longcor1<-rep(NA,length(datas$Airport.ID))
latcor1<-rep(NA,length(datas$Airport.ID))
latmis1<-c()
longmis1<-c()
for(i in 1:length(datas$Airport.ID)){
  if(length(usair[grep(datas$Airport.ID[i],usair$ident),]$longitude_deg)==0){
    longmis1<-append(longmis1,datas$Airport.ID[i])
    longcor1[i]<-NA
  }else{
    longcor1[i]<-usair[grep(datas$Airport.ID[i],usair$ident),]$longitude_deg
  }
  if(length(usair[grep(datas$Airport.ID[i],usair$ident),]$latitude_deg)==0){
    latcor1[i]<-NA
    latmis1<-append(latmis1,datas$Airport.ID[i])
  }else{
    latcor1[i]<-usair[grep(datas$Airport.ID[i],usair$ident),]$latitude_deg
  }
}

length(unique(latmis1)) #
length(unique(longmis1))









airportdata[grep(data$Airport.ID[1],airportdata$`ICAO Code`),]$Latitude
airportdata[grep("KJWN", airportdata$`ICAO Code`),]

eq_transformed <- usmap_transform(earthquakes)

plot_usmap() +
  geom_point(data = eq_transformed, aes(x = lon.1, y = lat.1, size = mag),
             color = "red", alpha = 0.25) +
  labs(title = "US Earthquakes",
       subtitle = "Source: USGS, Jan 1 to Jun 30 2019",
       size = "Magnitude") +
  theme(legend.position = "right")