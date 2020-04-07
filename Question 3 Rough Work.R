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
#install.packages("lubridate")
#install.packages("ggrepel")
#install.packages("tidyverse")
#install.packages("usmap")
#install.packages("maptools")
#install.packages("rgdal")
#install.packages("maps")
#install.packages("rworldmap")
#install.packages("magick")
#install.packages("rlang")

# Load libraries - do this every time
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(usmap)
library(maptools)
library(rgdal)
library(maps)
library(rworldmap)
library(magick)

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
summary(statestrike)
statestrike[statestrike$Strike==max(statestrike$Strikes),] 
# TX has the most strikes with 14854- but Texas is also a huge state (second largest after Alaska) so it may just have a lot of airports
statestrike[statestrike$Strike==min(statestrike$Strikes),] 
# DE has the least strikes with 166- but DE is also the second smallest state after Rhode Island- so it may not have very many airports

p<-plot_usmap(data = statestrike, values = "Strikes", color = "white") + 
  scale_fill_continuous(name = "Number of Bird Strikes", label = scales::comma) + 
  theme(legend.position = "right")
p

# Scale the bird strikes by number of airports in each state
# Some states have more airports than others, so may not be a fair comparison
head(datas)
length(datas$State)
test2<-c()
airportnum<-c() #will be number of airports in each state
for(i in 1:length(states)){
  test2<-datas[datas$State==states[i],c(6,7)]
  airportnum<-append(airportnum,length(unique(test2$Airport)))
}
statestrikean<-data.frame(state=states, Strikes=round(counts/airportnum))
head(statestrikean)
summary(statestrikean)
statestrikean[statestrikean$Strike==max(statestrikean$Strikes),] # DC has the most strikes with 1475
statestrikean[statestrikean$Strike==min(statestrikean$Strikes),] # WY has the least strikes with 11

p1<-plot_usmap(data = statestrikean, values = "Strikes", color = "grey") + 
  scale_fill_continuous(low= "white", high= "cadetblue",name = "Number of Bird Strikes per Airport in each State", label = scales::comma) + 
  theme(legend.position = "right")
p1
# The whole map is very light because DC is so much higher than everything else, yet very small
# Need to show DC better
pp1<-plot_usmap(data = statestrikean, values = "Strikes", color = "grey", include=c("DC","VA","MD","WD")) + 
  scale_fill_continuous(low= "white", high= "cadetblue",name = "Number of Bird Strikes per Airport in each State", label = scales::comma) + 
  theme(legend.position = "right")
pp1

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

length(unique(latmis1)) #only missing 37 out of the 1914 airports in the US
length(unique(longmis1))

# Filter out missing airports
head(datas)
dim(datas)
misports<-c(unique(latmis1))
'%!in%' <- function(x,y)!('%in%'(x,y))
ndatas<-subset(datas,datas$Airport.ID %!in% misports)
dim(ndatas)

# Find the count of strikes in each state for the new filtered airport dataset
ncounts<-rep(NA,51)
for(i in 1:51){
  ncounts[i]<-sum(ndatas$State==states[i])
}

# Make a new dataset of the states and strikes
nstatestrike<-data.frame(state=states, Strikes=ncounts)
head(nstatestrike)

# Make a new dataset of the airports and strikes
length(unique(ndatas$Airport.ID))
usairports<-unique(ndatas$Airport.ID)
portstrikescounts<-rep(NA,length(unique(ndatas$Airport.ID)))
for(i in 1:length(unique(ndatas$Airport.ID))){
  portstrikescounts[i]<-sum(ndatas$Airport.ID==usairports[i])
}

airportstrikes<-data.frame(Airport=usairports, Strikes=portstrikescounts)
head(airportstrikes)

# Run loop with new airports and strikes dataset
nlongcor<-rep(NA,length(airportstrikes$Airport))
nlatcor<-rep(NA,length(airportstrikes$Airport))
nlatmis<-c()
nlongmis<-c()
for(i in 1:length(airportstrikes$Airport)){
  if(length(usair[grep(airportstrikes$Airport[i],usair$ident),]$longitude_deg)==0){
    nlongmis<-append(nlongmis,airportstrikes$Airport[i])
    nlongcor[i]<-NA
  }else{
    nlongcor[i]<-usair[grep(airportstrikes$Airport[i],usair$ident),]$longitude_deg
  }
  if(length(usair[grep(airportstrikes$Airport[i],usair$ident),]$latitude_deg)==0){
    nlatcor[i]<-NA
    nlatmis<-append(nlatmis,airportstrikes$Airport[i])
  }else{
    nlatcor[i]<-usair[grep(airportstrikes$Airport[i],usair$ident),]$latitude_deg
  }
}

length(unique(nlatmis)) #these are zero as they should be
length(unique(nlongmis))

# Add the coordinates to the new airport and strikes dataset
airportstrikes$Longitude<-nlongcor
airportstrikes$Latitude<-nlatcor
head(airportstrikes)
airportstrikes$Longitude<-as.numeric(airportstrikes$Longitude)
airportstrikes$Latitude<-as.numeric(airportstrikes$Latitude)
airportstrikes$Strikes<-as.numeric(airportstrikes$Strikes)
str(airportstrikes)

# Makea map using both datasets
p2<-plot_usmap(data = nstatestrike, values = "Strikes", color = "grey") + 
  scale_fill_continuous(low= "white", high= "cadetblue",name = "Number of Bird Strikes per State", label = scales::comma) + 
  theme(legend.position = "right")
p2

test_data <- data.frame(lon = as.numeric(nlongcor), lat = as.numeric(nlatcor), strike=portstrikescounts)

transformed_data <- usmap_transform(test_data)

dim(test_data)
dim(transformed_data)
head(test_data)
head(transformed_data) #strange that I loose two rows when I transform the data
tail(test_data)
tail(transformed_data) #missing the last five rows from test_data?

plot_usmap("states") + 
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1, size = strike), 
             color = "red",
             alpha = 0.25)+
  labs(title = "US Bird Strikes per Airport",
       size = "Number of Strikes") +
  theme(legend.position = "right")

#Overlay the second map 
p3<-p2 + 
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1, size = strike), 
             color = "orange",
             alpha = 0.25)+
  labs(title = "US Bird Strikes per Airport",
       size = "Number of Strikes per Airport") +
  theme(legend.position = "right")
p3

# Make a world map
map('world', fill = TRUE, col = "grey")

prelim_plot <- ggplot(airportdata, aes(x = Longitude, y = Latitude)) +
    geom_point()
prelim_plot                

world <- getMap(resolution = "low")

with_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = airportdata, 
               aes(x = Longitude, y = Latitude, colour="orange")) +
    coord_quickmap() +  
    theme_classic() +  
    xlab("Longitude") +
    ylab("Latitude") 
with_world

# Make a new dataset of global airports and strikes
head(data)
head(airportdata)
length(unique(data$Airport.ID))
globalairports<-unique(data$Airport.ID)
gloportstrikescounts<-rep(NA,length(unique(data$Airport.ID)))
for(i in 1:length(unique(data$Airport.ID))){
  gloportstrikescounts[i]<-sum(data$Airport.ID==globalairports[i])
}

globalairportstrikes<-data.frame(Airport=globalairports, Strikes=gloportstrikescounts)
head(globalairportstrikes)
dim(globalairportstrikes)
summary(globalairportstrikes)
globalairportstrikes[globalairportstrikes$Strike==max(globalairportstrikes$Strikes),] 
# Airport ZZZZ has the most strikes with 18570 which makes sense since according to wikipedia
# "ZZZZ is a special code which is used when no ICAO code exists for the airport. It is often used by helicopters not operating at an aerodrome."
globalairportstrikes[globalairportstrikes$Strike==min(globalairportstrikes$Strikes),] # There's 2147 airports all with only 1 recorded strike

# Subset global airport and strikes dataset by the airports included in the airportdata
nglobalairportstrikes<-subset(globalairportstrikes, globalairportstrikes$Airport %in% airportdata$`ICAO Code`)
dim(globalairportstrikes)
dim(nglobalairportstrikes) #639 global airports (missing a lot of those US airports probably)
summary(nglobalairportstrikes)
nglobalairportstrikes[nglobalairportstrikes$Strike==max(nglobalairportstrikes$Strikes),] 
# KDEN airport has the most strikes with 5434
nglobalairportstrikes[nglobalairportstrikes$Strike==min(nglobalairportstrikes$Strikes),]
# 2223 airports all have only 1 recorded strike

longcoor<-rep(NA,length(nglobalairportstrikes$Airport))
latcoor<-rep(NA,length(nglobalairportstrikes$Airport))
latmiss<-c()
longmiss<-c()
for(i in 1:length(nglobalairportstrikes$Airport)){
  if(length(airportdata[grep(nglobalairportstrikes$Airport[i],airportdata$`ICAO Code`),]$Longitude)==0){
    longmiss<-append(longmiss,nglobalairportstrikes$Airport[i])
    longcoor[i]<-NA
  }else{
    longcoor[i]<-airportdata[grep(nglobalairportstrikes$Airport[i],airportdata$`ICAO Code`),]$Longitude
  }
  if(length(airportdata[grep(nglobalairportstrikes$Airport[i],airportdata$`ICAO Code`),]$Latitude)==0){
    latcoor[i]<-NA
    latmiss<-append(latmiss,nglobalairportstrikes$Airport[i])
  }else{
    latcoor[i]<-airportdata[grep(nglobalairportstrikes$Airport[i],airportdata$`ICAO Code`),]$Latitude
  }
}

length(unique(latmiss)) #zero as it should be
length(unique(data$Airport.ID)) #2228

# Add the coordinates to the new airport and strikes dataset
nglobalairportstrikes$Longitude<-as.numeric(longcoor)
nglobalairportstrikes$Latitude<-as.numeric(latcoor)
head(nglobalairportstrikes)
nglobalairportstrikes$Strikes<-as.numeric(nglobalairportstrikes$Strikes)
str(nglobalairportstrikes)

# Plot
map1 <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "cadetblue", colour = "grey") + 
  geom_point(data = nglobalairportstrikes, 
             aes(x = Longitude, y = Latitude, size= Strikes),colour="orange",alpha = 0.25) +
  labs(title = "Global Bird Strikes per Airport",
       size = "Number of Strikes per Airport")+
  coord_quickmap() +  
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
map1

# I should combine the two data sets- the global and US airports so that overall more of the data is included in the worldwide map
head(nglobalairportstrikes)
dim(nglobalairportstrikes)
head(airportstrikes)
dim(airportstrikes)
allairportstrikes<-rbind(nglobalairportstrikes,airportstrikes)
dim(allairportstrikes)
length(unique(allairportstrikes$Airport))
# Need to remove all duplicate airports
abcairports<-allairportstrikes[order(allairportstrikes$Airport),] #let's us easily see the duplicates
uniqueallairports<-allairportstrikes[!duplicated(allairportstrikes$Airport),]
uniqueallairportss<-uniqueallairports[order(uniqueallairports$Airport),] #let's us easily check no more duplicates

# Now can remake the world map with way more points
map2 <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "darkslategray3", colour = "white") + 
  geom_point(data = uniqueallairports, 
             aes(x = Longitude, y = Latitude, size= Strikes),colour="tomato",alpha = 0.25) +
  labs(title = "Global Bird Strikes per Airport",
       size = "Number of Strikes per Airport")+
  coord_quickmap() +  
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
map2

# Now need to figure out how to overlay maps/ how to make a map of the migration pathways
# See if we can save the world map as transparent- could take the points and overlay them on the migration image
map3 <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "darkslategray3", colour = "white") + 
  geom_point(data = uniqueallairports, 
             aes(x = Longitude, y = Latitude, size= Strikes),colour="tomato",alpha = 0.25) +
  labs(title = "Global Bird Strikes per Airport",
       size = "Number of Strikes per Airport")+
  coord_quickmap() +  
  theme(
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.border=element_blank()
  )
map3
ggsave("test.png", map3, bg = "transparent") 
# Awesome! That saved it transparent!

# Probably only want to save the points and border though- no fill
map4 <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "grey") + 
  geom_point(data = uniqueallairports, 
             aes(x = Longitude, y = Latitude, size= Strikes),colour="tomato",alpha = 0.25) +
  labs(title = "Global Bird Strikes per Airport",
       size = "Number of Strikes per Airport")+
  coord_quickmap() +  
  theme(
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.border=element_blank()
  )
map4
ggsave("maptest.png", map4, bg = "transparent",width = 90,height =70,units = "cm") # needs to be pretty big so its not too crowded

# Now it makes the most sense only look at North America and South America on the world map
# I think it may be easiest to just crop and blow up the original world map, rather than remake the map with only north and latin america

testmap <- image_read('maptest.png')
print(testmap)

testmap1<-image_crop(testmap, "4500x6000")
image_browse(testmap1)

# I downloaded a lot of bird migration diagrams- but finding a good one to overlay may be difficult
