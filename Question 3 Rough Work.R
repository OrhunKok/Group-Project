# Sasha's Script
# March 12th 2020
# Working on Question 3:
# We will look into which region is the worst offender for bird strikes. 
# We will convert the airport names into airport coordinates so that geospatial packages like ggmaps, maps, mapdata, sp, and ggplot will be able to process the data effectively.
# We will then colour code each state by bird strike quantity that has occurred within their borders, with each airport marked with circle size proportional to strike count. 
# To tie our questions together, we will use a second map where the regions are colour coded by the most common clade of bird to be struck in each area. 
# We will then overlay a map of the four major migration pathways of birds in North America (Atlantic, Mississippi, Central, and Pacific) on top of our bird strike frequency map.
# Around 40% of all migrating waterfowl use the Mississippi Flyway, so we hypothesize that bird strikes of waterfowl will be significantly higher along this route.

data<-read.csv("database.csv", header=TRUE, stringsAsFactors = FALSE)
head(data)
colnames(data)

# To address this question I've downloaded a Global Airport Database from https://www.partow.net/miscellaneous/airportdatabase/
# With this database I will be able to get the coordinates of each airport
airportdata<-read.table("GlobalAirportDatabase.txt", header= FALSE, sep=":",quote = "")
head(airportdata)
dim(airportdata)

# I need to set the column names as they're outlined on the website
columnnames<-c("ICAO Code","IATA Code","Airport Name","City/Town","Country","Latitude Degrees","Latitude Minutes","Latitude Seconds","Latitude Direction","Longitude Degrees","Longitude Minutes","Longitude Seconds","Longitude Direction","Altitude","Latitude","Longitude")
colnames(airportdata)<-columnnames
head(airportdata)
