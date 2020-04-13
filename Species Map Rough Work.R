# April 13th 2020
# Sasha Main
# Making Species Map
# This was originally supposed to be a map of clades- but that whole part of the project isn't working out so I'm just going to try to make
# a map of the most hit species in each state (and may in each airport if there's time!)

data<-read.csv("NEwDataset.csv", header=TRUE, stringsAsFactors = FALSE, row.names = "X")
head(data)
colnames(data)
dim(data)

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

# First make map coloured by the species hit the most in each state
length(unique(data$State)) # need to filter for US states
length(unique(usmap::us_map()$abbr)) # going to filter them based on the ones here
states<-c(unique(usmap::us_map()$abbr))
datas<-subset(data, data$State %in% states)
length(unique(datas$State))
dim(datas) #150951  by   19
dim(data)
length(unique(datas$Species.Name))
# all the species for these states- theres 651
sort(unique(datas$Species.Name))
# Remove unknown species
datas2<- datas[- grep("UNKNOWN", datas$Species.Name),]
dim(datas2) #87350  by  19 so it gets rid of a lot of entries
length(unique(datas2$Species.Name)) #645
colnames(datas2)
View(datas2)


# First look at the number of species hit in each state
head(datas2)
length(datas2$State)
test2<-c()
speciesnum<-c() #will be number of species hit in each state
for(i in 1:length(states)){
  test2<-datas2[datas2$State==states[i],c(7,17)]
  speciesnum<-append(speciesnum,length(unique(test2$Species.Name)))
}

speciesstrikes<-data.frame(state=states, SpeciesStrikes=speciesnum)
head(speciesstrikes)
View(speciesstrikes)
summary(speciesstrikes) # min 31 median 113 mean 120 max 279
speciesstrikes[speciesstrikes$SpeciesStrikes==max(speciesstrikes$SpeciesStrikes),] # TX has the highest diversity of strikes with 279
speciesstrikes[speciesstrikes$SpeciesStrikes==min(speciesstrikes$SpeciesStrikes),] # WY has the lowest diversity of strikes with 31
# not very interesting since it just matches the total number of strikes

# maybe should standardize by the number of airports again
test<-c()
airportnum<-c() #will be number of airports in each state
for(i in 1:length(states)){
  test<-datas[datas$State==states[i],c(6,7)]
  airportnum<-append(airportnum,length(unique(test$Airport)))
}
head(airportnum)
speciesstrikess<-data.frame(state=states, SpeciesStrikes=round(speciesnum/airportnum))
head(speciesstrikess)
View(speciesstrikess)
summary(speciesstrikess) # min 2 median 4 mean 5.25 max 60
speciesstrikess[speciesstrikess$SpeciesStrikes==max(speciesstrikess$SpeciesStrikes),] # DC with 60
speciesstrikess[speciesstrikess$SpeciesStrikes==min(speciesstrikess$SpeciesStrikes),]
dim(speciesstrikess[speciesstrikess$SpeciesStrikes==min(speciesstrikess$SpeciesStrikes),]) # 12 states with just 2 species
# Not super interesting either

# Find the species hit the most in each state
test3<-c()
length(states)
specieshitmax<-c() #will be number of species hit in each state
amountofhits<-c()
testtable<-c()
for(i in 1:51){
  test3<-datas2[datas2$State==states[i],c(7,17)]
  testtable<-as.data.frame(table(test3$Species.Name))
  specieshitmax<-append(specieshitmax,as.character(testtable[testtable$Freq==max(testtable$Freq),1]))
  amountofhits<-append(amountofhits,testtable[testtable$Freq==max(testtable$Freq),2])
}
length(specieshitmax)
specieshitmax
length(amountofhits)
amountofhits
states
# getting 52 instead of 51! There's a tie!
# Troubleshoot this- it's at index 17

# Fix loop
test3<-c()
length(states)
specieshitmax<-c() #will be number of species hit in each state
amountofhits<-c()
testtable<-c()
for(i in 1:length(states)){
  test3<-datas2[datas2$State==states[i],c(7,17)]
  testtable<-as.data.frame(table(test3$Species.Name))
  specieshitmax<-append(specieshitmax,paste(as.character(testtable[testtable$Freq==max(testtable$Freq),1]), collapse = "/"))
  amountofhits<-append(amountofhits,testtable[testtable$Freq==max(testtable$Freq),2][1])
}
length(specieshitmax)
specieshitmax
length(amountofhits)
amountofhits
states
speciesstrikes2<-data.frame(state=states, Species=specieshitmax, Hits=amountofhits)
head(speciesstrikes2)
View(speciesstrikes2)
summary(speciesstrikes2) # min 31 median 113 mean 120 max 279

# Now need to make a map
