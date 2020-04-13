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

# Find the species hit the most in each state
test3<-c()
specieshitmax<-c() #will be number of species hit in each state
for(i in 1:length(states)){
  test3<-datas2[datas2$State==states[i],c(7,17)]
  specieshitmax<-append(specieshitmax,length(unique(test2$Species.Name)))
}

levels(test2$Species.Name)

