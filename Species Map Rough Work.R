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


