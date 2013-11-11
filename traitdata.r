# code to clean and organize the hummingbird trait datasets to be used in the project

library(ggplot2)
library(ggmap)

#import the data
wd = "C:\\Users\\sarah\\Dropbox\\ActiveResearchProjects\\HummingbirdTraits\\"
setwd(wd)

traits = read.table("Morphology.txt", header = T, sep=",", na.strings=9999)
refs = read.table("References.txt", header = T, sep = ",")
species = read.table("AouSaccHumList.txt", header = T, sep=",")

#find and subset only Gary Stiles' data (probably the most trusted data)
Stiles = subset(refs, Title == "Gary Stiles Personal database")
GS_ID = index$RefID

traits = subset(traits, ReferenceID == GS_ID)

#replace 9999 with NA
traits[traits == 9999] <- NA

#plot stuff


