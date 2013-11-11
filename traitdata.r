# code to clean and organize the hummingbird trait datasets to be used in the project
# note that this dataset only includes information for male hummingbirds

library(ggplot2)
library(lattice)
library(GGally)

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

##weight
peso <- ggplot(traits, aes(x=Peso))
peso + geom_histogram(binwidth=0.5) + theme_bw() + 
  labs(x="Weight (g)", y = "Count")

##bill measurements
billlen <- ggplot(traits, aes(x=ExpC))
billlen + geom_histogram(binwidth = 1) + 
  theme_bw() + labs(x="Exposed Bill Length (mm)", y="Count") 

billwidth <- ggplot(traits, aes(x=Acom))
billwidth + geom_histogram(binwidth=0.5) + theme_bw() +
  labs(x="Bill width(mm)", y = "count")

billdepth <- ggplot(traits, aes(x=PrfP))
billdepth + geom_histogram(binwidth=0.25) + theme_bw() +
  labs (x="Bill depth (mm)", y = "Count")

#ignore TotC because it is well-predicted by ExpC, and ExpC is better predictor of flower use by hb

##wing measurements
wingchord <- ggplot(traits, aes(x=AlCdo))
wingchord + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing chord length (mm)", y = "Count")

wingwidth <- ggplot(traits, aes(x=AlAnc))
wingwidth + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing width (mm)", y = "Count")

winglength <- ggplot(traits, aes(x=AlLgo))
winglength + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing length(mm)", y = "count") 

wingload <- ggplot(traits, aes(WiLo))
wingload + geom_histogram(binwidth=0.05) + theme_bw() + 
  labs(x="wing load", y="Count")

wingratio <- ggplot(traits, aes(Rasp))
wingratio + geom_histogram(binwidth=0.25) + theme_bw() + 
  labs(x="wing length / wing width", y="Count")

wingtaper <- ggplot(traits, aes(Wtap))
wingtaper + geom_histogram(binwidth=0.05) + theme_bw() +
  labs(x="wing taper", y="Count")

wingarea <- ggplot(traits, aes(AlArea))
wingarea + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="total wing area", y="count")

##tail traits
taillength <- ggplot(traits, aes(ColaL))
taillength + geom_histogram(binwidth=1) + theme_bw() + 
  labs(x="tail length(mm)", y="Count")

##foot traits
footwidth <- ggplot(traits, aes(PataE))
footwidth + geom_histogram(binwidth=0.5) + theme_bw()  +
  labs(x="foot extension (mm)", y="Count")

tarslength <- ggplot(traits, aes(TarsL))
tarslength + geom_histogram(binwidth=0.5) + theme() +
  labs(x = "tarsus length (mm)", y="Count")

#pairs plot of all the data
par(mfrow = c(2,3))
pairs(traits[,5:21])
