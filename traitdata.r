# code to clean and organize the hummingbird trait datasets to be used in the project
# note that this dataset only includes information for male hummingbirds

#library(lattice)
#library(GGally)
library(ggplot2)
library(ggmap)
library(vegan)
#library(data.table)
library(reshape)

#import the data
wd = "C:\\Users\\sarah\\Dropbox\\ActiveResearchProjects\\HummingbirdTraits\\Data\\"
#wd = "/Users/sarah/Desktop/Dropbox/ActiveResearchProjects/HummingbirdTraits/data"
setwd(wd)

#------------------------------------------
#         Import and clean the data
#------------------------------------------

traits = read.table("Morphology.txt", header = T, sep = ",", na.strings=9999)
refs = read.table("References.txt", header = T, sep = ",")
species = read.table("AouSaccHumList.txt", header = T, sep = ",")
geo = read.table("GeoRefs.txt", header = T, sep = ",")
sites = read.csv("Sites_BW.csv", header = T, sep = ",")
sitexspp = read.csv("SiteXspp_BW.csv", header = T, sep = ",")
  sitexspp= sitexspp[-c(1:3), ] #don't need the elevation data

#find and subset only Gary Stiles' data (probably the most trusted data)
Stiles = subset(refs, Title == "Gary Stiles Personal database")
GS_ID = Stiles$RefID
traits = subset(traits, ReferenceID == GS_ID)
  traits[traits == 9999] <- NA

#Use only Colombian assemblages
colgeo = subset(geo, Country == "Colombia") #369 sites
colsites = subset(sites, Country == "Colombia") #124 sites (what did Ben clean/remove?)
  sitenames = unique(colsites$Community)

#include only sites that are in Colombia
use = as.numeric()
s = as.numeric()

for (iRow in 1:nrow(sitexspp)){
  if (sitexspp[iRow,1] %in% sitenames){
    use[iRow] = 1
    pres = as.numeric(sitexspp[iRow,2:134])
    s[iRow] = specnumber(pres)
  }
  else { 
    use[iRow] = 0
    s[iRow] = 0
  }
}
sitexspp$use = use
sitexspp$s = s

#remove sites that are not cleaned & in Colombia
sitexspp = sitexspp[which(sitexspp$use == 1),]

#------------------------------------------
#        Map of Colombia and Bogota sites
#------------------------------------------
##map of Colombian Sites
colombia = get_map(location = "Bogota", zoom = 7, maptype = "terrain", color = "bw")
ggmap(colombia) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), data = colgeo)

colombiasites = ggmap(colombia) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), data = colsites, cex = 4)

#Just sites near Bogota
bogosites = colsites[which(colsites$LongDecDeg < -73.5  & colsites$LongDecDeg > -75.25 & 
                             colsites$LatDecDeg > 3 & colsites$LatDecDeg < 5.5),]

bogota = get_map(location = "Bogota", zoom = 8, maptype = "terrain", color = "bw")

sitemap = ggmap(bogota) + geom_point(aes(x = LongDecDeg, y = LatDecDeg, col = Biome, size = Richness), 
                           data = bogosites) + element_blank() + scale_fill_brewer(palette=1)

sitemap2 = ggmap(bogota) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), size = 5, 
                                     data = bogosites) + element_blank() + scale_fill_brewer(palette=1)

siterichness = ggplot(bogosites, aes(LongDecDeg, LatDecDeg)) + geom_point(aes(size = Richness))


#------------------------------------------
#           Plot the trait data
#------------------------------------------
##weight
peso <- ggplot(traits, aes(x=Peso)) + geom_histogram(binwidth=0.5) + theme_bw() + 
  labs(x="Weight (g)", y = "Count")

##bill measurements
billlen <- ggplot(traits, aes(x=ExpC)) + geom_histogram(binwidth = 1) + 
  theme_bw() + labs(x="Exposed Bill Length (mm)", y="Count") 

billwidth <- ggplot(traits, aes(x=Acom)) + geom_histogram(binwidth=0.5) + theme_bw() +
  labs(x="Bill width(mm)", y = "count")

billdepth <- ggplot(traits, aes(x=PrfP)) + geom_histogram(binwidth=0.25) + theme_bw() +
  labs (x="Bill depth (mm)", y = "Count")

#ignore TotC because it is well-predicted by ExpC, and ExpC is better predictor of flower use by hb

##wing measurements
wingchord <- ggplot(traits, aes(x=AlCdo)) + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing chord length (mm)", y = "Count")

wingwidth <- ggplot(traits, aes(x=AlAnc))+ geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing width (mm)", y = "Count") 

winglength <- ggplot(traits, aes(x=AlLgo)) + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="wing length(mm)", y = "count")

wingload <- ggplot(traits, aes(WiLo)) + geom_histogram(binwidth=0.05) + theme_bw() + 
  labs(x="wing load", y="Count")

wingratio <- ggplot(traits, aes(Rasp)) + geom_histogram(binwidth=0.25) + theme_bw() + 
  labs(x="wing length / wing width", y="Count")

wingtaper <- ggplot(traits, aes(Wtap))  + geom_histogram(binwidth=0.05) + theme_bw() +
  labs(x="wing taper", y="Count")

wingarea <- ggplot(traits, aes(AlArea)) + geom_histogram(binwidth=1) + theme_bw() +
  labs(x="total wing area", y="count")

##tail traits
taillength <- ggplot(traits, aes(ColaL)) + geom_histogram(binwidth=1) + theme_bw() + 
  labs(x="tail length(mm)", y="Count")

##foot traits
footwidth <- ggplot(traits, aes(PataE)) + geom_histogram(binwidth=0.5) + theme_bw()  +
  labs(x="foot extension (mm)", y="Count") 

tarslength <- ggplot(traits, aes(TarsL)) + geom_histogram(binwidth=0.5) + theme_bw() +
  labs(x = "tarsus length (mm)", y="Count")
 
#pairs plot of all the data
par(mfrow = c(2,3))
pairs(traits[,5:21])

#------------------------------------------
#    Link species, locality and trait data
#------------------------------------------
#add a column to species table that will match species name format in sitexspp table
species$spname_dot = paste(species$Genus, species$Species, sep = ".")

#add species name to traits data
sp = unique(traits$SpID)
traits$spname=NA

for (row in 1:nrow(traits)){
  id = traits[row, 2]
  spdat = species[which(species$SpID == id), c(3,4)]
  traits[row,]$spname = paste(spdat[1,1], spdat[1,2], sep = ".")   
  }

#keep only sites in sitexspp table that correspond to cleaned Bogota sites
sitenames = unique(bogosites$Community)
#remove sites that are not cleaned & in Colombia
sitexspp = sitexspp[which(sitexspp$X %in% sitenames),]

#make a new dataframe for the traits in all the sites and species
commtraits = data.frame(comm="name", biome="name", species="name", mass=0, billwidth=0, billlength=0, wingchord=0,
                        wingarea=0, wingload=0, taillength=0, tarsuslength=0)
  levels(commtraits$comm) = unique(sitexspp$X)
  levels(commtraits$biome) = as.character(c(1:8))
  levels(commtraits$species) = names(sitexspp[,2:134])
counter = 1

for (row in 1:nrow(sitexspp)){
  sitename = sitexspp[row,1]
    biome = bogosites[which(bogosites$Community == sitename),8]
  dat = sitexspp[row,c(1:134)]
    dat = cbind(dat, biome)
  dat2=melt(dat, id = c("X", "biome"))
  dat3 = dat2[which(dat2$value == 1),]
    names(dat3) = c("comm", "biome", "species", "presence")
  names = dat3$species
  for (n in 1:length(names)){
    traitdat = traits[which(traits$spname == names[n]),c(6,7,8,9,17,15,18,20)]
    vals = c()
    for (t in 1:ncol(traitdat)){
      vals = append(vals, mean(traitdat[,t]))  #if a species appears more than once in traits, take the mean value
    }
    vec = c(as.character(dat3$comm[1]), as.character(dat3$biome[1]), as.character(names[n]), vals)
    commtraits[counter,] = vec
    counter = counter+1
  }
}

commtraits$biome = as.numeric(commtraits$biome)
commtraits$mass = as.numeric(commtraits$mass)
commtraits$billwidth = as.numeric(commtraits$billwidth)
commtraits$billlength = as.numeric(commtraits$billlength)
commtraits$wingchord = as.numeric(commtraits$wingchord)
commtraits$wingarea = as.numeric(commtraits$wingarea)
commtraits$wingload = as.numeric(commtraits$wingload)
commtraits$taillength = as.numeric(commtraits$taillength)
commtraits$tarsuslength = as.numeric(commtraits$tarsuslength)

ct = aggregate(. ~ comm, data = commtraits, mean)


#------------------------------------------
#    Plot community comparison data
#------------------------------------------

mass_by_communities <- ggplot(commtraits, aes(x=comm, y = mass)) + geom_boxplot() + 
  theme_bw() + theme(axis.text.x=element_text(angle=60, vjust=0.5))

cols2plot=c("mass", "billwidth", "billlength", "wingchord", "wingarea", 
            "wingload", "taillength", "tarsuslength")
for (i in seq_along(cols2plot)){
  print(ggplot(commtraits, aes_string(x="comm", y = cols2plot[i])) + geom_boxplot(aes(fill="gray20")) + 
   theme_bw() + theme(text = element_text(size =20), axis.text.x=element_text(angle=60, vjust=0.5)))
}

theme(text = element_text(size=20),
      axis.text.x = element_text(angle=90, vjust=1)) 

ggplot(bogosites, aes(LongDecDeg, LatDecDeg)) + 
  geom_point(aes(col=Biome, size = Richness)) + theme_bw() +
  scale_colour_gradient(low = "blue", high = "indianred")

cols2plot=c("ct$mass", "ct$billwidth", "ct$billlength", "ct$wingchord", "ct$wingarea", 
            "ct$wingload", "ct$taillength", "ct$tarsuslength")
for (i in seq_along(cols2plot)){
  print(ggplot(bogosites,aes(LongDecDeg, LatDecDeg)) + 
    geom_point(aes_string(col=cols2plot[i], size="Richness")) + theme_bw() +
  scale_colour_gradient(low="lightgreen", high = "hotpink"))
}

ggplot(commtraits, aes(mass, wingchord)) + geom_point(aes(col = comm, size = 3)) + 
  theme_bw() + stat_smooth(method = "lm", aes(group = comm, col = comm),alpha = 0.1)
