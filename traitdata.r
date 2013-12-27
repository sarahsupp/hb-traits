# code to clean and organize the hummingbird trait datasets to be used in the project
# note that this dataset only includes information for male hummingbirds

#library(lattice)
library(GGally)
library(ggplot2)
library(ggmap)
library(vegan)
#library(data.table)
library(reshape2)
library(grid)
library(devtools)
library(ggbiplot)
library(sp)
library(raster)
library(dismo)
library(maptools)


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
  sitexspp = sitexspp[-c(1:3), ] #don't need the elevation data

#find and subset only Gary Stiles' data (the most trusted data)
Stiles = subset(refs, Title == "Gary Stiles Personal database")
GS_ID = Stiles$RefID
traits = subset(traits, ReferenceID == GS_ID)
  traits[traits == 9999] <- NA
  traits = traits[,c(1:21)]

# aggregate for species, remove last column
# traits <- aggregate(traits, list(traits$SpID), mean, na.rm = TRUE)
# mon <- agg.morph[, -23]

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
ggmap(colombia) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), size = 3, data = colgeo)

colombiasites = ggmap(colombia) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), data = colsites, cex = 4)

#Just sites near Bogota
bogosites = colsites[which(colsites$LongDecDeg < -73.5  & colsites$LongDecDeg > -75.25 & 
                             colsites$LatDecDeg > 3 & colsites$LatDecDeg < 5.5),]

bogota = get_map(location = "Bogota", zoom = 8, maptype = "terrain", color = "bw")

sitemap = ggmap(bogota) + geom_point(aes(x = LongDecDeg, y = LatDecDeg), size = 5, 
                                     data = bogosites) + element_blank() + scale_fill_brewer(palette=1)

siterichness = ggplot(bogosites, aes(LongDecDeg, LatDecDeg)) + geom_point(aes(size = Richness)) + theme_bw()


#------------------------------------------
#         Extract elevation for the sites
#------------------------------------------
# make a SpatialPointsDataFrame
bogosp<-SpatialPointsDataFrame(cbind(bogosites$LongDecDeg,bogosites$LatDecDeg),bogosites)

#get elevation raster for Colombia from WorldClim data
r_elev = raster("COL_msk_alt.grd")

# clip elevation raster to extent of study
exte = c(-75.25,-73,3.5,5.5)
elev_sub <- crop(r_elev, exte)
plot(elev_sub)
  points(bogosp, pch=19)

site_elev = extract(elev_sub, bogosp)

bogosites = cbind(bogosites,site_elev)

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
commtraits = data.frame(comm="name", elev=0, species="name", clade = "name", 
                        mass=0, billwidth=0, expbilllength=0, totbilllength=0, billdepth=0,
                        wingchord=0, wingwidth=0, winglength=0, wingaspectratio=0, wingform=0, wingarea=0, wingload=0, wingtaper=0,
                        taillength=0, tarsuslength=0, footextend=0, naillength=0)
  levels(commtraits$comm) = unique(sitexspp$X)
  levels(commtraits$species) = names(sitexspp[,2:134])
  levels(commtraits$clade) = unique(species$Clade)
counter = 1

for (row in 1:nrow(sitexspp)){
  sitename = sitexspp[row,1]
    elev = bogosites[which(bogosites$Community == sitename),9]
  dat = sitexspp[row,c(1:134)]
    dat = cbind(dat, elev)
  dat2=melt(dat, id = c("X", "elev"))
  dat3 = dat2[which(dat2$value == 1),]
    dat3$clade = NA
    names(dat3) = c("comm", "elev", "species", "presence", "clade")
  names = dat3$species
  for (n in 1:length(names)){
    spclade = species[which(species$spname_dot == names[n]),2]
    traitdat = traits[which(traits$spname == names[n]),c(6,7,5,8,10,9,11,12,13,14,17,15,16,18,20,19,21)] #was only using these 8 traits: c(6,7,8,9,17,15,18,20)
    vals = c()
    for (t in 1:ncol(traitdat)){
      vals = append(vals, mean(traitdat[,t]))  #if a species appears more than once in traits, take the mean value
    }
    vec = c(as.character(dat3$comm[1]), as.character(dat3$elev[1]), as.character(names[n]), 
            as.character(spclade), vals)
    commtraits[counter,] = vec
    counter = counter+1
  }
}

#change measurements columns to numeric
id <- c(2,5:ncol(commtraits)) 
commtraits[,id] <- as.numeric(as.character(unlist(commtraits[,id])))

ct = aggregate(. ~ comm, data = commtraits[,c(1,5:ncol(commtraits))], mean)
ct2 = aggregate(. ~ species, data = commtraits[,c(3,5:ncol(commtraits))], mean)


#---------------------------------------------
#     Make PCA biplots of the traits by species
#---------------------------------------------

rownames(ct2) = ct2$species
ct2=ct2[,-1]

# Standard the matrix to correct for different units by subtracting the
# means and dividing by sd
zscore <- apply(ct2, 2, function(x) {
  y <- (x - mean(x))/sd(x)
  return(y)
})
rownames(zscore) <- rownames(ct2)

# Take only reasonably uncorrelated traits
trait_keep <- c("mass", "totbillength", "wingchord", "wingload", "tarsuslength", 
                "taillength", "naillength")

zscore_sub <- zscore[, colnames(zscore) %in% trait_keep]

trait_pc<-prcomp(ct2)

#Plot PCA - a bit ugly default
biplot(trait_pc,cex=.75)

#Use dev libary to ggplot PCA, color by clades
#Try the ggplot biplot to color by clades (or later, behavioral roles)
toCol<-species[species$spname_dot %in% rownames(trait_pc$x),"Clade"]

#Label species names and clades, circles cover normal distribuiton of groups
ggbiplot(trait_pc, groups=toCol, labels=rownames(trait_pc$x), ellipse=TRUE)

#TODO: ADD info that would allow grouping of species by behavioral strategy
#toCol<-species[species$spname_dot %in% rownames(trait_pc$x),"Role"]
#ggbiplot(trait_pc, groups=toCol, labels=rownames(trait_pc$x), ellipse=TRUE)


#---------------------------------------------
#     Make PCA biplots of the traits by community
#---------------------------------------------

rownames(ct) = ct$comm
ct=ct[,-1]

# Standard the matrix to correct for different units by subtracting the
# means and dividing by sd
zscore <- apply(ct, 2, function(x) {
  y <- (x - mean(x))/sd(x)
  return(y)
})
rownames(zscore) <- rownames(ct)

# Take only reasonably uncorrelated traits
trait_keep <- c("mass", "totbilllength", "wingchord", "wingload", "tarsuslength", 
                "taillength", "naillength")

zscore_sub <- zscore[, colnames(zscore) %in% trait_keep]

trait_pc<-prcomp(ct)

#Plot PCA - a bit ugly default
biplot(trait_pc,cex=.75)

#Use dev libary to ggplot PCA, color by Elevation
#Try the ggplot biplot to color by Elevation (or later, behavioral roles)
toCol<-bogosites[bogosites$CommunityName %in% rownames(trait_pc$x),"site_elev"]  
  toCol[toCol < 2000] <- 0
  toCol[toCol > 2000] <- 1

#Label species names and clades, circles cover normal distribuiton of groups
ggbiplot(trait_pc, groups=as.factor(toCol), labels=rownames(trait_pc$x), ellipse=TRUE)


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


ggplot(bogosites, aes(LongDecDeg, LatDecDeg)) + 
  geom_point(aes(col=elev, size = Richness)) + theme_bw() +
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

#how correlated are the traits?
pairs(commtraits[,c(4:11)], pch = 19, cex.labels=2)

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray80", ...)
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

commtraits$biome = as.factor(commtraits$biome)
pairs(commtraits[,c(4:11)],cex = 1.5, pch = 19, bg = "gray20",
      diag.panel = panel.hist, cex.labels = 1.5, font.labels = 2)
 #     lower.panel = panel.smooth, upper.panel = panel.cor, 
 #     bg = rainbow(8)[unclass(commtraits$biome)])

#plot traits pairs, colored by biome ()
alltraits = commtraits[complete.cases(commtraits),c(2,4,6,7,9,10,11)]
ggpairs(alltraits, colour = "biome", alpha = 0.5)
