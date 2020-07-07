library(raster)
library(dismo)
library(rpart)
library(maptools)
library(data.table)
library(rgdal)
library(dplyr)
# Preparing data

# Load AB region raster, project and extract data for AB, 
#LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# coordinates(SS) <- c("X", "Y") 
# proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# SSLCC <- as.data.frame(spTransform(SS, LCC))
# ABSS <- SSLCC[SSLCC$JURS=="AB",]
# rm(SSLCC)
# gc()

# offl <- data.table(melt(OFF))
# names(offl) <- c("PKEY","SPECIES","logoffset")
# offl$SPECIES <- as.character(offl$SPECIES)
# offl$PKEY <-as.character(offl$PKEY)
# rm(OFF)
# gc()

# eco <- raster("E:/MITACS ACCELERATE FELLOWSHIP/Incidental Take Risk Ranking Matrix for Westfraser/FDenes BRT example/CHID-regional-Alberta-BRT-master/albertaeco1.tif") 
# eco<-projectRaster(eco,crs=LCC)
# plot(eco)
#alberta <- raster("0_data/raw/AlbertaLCC.tif")
#alberta<-projectRaster(alberta,crs=LCC)
#plot(alberta)

# Load Beaudoin layers (2011 only), crop and mask for AB, save as rasters
# b2011 <- list.files("D:/Beaudoin/2011/Processed/sppBiomass_Canada_t_per_ha",pattern="tif$")
# setwd("D:/Beaudoin/2011/Processed/sppBiomass_Canada_t_per_ha")
# bs2011 <- stack(raster(b2011[1]))
# for (i in 2:length(b2011)) { bs2011 <- addLayer(bs2011, raster(b2011[i]))}
# names(bs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(bs2011))
# abs2011 <- crop(bs2011,alberta)
# abs2011 <- mask(abs2011,alberta)
# writeRaster(abs2011, filename="D:/Beaudoin/2011/Processed/AB/abs2011_250m.grd", format="raster",overwrite=TRUE)

abs2011<-stack("0_data/raw/abs2011_250m.grd")
#Note: this "raw" data consists of Beaudoin layers that have 
#already been clipped from national-scale to Alberta and been 
#stacked. There are 40 variables so there are 40 raster layers
#in the stack.
names(abs2011)

#Read in bird data
birdcounts<-read.csv("0_data/raw/allvisitsBG15.csv", header=TRUE)
str(birdcounts)#There are 98 point count stations in the Kirby grid 
#with bird data. So Beaudoin raster data should be extracted to
#these 98 points.

#Extract raster data to points at 50-m, 150-m, and 500-m scales of analysis
kirby<-read.csv("0_data/raw/Kirby.coord.csv", header=TRUE)
str(kirby)

coordinates(kirby) <- c("EASTING", "NORTHING") 
LCC <- CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs")
proj4string(kirby) <- LCC

abs2011B<-projectRaster(abs2011,crs=LCC)
#reproject raster into same LCC as Kirby points
writeRaster(abs2011B, filename="0_data/processed/abs2011_250mB.grd", format="raster",overwrite=TRUE)

plot(abs2011B[["Structure_Biomass_TotalLiveAboveGround_v1"]])
plot(kirby, add=TRUE, pch=20)
#to test if the rasters and points are in the same projection

for(i in names(abs2011B)) { 
  kirby<-cbind(kirby, extract(abs2011B[[i]], coordinates(kirby), buffer=50, small=TRUE, fun=mean))
  names(kirby)[ncol(kirby)] <- paste0(names(abs2011B[[i]]), "_50")
}#

for(i in names(abs2011B)) { 
  kirby<-cbind(kirby, extract(abs2011B[[i]], coordinates(kirby), buffer=150, small=TRUE, fun=mean))
  names(kirby)[ncol(kirby)] <- paste0(names(abs2011B[[i]]), "_150")
}#

for(i in names(abs2011B)) { 
  kirby<-cbind(kirby, extract(abs2011B[[i]], coordinates(kirby), buffer=500, small=TRUE, fun=mean))
  names(kirby)[ncol(kirby)] <- paste0(names(abs2011B[[i]]), "_500")
}#

names(kirby)
write.csv(kirby, file="0_data/processed/Kirby.Beaudoin.3spatscale.csv")

