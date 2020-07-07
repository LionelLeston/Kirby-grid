options (java.parameters = "-Xmx1024m" )
#library(XLConnect)

#1. Import 50-m vegetation data from AVI
AVIforestveg_50 <- read.csv("0_data/raw/KirbyBuff50_AVI.csv", header=TRUE)
AVIforestveg_150 <- read.csv("0_data/raw/KirbyBuff150_AVI.csv", header=TRUE)
AVIforestveg_500 <- read.csv("0_data/raw/KirbyBuff500_AVI.csv", header=TRUE)
#pulls data into R  
str(AVIforestveg_50)
str(AVIforestveg_150)
str(AVIforestveg_500)


#2. calculate total area of each habitat type within 50 m of each station
AVIforestveg_50$SS<-AVIforestveg_50$SiteName
AVIforestveg_50$ForestAge<-ifelse(!is.na(AVIforestveg_50$Origin_Year),(2019-AVIforestveg_50$Origin_Year),"NA")
AVIforestveg_50$ForestAge<-as.numeric(AVIforestveg_50$ForestAge)
library(dplyr)
SSforestage<-AVIforestveg_50%>%
  group_by(SS)%>%
  summarize(wtage=mean(ForestAge, na.rm=TRUE))

veg50<-aggregate(AVIforestveg_50$Shape_Area, list(AVIforestveg_50$SS, AVIforestveg_50$Veg_Type), sum)
str(veg50)

#White spruce Picea glauca Sw Sw
#Engelmann spruce Picea engelmannii Se Sw
#Black spruce Picea mariana Sb Sb
#Lodgepole pine Pinus contorta Pl P
#Jack pine Pinus banksiana Pj P
#White-bark pine Pinus albicaulis Pa P
#Limber pine Pinus flexilis Pf P
#Balsam fir Abies balsamea Fb Fb
#Alpine fir Abies lasiocarpa Fa Fb
#Douglas fir Pseudotsuga menziesii Fd Fd
#Alpine larch Larix lyallii La Lt
#Tamarack Larix laricina Lt Lt
#Western larch Larix occidentalis Lw Lt
#Trembling aspen Populus tremuloides Aw A2
#Balsam poplar Populus balsamifera Pb A
#Paper (white) birch Betula papyrifera Bw Bw
 
library(reshape)
veg50.recast<-cast(veg50, Group.1~Group.2, sum)
#veg50.recast
str(veg50.recast)
veg50.recast$SS<-veg50.recast$Group.1
veg50.recast$Group.1<-NULL

forestage<-merge(veg50.recast, SSforestage, by=c("SS"))

write.csv(forestage, file = "0_data/processed/veg50.recast.csv")

#150-m scale
AVIforestveg_150$SS<-AVIforestveg_150$SiteName
AVIforestveg_150$ForestAge<-ifelse(!is.na(AVIforestveg_150$Origin_Year),(2019-AVIforestveg_150$Origin_Year),"NA")
AVIforestveg_150$ForestAge<-as.numeric(AVIforestveg_150$ForestAge)

SSforestage<-AVIforestveg_150%>%
  group_by(SS)%>%
  summarize(wtage=mean(ForestAge, na.rm=TRUE))


veg150<-aggregate(AVIforestveg_150$Shape_Area, list(AVIforestveg_150$SS, AVIforestveg_150$Veg_Type), sum)
str(veg150)

#White spruce Picea glauca Sw Sw
#Engelmann spruce Picea engelmannii Se Sw
#Black spruce Picea mariana Sb Sb
#Lodgepole pine Pinus contorta Pl P
#Jack pine Pinus banksiana Pj P
#White-bark pine Pinus albicaulis Pa P
#Limber pine Pinus flexilis Pf P
#Balsam fir Abies balsamea Fb Fb
#Alpine fir Abies lasiocarpa Fa Fb
#Douglas fir Pseudotsuga menziesii Fd Fd
#Alpine larch Larix lyallii La Lt
#Tamarack Larix laricina Lt Lt
#Western larch Larix occidentalis Lw Lt
#Trembling aspen Populus tremuloides Aw A2
#Balsam poplar Populus balsamifera Pb A
#Paper (white) birch Betula papyrifera Bw Bw

library(reshape)
veg150.recast<-cast(veg150, Group.1~Group.2, sum)
#veg150.recast
str(veg150.recast)
veg150.recast$SS<-veg150.recast$Group.1
veg150.recast$Group.1<-NULL

forestage<-merge(veg150.recast, SSforestage, by=c("SS"))

write.csv(forestage, file = "0_data/processed/veg150.recast.csv")

#500-m scale
AVIforestveg_500$SS<-AVIforestveg_500$SiteName
AVIforestveg_500$ForestAge<-ifelse(!is.na(AVIforestveg_500$Origin_Year),(2019-AVIforestveg_500$Origin_Year),"NA")
AVIforestveg_500$ForestAge<-as.numeric(AVIforestveg_500$ForestAge)

SSforestage<-AVIforestveg_500%>%
  group_by(SS)%>%
  summarize(wtage=mean(ForestAge, na.rm=TRUE))

veg500<-aggregate(AVIforestveg_500$Shape_Area, list(AVIforestveg_500$SS, AVIforestveg_500$Veg_Type), sum)
str(veg500)

#White spruce Picea glauca Sw Sw
#Engelmann spruce Picea engelmannii Se Sw
#Black spruce Picea mariana Sb Sb
#Lodgepole pine Pinus contorta Pl P
#Jack pine Pinus banksiana Pj P
#White-bark pine Pinus albicaulis Pa P
#Limber pine Pinus flexilis Pf P
#Balsam fir Abies balsamea Fb Fb
#Alpine fir Abies lasiocarpa Fa Fb
#Douglas fir Pseudotsuga menziesii Fd Fd
#Alpine larch Larix lyallii La Lt
#Tamarack Larix laricina Lt Lt
#Western larch Larix occidentalis Lw Lt
#Trembling aspen Populus tremuloides Aw A2
#Balsam poplar Populus balsamifera Pb A
#Paper (white) birch Betula papyrifera Bw Bw

library(reshape)
veg500.recast<-cast(veg500, Group.1~Group.2, sum)
#veg500.recast
str(veg500.recast)
veg500.recast$SS<-veg500.recast$Group.1
veg500.recast$Group.1<-NULL

forestage<-merge(veg500.recast, SSforestage, by=c("SS"))

write.csv(forestage, file = "0_data/processed/veg500.recast.csv")

