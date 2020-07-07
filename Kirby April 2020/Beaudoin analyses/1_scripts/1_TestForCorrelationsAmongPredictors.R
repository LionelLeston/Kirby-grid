library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(forcats)

#get coefficients of variation
CV<-function(x){
  (sd(x)/mean(x))*100
}
# Get lower triangle of the correlation matrix
get_lower_tri<-function(x){
  x[upper.tri(x)] <- NA
  return(x)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(x){
  x[lower.tri(x)]<- NA
  return(x)
}


my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

my.theme2 <- theme_classic() +
  theme(text=element_text(size=24, family="Arial"),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))


#Start with all station visits, create a summary statistic for each station
birds<-read.csv(file="0_data/raw/allvisitsBG15.csv", header=TRUE)      
str(birds)

kirby.summ<-birds%>%
  group_by(SS)%>%
  summarise_at(vars(ALFL:YRWA), funs(mean, max))
kirby.summ<-data.frame(kirby.summ)
nrow(kirby.summ)#84
write.csv(kirby.summ, file="0_data/processed/kirby.summ.csv")#data check

kirby.summ<-read.csv("0_data/processed/kirby.summ.csv", header=TRUE)
bg15coord<-read.csv("0_data/raw/Kirby.coord.csv", header=TRUE)
bg15coord<-bg15coord[,c("SS","EASTING","NORTHING")]
str(bg15coord)
birdsXY<-merge(bg15coord, kirby.summ, by=c("SS"))
write.csv(birdsXY, file="0_data/processed/birdsXY.csv")#data check

birdsXY<-read.csv("0_data/processed/birdsXY.csv", header=TRUE)
habitat<-read.csv("0_data/processed/Kirby.Beaudoin.3spatscale.csv", header=TRUE)
#combine bird and Beaudoin layer habitat data

birdsXY.hab<-merge(birdsXY, habitat, by=c("SS","EASTING","NORTHING"))
write.csv(birdsXY.hab, file="0_data/processed/birdsXY.hab.csv")#data check

birdsXY.hab<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)

#Graph mean values of site variables,
#coefficients of variation, and
#correlation coefficients among predictorx

#note: variables estimated to be zero or zero CV at all points have
#been highlighted out to reduce matrix size
mydata50<-birdsXY.hab[,c("LandCover_NonVeg_v1_50",
                       "LandCover_Veg_v1_50",
                       "LandCover_VegNonTreed_v1_50",
                       "LandCover_VegTreed_v1_50",
                       "Species_Abie_Bal_v1_50",
                       #"Species_Abie_Las_v1_50",
                       #"Species_Acer_Neg_v1_50",
                       #"Species_Alnu_Spp_v1_50",
                       "Species_Betu_Pap_v1_50",
                       #"Species_Frax_Pen_v1_50",
                       "Species_Lari_Lar_v1_50",
                       #"Species_Lari_Lya_v1_50",
                       #"Species_Pice_Eng_v1_50",
                       "Species_Pice_Gla_v1_50",
                       "Species_Pice_Mar_v1_50",
                       #"Species_Pinu_Alb_v1_50",
                       "Species_Pinu_Ban_v1_50",
                       "Species_Pinu_Con_v1_50",
                       #"Species_Pinu_Pon_v1_50",
                       #"Species_Pinu_Syl_v1_50",
                       "Species_Popu_Bal_v1_50",
                       "Species_Popu_Tre_v1_50",
                       #"Species_Popu_Tri_v1_50",
                       #"Species_Prun_Pen_v1_50",
                       #"Species_Pseu_Men_v1_50",
                       "Species_Sali_Spp_v1_50",
                       "SpeciesGroups_Broadleaf_Spp_v1_50",
                       "SpeciesGroups_Needleleaf_Spp_v1_50",
                       "SpeciesGroups_Unknown_Spp_v1_50",
                       "Structure_Biomass_Branch_v1_50",
                       "Structure_Biomass_Foliage_v1_50",
                       "Structure_Biomass_StemBark_v1_50",
                       "Structure_Biomass_StemWood_v1_50",
                       "Structure_Biomass_TotalDead_v1_50",
                       "Structure_Biomass_TotalLiveAboveGround_v1_50",
                       "Structure_Stand_Age_v1_50",
                       "Structure_Stand_CrownClosure_v1_50",
                       "Structure_Stand_Height_v1_50",
                       "Structure_Volume_Merch_v1_50",
                       "Structure_Volume_Total_v1_50")]#just the Beaudoin variables
str(mydata50)

mydata50.cv<-mydata50%>%
  summarise_at(vars(LandCover_NonVeg_v1_50:Structure_Volume_Total_v1_50), funs(CV))

mydata50.cv2 <- gather(mydata50.cv,
                   key = "predictor",
                   value = "cv")
mydata50.cv2$goodcv<-ifelse(mydata50.cv2$cv>100,1,0)

tiff("3_outputs/figures/CoefVarPredictors50m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata50.cv2, aes(x=fct_reorder(predictor, cv), y=cv, fill=as.factor(goodcv))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Coefficient of Variation")+
  xlab("")+
  coord_flip()
dev.off()

#Get Mean Amount of Cover
mydata50.mean<-mydata50%>%
  summarise_at(vars(LandCover_NonVeg_v1_50:Structure_Volume_Total_v1_50), funs(mean))

mydata50.mean2 <- gather(mydata50.mean,
                       key = "predictor",
                       value = "mean")
mydata50.mean2$goodmean<-ifelse(mydata50.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count

tiff("3_outputs/figures/PercCovPredictors50m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata50.mean2, aes(x=fct_reorder(predictor, mean), y=mean, fill=as.factor(goodmean))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Percent Cover")+
  xlab("")+
  coord_flip()
dev.off()

#Get Correlation Heat Plot
cormat50 <- round(cor(mydata50),2)
head(cormat50)

# Melt the correlation matrix
upper_tri50 <- get_upper_tri(cormat50)
upper_tri50

melted_cormat50 <- melt(upper_tri50, na.rm = TRUE)
# Heatmap
tiff("3_outputs/figures/CorrelationCoefHeatMap50m.tiff", units="in", width=24, height=24, res=300)
ggplot(data = melted_cormat50, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  my.theme+
  #theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 16, hjust = 1))+
  coord_fixed()
dev.off()

mydata150<-birdsXY.hab[,c("LandCover_NonVeg_v1_150",
                         "LandCover_Veg_v1_150",
                         "LandCover_VegNonTreed_v1_150",
                         "LandCover_VegTreed_v1_150",
                         "Species_Abie_Bal_v1_150",
                         #"Species_Abie_Las_v1_150",
                         #"Species_Acer_Neg_v1_150",
                         #"Species_Alnu_Spp_v1_150",
                         "Species_Betu_Pap_v1_150",
                         #"Species_Frax_Pen_v1_150",
                         "Species_Lari_Lar_v1_150",
                         #"Species_Lari_Lya_v1_150",
                         #"Species_Pice_Eng_v1_150",
                         "Species_Pice_Gla_v1_150",
                         "Species_Pice_Mar_v1_150",
                         #"Species_Pinu_Alb_v1_150",
                         "Species_Pinu_Ban_v1_150",
                         "Species_Pinu_Con_v1_150",
                         #"Species_Pinu_Pon_v1_150",
                         #"Species_Pinu_Syl_v1_150",
                         "Species_Popu_Bal_v1_150",
                         "Species_Popu_Tre_v1_150",
                         #"Species_Popu_Tri_v1_150",
                         #"Species_Prun_Pen_v1_150",
                         #"Species_Pseu_Men_v1_150",
                         "Species_Sali_Spp_v1_150",
                         "SpeciesGroups_Broadleaf_Spp_v1_150",
                         "SpeciesGroups_Needleleaf_Spp_v1_150",
                         "SpeciesGroups_Unknown_Spp_v1_150",
                         "Structure_Biomass_Branch_v1_150",
                         "Structure_Biomass_Foliage_v1_150",
                         "Structure_Biomass_StemBark_v1_150",
                         "Structure_Biomass_StemWood_v1_150",
                         "Structure_Biomass_TotalDead_v1_150",
                         "Structure_Biomass_TotalLiveAboveGround_v1_150",
                         "Structure_Stand_Age_v1_150",
                         "Structure_Stand_CrownClosure_v1_150",
                         "Structure_Stand_Height_v1_150",
                         "Structure_Volume_Merch_v1_150",
                         "Structure_Volume_Total_v1_150")]#just the Beaudoin variables
str(mydata150)

#get coefficients of variation
mydata150.cv<-mydata150%>%
  summarise_at(vars(LandCover_NonVeg_v1_150:Structure_Volume_Total_v1_150), funs(CV))

mydata150.cv2 <- gather(mydata150.cv,
                       key = "predictor",
                       value = "cv")
mydata150.cv2$goodcv<-ifelse(mydata150.cv2$cv>100,1,0)

tiff("3_outputs/figures/CoefVarPredictors150m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata150.cv2, aes(x=fct_reorder(predictor, cv), y=cv, fill=as.factor(goodcv))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Coefficient of Variation")+
  xlab("")+
  coord_flip()
dev.off()

#Get Mean Amount of Cover
mydata150.mean<-mydata150%>%
  summarise_at(vars(LandCover_NonVeg_v1_150:Structure_Volume_Total_v1_150), funs(mean))

mydata150.mean2 <- gather(mydata150.mean,
                         key = "predictor",
                         value = "mean")
mydata150.mean2$goodmean<-ifelse(mydata150.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count

tiff("3_outputs/figures/PercCovPredictors150m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata150.mean2, aes(x=fct_reorder(predictor, mean), y=mean, fill=as.factor(goodmean))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Percent Cover")+
  xlab("")+
  coord_flip()
dev.off()

#Correlation Heat Plot
cormat150 <- round(cor(mydata150),2)
head(cormat150)

# Melt the correlation matrix
upper_tri150 <- get_upper_tri(cormat150)
upper_tri150

melted_cormat150 <- melt(upper_tri150, na.rm = TRUE)
# Heatmap
tiff("3_outputs/figures/CorrelationCoefHeatMap150m.tiff", units="in", width=24, height=24, res=300)
ggplot(data = melted_cormat150, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  my.theme+
  #theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 16, hjust = 1))+
  coord_fixed()
dev.off()


mydata500<-birdsXY.hab[,c("LandCover_NonVeg_v1_500",
                          "LandCover_Veg_v1_500",
                          "LandCover_VegNonTreed_v1_500",
                          "LandCover_VegTreed_v1_500",
                          "Species_Abie_Bal_v1_500",
                          #"Species_Abie_Las_v1_500",
                          #"Species_Acer_Neg_v1_500",
                          #"Species_Alnu_Spp_v1_500",
                          "Species_Betu_Pap_v1_500",
                          #"Species_Frax_Pen_v1_500",
                          "Species_Lari_Lar_v1_500",
                          #"Species_Lari_Lya_v1_500",
                          #"Species_Pice_Eng_v1_500",
                          "Species_Pice_Gla_v1_500",
                          "Species_Pice_Mar_v1_500",
                          #"Species_Pinu_Alb_v1_500",
                          "Species_Pinu_Ban_v1_500",
                          "Species_Pinu_Con_v1_500",
                          #"Species_Pinu_Pon_v1_500",
                          #"Species_Pinu_Syl_v1_500",
                          "Species_Popu_Bal_v1_500",
                          "Species_Popu_Tre_v1_500",
                          #"Species_Popu_Tri_v1_500",
                          #"Species_Prun_Pen_v1_500",
                          #"Species_Pseu_Men_v1_500",
                          "Species_Sali_Spp_v1_500",
                          "SpeciesGroups_Broadleaf_Spp_v1_500",
                          "SpeciesGroups_Needleleaf_Spp_v1_500",
                          "SpeciesGroups_Unknown_Spp_v1_500",
                          "Structure_Biomass_Branch_v1_500",
                          "Structure_Biomass_Foliage_v1_500",
                          "Structure_Biomass_StemBark_v1_500",
                          "Structure_Biomass_StemWood_v1_500",
                          "Structure_Biomass_TotalDead_v1_500",
                          "Structure_Biomass_TotalLiveAboveGround_v1_500",
                          "Structure_Stand_Age_v1_500",
                          "Structure_Stand_CrownClosure_v1_500",
                          "Structure_Stand_Height_v1_500",
                          "Structure_Volume_Merch_v1_500",
                          "Structure_Volume_Total_v1_500")]#just the Beaudoin variables
str(mydata500)

#get coefficients of variation
mydata500.cv<-mydata500%>%
  summarise_at(vars(LandCover_NonVeg_v1_500:Structure_Volume_Total_v1_500), funs(CV))

mydata500.cv2 <- gather(mydata500.cv,
                        key = "predictor",
                        value = "cv")
mydata500.cv2$goodcv<-ifelse(mydata500.cv2$cv>100,1,0)

tiff("3_outputs/figures/CoefVarPredictors500m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata500.cv2, aes(x=fct_reorder(predictor, cv), y=cv, fill=as.factor(goodcv))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Coefficient of Variation")+
  xlab("")+
  coord_flip()
dev.off()

#Get Mean Amount of Cover
mydata500.mean<-mydata500%>%
  summarise_at(vars(LandCover_NonVeg_v1_500:Structure_Volume_Total_v1_500), funs(mean))

mydata500.mean2 <- gather(mydata500.mean,
                          key = "predictor",
                          value = "mean")
mydata500.mean2$goodmean<-ifelse(mydata500.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count

tiff("3_outputs/figures/PercCovPredictors500m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata500.mean2, aes(x=fct_reorder(predictor, mean), y=mean, fill=as.factor(goodmean))) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Percent Cover")+
  xlab("")+
  coord_flip()
dev.off()

#Correlation Heat Plot
cormat500 <- round(cor(mydata500),2)
head(cormat500)

# Melt the correlation matrix
upper_tri500 <- get_upper_tri(cormat500)
upper_tri500

melted_cormat500 <- melt(upper_tri500, na.rm = TRUE)
# Heatmap
tiff("3_outputs/figures/CorrelationCoefHeatMap500m.tiff", units="in", width=24, height=24, res=300)
ggplot(data = melted_cormat500, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  my.theme+
  #theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 16, hjust = 1))+
  coord_fixed()
dev.off()

#Based on the outputs, the following variables have sufficient
#coefficients of variation (>1 or 100%) for analysis at all 3
#spatial scales: Balsam Fir, Willow, Lodgepole Pine, Birch,
#Nonvegetated, Larch, Balsam Poplar

#However, the average % cover of these variables within 50 m
#of the point count stations is small (<25%)

#In contrast, variables with low CV like Live Aboveground Biomass,
#Crown Closure, Black Spruce, Total Volume, Stand Age, Treed Cover, 
#Vegetated Cover, and Total Conifer (Needleleaf Spp Cover) occupy 30
#% or more of land within 50 m of points

#Of these latter variables