library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(forcats)

#get coefficients of variation
CV<-function(x){
  (sd(x)/abs(mean(x)))*100
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
habitat50<-read.csv("0_data/raw/site_lidar_cov_50mNAZEROED.csv", header=TRUE)
str(habitat50)
habitat50B<-habitat50%>%
  rename(mean_MaxHeight_50=mean_MaxHeight,
         mean_MeanHeight_50=mean_MeanHeight,
         mean_PercCanopyCoverCut0.1m_50=mean_PercCanopyCoverCut0.1m,     
         mean_PercCanopyCoverCut0.5m_50=mean_PercCanopyCoverCut0.5m,
         mean_PercCanopyCoverCut1.0m_50=mean_PercCanopyCoverCut1.0m,
         mean_PercCanopyCoverCut1.5m_50=mean_PercCanopyCoverCut1.5m,
         mean_PercCanopyDensity_50=mean_PercCanopyDensity,
         sd_MaxHeight_50=sd_MaxHeight,
         sd_MeanHeight_50=sd_MeanHeight,
         sd_PercCanopyCoverCut0.1m_50=sd_PercCanopyCoverCut0.1m,     
         sd_PercCanopyCoverCut0.5m_50=sd_PercCanopyCoverCut0.5m,
         sd_PercCanopyCoverCut1.0m_50=sd_PercCanopyCoverCut1.0m,
         sd_PercCanopyCoverCut1.5m_50=sd_PercCanopyCoverCut1.5m,
         sd_PercCanopyDensity_50=sd_PercCanopyDensity,
         cv_MaxHeight_50=cv_MaxHeight,
         cv_MeanHeight_50=cv_MeanHeight,
         cv_PercCanopyCoverCut0.1m_50=cv_PercCanopyCoverCut0.1m,     
         cv_PercCanopyCoverCut0.5m_50=cv_PercCanopyCoverCut0.5m,
         cv_PercCanopyCoverCut1.0m_50=cv_PercCanopyCoverCut1.0m,
         cv_PercCanopyCoverCut1.5m_50=cv_PercCanopyCoverCut1.5m,
         cv_PercCanopyDensity_50=cv_PercCanopyDensity,
         kurtosis_MaxHeight_50=kurtosis_MaxHeight,
         kurtosis_MeanHeight_50=kurtosis_MeanHeight,
         kurtosis_PercCanopyCoverCut0.1m_50=kurtosis_PercCanopyCoverCut0.1m,     
         kurtosis_PercCanopyCoverCut0.5m_50=kurtosis_PercCanopyCoverCut0.5m,
         kurtosis_PercCanopyCoverCut1.0m_50=kurtosis_PercCanopyCoverCut1.0m,
         kurtosis_PercCanopyCoverCut1.5m_50=kurtosis_PercCanopyCoverCut1.5m,
         kurtosis_PercCanopyDensity_50=kurtosis_PercCanopyDensity,
         skewness_MaxHeight_50=skewness_MaxHeight,
         skewness_MeanHeight_50=skewness_MeanHeight,
         skewness_PercCanopyCoverCut0.1m_50=skewness_PercCanopyCoverCut0.1m,     
         skewness_PercCanopyCoverCut0.5m_50=skewness_PercCanopyCoverCut0.5m,
         skewness_PercCanopyCoverCut1.0m_50=skewness_PercCanopyCoverCut1.0m,
         skewness_PercCanopyCoverCut1.5m_50=skewness_PercCanopyCoverCut1.5m,
         skewness_PercCanopyDensity_50=skewness_PercCanopyDensity,
         strata_00_to_02_returnProportion_50=strata_00_to_02_returnProportion,
         strata_02_to_04_returnProportion_50=strata_02_to_04_returnProportion,
         strata_04_to_06_returnProportion_50=strata_04_to_06_returnProportion,
         strata_06_to_08_returnProportion_50=strata_06_to_08_returnProportion,
         strata_08_to_10_returnProportion_50=strata_08_to_10_returnProportion,
         strata_10_to_15_returnProportion_50=strata_10_to_15_returnProportion,
         strata_15_to_20_returnProportion_50=strata_15_to_20_returnProportion,
         strata_20_above_returnProportion_50=strata_20_above_returnProportion)

habitat150<-read.csv("0_data/raw/site_lidar_cov_150m.csv", header=TRUE)
str(habitat150)
habitat150B<-habitat150%>%
  rename(mean_MaxHeight_150=mean_MaxHeight,
         mean_MeanHeight_150=mean_MeanHeight,
         mean_PercCanopyCoverCut0.1m_150=mean_PercCanopyCoverCut0.1m,     
         mean_PercCanopyCoverCut0.5m_150=mean_PercCanopyCoverCut0.5m,
         mean_PercCanopyCoverCut1.0m_150=mean_PercCanopyCoverCut1.0m,
         mean_PercCanopyCoverCut1.5m_150=mean_PercCanopyCoverCut1.5m,
         mean_PercCanopyDensity_150=mean_PercCanopyDensity,
         sd_MaxHeight_150=sd_MaxHeight,
         sd_MeanHeight_150=sd_MeanHeight,
         sd_PercCanopyCoverCut0.1m_150=sd_PercCanopyCoverCut0.1m,     
         sd_PercCanopyCoverCut0.5m_150=sd_PercCanopyCoverCut0.5m,
         sd_PercCanopyCoverCut1.0m_150=sd_PercCanopyCoverCut1.0m,
         sd_PercCanopyCoverCut1.5m_150=sd_PercCanopyCoverCut1.5m,
         sd_PercCanopyDensity_150=sd_PercCanopyDensity,
         cv_MaxHeight_150=cv_MaxHeight,
         cv_MeanHeight_150=cv_MeanHeight,
         cv_PercCanopyCoverCut0.1m_150=cv_PercCanopyCoverCut0.1m,     
         cv_PercCanopyCoverCut0.5m_150=cv_PercCanopyCoverCut0.5m,
         cv_PercCanopyCoverCut1.0m_150=cv_PercCanopyCoverCut1.0m,
         cv_PercCanopyCoverCut1.5m_150=cv_PercCanopyCoverCut1.5m,
         cv_PercCanopyDensity_150=cv_PercCanopyDensity,
         kurtosis_MaxHeight_150=kurtosis_MaxHeight,
         kurtosis_MeanHeight_150=kurtosis_MeanHeight,
         kurtosis_PercCanopyCoverCut0.1m_150=kurtosis_PercCanopyCoverCut0.1m,     
         kurtosis_PercCanopyCoverCut0.5m_150=kurtosis_PercCanopyCoverCut0.5m,
         kurtosis_PercCanopyCoverCut1.0m_150=kurtosis_PercCanopyCoverCut1.0m,
         kurtosis_PercCanopyCoverCut1.5m_150=kurtosis_PercCanopyCoverCut1.5m,
         kurtosis_PercCanopyDensity_150=kurtosis_PercCanopyDensity,
         skewness_MaxHeight_150=skewness_MaxHeight,
         skewness_MeanHeight_150=skewness_MeanHeight,
         skewness_PercCanopyCoverCut0.1m_150=skewness_PercCanopyCoverCut0.1m,     
         skewness_PercCanopyCoverCut0.5m_150=skewness_PercCanopyCoverCut0.5m,
         skewness_PercCanopyCoverCut1.0m_150=skewness_PercCanopyCoverCut1.0m,
         skewness_PercCanopyCoverCut1.5m_150=skewness_PercCanopyCoverCut1.5m,
         skewness_PercCanopyDensity_150=skewness_PercCanopyDensity,
         strata_00_to_02_returnProportion_150=strata_00_to_02_returnProportion,
         strata_02_to_04_returnProportion_150=strata_02_to_04_returnProportion,
         strata_04_to_06_returnProportion_150=strata_04_to_06_returnProportion,
         strata_06_to_08_returnProportion_150=strata_06_to_08_returnProportion,
         strata_08_to_10_returnProportion_150=strata_08_to_10_returnProportion,
         strata_10_to_15_returnProportion_150=strata_10_to_15_returnProportion,
         strata_15_to_20_returnProportion_150=strata_15_to_20_returnProportion,
         strata_20_above_returnProportion_150=strata_20_above_returnProportion)

habitat500<-read.csv("0_data/raw/site_lidar_cov_500m.csv", header=TRUE)
str(habitat500)
habitat500B<-habitat500%>%
  rename(mean_MaxHeight_500=mean_MaxHeight,
         mean_MeanHeight_500=mean_MeanHeight,
         mean_PercCanopyCoverCut0.1m_500=mean_PercCanopyCoverCut0.1m,     
         mean_PercCanopyCoverCut0.5m_500=mean_PercCanopyCoverCut0.5m,
         mean_PercCanopyCoverCut1.0m_500=mean_PercCanopyCoverCut1.0m,
         mean_PercCanopyCoverCut1.5m_500=mean_PercCanopyCoverCut1.5m,
         mean_PercCanopyDensity_500=mean_PercCanopyDensity,
         sd_MaxHeight_500=sd_MaxHeight,
         sd_MeanHeight_500=sd_MeanHeight,
         sd_PercCanopyCoverCut0.1m_500=sd_PercCanopyCoverCut0.1m,     
         sd_PercCanopyCoverCut0.5m_500=sd_PercCanopyCoverCut0.5m,
         sd_PercCanopyCoverCut1.0m_500=sd_PercCanopyCoverCut1.0m,
         sd_PercCanopyCoverCut1.5m_500=sd_PercCanopyCoverCut1.5m,
         sd_PercCanopyDensity_500=sd_PercCanopyDensity,
         cv_MaxHeight_500=cv_MaxHeight,
         cv_MeanHeight_500=cv_MeanHeight,
         cv_PercCanopyCoverCut0.1m_500=cv_PercCanopyCoverCut0.1m,     
         cv_PercCanopyCoverCut0.5m_500=cv_PercCanopyCoverCut0.5m,
         cv_PercCanopyCoverCut1.0m_500=cv_PercCanopyCoverCut1.0m,
         cv_PercCanopyCoverCut1.5m_500=cv_PercCanopyCoverCut1.5m,
         cv_PercCanopyDensity_500=cv_PercCanopyDensity,
         kurtosis_MaxHeight_500=kurtosis_MaxHeight,
         kurtosis_MeanHeight_500=kurtosis_MeanHeight,
         kurtosis_PercCanopyCoverCut0.1m_500=kurtosis_PercCanopyCoverCut0.1m,     
         kurtosis_PercCanopyCoverCut0.5m_500=kurtosis_PercCanopyCoverCut0.5m,
         kurtosis_PercCanopyCoverCut1.0m_500=kurtosis_PercCanopyCoverCut1.0m,
         kurtosis_PercCanopyCoverCut1.5m_500=kurtosis_PercCanopyCoverCut1.5m,
         kurtosis_PercCanopyDensity_500=kurtosis_PercCanopyDensity,
         skewness_MaxHeight_500=skewness_MaxHeight,
         skewness_MeanHeight_500=skewness_MeanHeight,
         skewness_PercCanopyCoverCut0.1m_500=skewness_PercCanopyCoverCut0.1m,     
         skewness_PercCanopyCoverCut0.5m_500=skewness_PercCanopyCoverCut0.5m,
         skewness_PercCanopyCoverCut1.0m_500=skewness_PercCanopyCoverCut1.0m,
         skewness_PercCanopyCoverCut1.5m_500=skewness_PercCanopyCoverCut1.5m,
         skewness_PercCanopyDensity_500=skewness_PercCanopyDensity,
         strata_00_to_02_returnProportion_500=strata_00_to_02_returnProportion,
         strata_02_to_04_returnProportion_500=strata_02_to_04_returnProportion,
         strata_04_to_06_returnProportion_500=strata_04_to_06_returnProportion,
         strata_06_to_08_returnProportion_500=strata_06_to_08_returnProportion,
         strata_08_to_10_returnProportion_500=strata_08_to_10_returnProportion,
         strata_10_to_15_returnProportion_500=strata_10_to_15_returnProportion,
         strata_15_to_20_returnProportion_500=strata_15_to_20_returnProportion,
         strata_20_above_returnProportion_500=strata_20_above_returnProportion)

#combine bird and Beaudoin layer habitat data

X1<-merge(birdsXY, habitat50B, by=c("SS"))
X2<-merge(X1, habitat150B, by=c("SS"))
birdsXY.hab<-merge(X2, habitat500B, by=c("SS"))
write.csv(birdsXY.hab, file="0_data/processed/birdsXY.hab.csv")#data check

birdsXY.hab<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)

#Graph mean values of site variables,
#coefficients of variation, and
#correlation coefficients among predictorx

#note: variables estimated to be zero or zero CV at all points have
#been highlighted out to reduce matrix size
mydata50<-birdsXY.hab[,c("mean_MaxHeight_50",
                         "mean_MeanHeight_50",
                         "mean_PercCanopyCoverCut0.1m_50",     
                         "mean_PercCanopyCoverCut0.5m_50",
                         "mean_PercCanopyCoverCut1.0m_50",
                         "mean_PercCanopyCoverCut1.5m_50",
                         "mean_PercCanopyDensity_50",
                         "sd_MaxHeight_50",
                         "sd_MeanHeight_50",
                         "sd_PercCanopyCoverCut0.1m_50",     
                         "sd_PercCanopyCoverCut0.5m_50",
                         "sd_PercCanopyCoverCut1.0m_50",
                         "sd_PercCanopyCoverCut1.5m_50",
                         "sd_PercCanopyDensity_50",
                         "cv_MaxHeight_50",
                         "cv_MeanHeight_50",
                         "cv_PercCanopyCoverCut0.1m_50",     
                         "cv_PercCanopyCoverCut0.5m_50",
                         "cv_PercCanopyCoverCut1.0m_50",
                         "cv_PercCanopyCoverCut1.5m_50",
                         "cv_PercCanopyDensity_50",
                         "kurtosis_MaxHeight_50",
                         "kurtosis_MeanHeight_50",
                         "kurtosis_PercCanopyCoverCut0.1m_50",     
                         "kurtosis_PercCanopyCoverCut0.5m_50",
                         "kurtosis_PercCanopyCoverCut1.0m_50",
                         "kurtosis_PercCanopyCoverCut1.5m_50",
                         "kurtosis_PercCanopyDensity_50",
                         "skewness_MaxHeight_50",
                         "skewness_MeanHeight_50",
                         "skewness_PercCanopyCoverCut0.1m_50",     
                         "skewness_PercCanopyCoverCut0.5m_50",
                         "skewness_PercCanopyCoverCut1.0m_50",
                         "skewness_PercCanopyCoverCut1.5m_50",
                         "skewness_PercCanopyDensity_50",
                         "strata_00_to_02_returnProportion_50",
                         "strata_02_to_04_returnProportion_50",
                         "strata_04_to_06_returnProportion_50",
                         "strata_06_to_08_returnProportion_50",
                         "strata_08_to_10_returnProportion_50",
                         "strata_10_to_15_returnProportion_50",
                         "strata_15_to_20_returnProportion_50",
                         "strata_20_above_returnProportion_50")]#just the Beaudoin variables
str(mydata50)

mydata50.cv<-mydata50%>%
  summarise_at(vars(mean_MaxHeight_50:strata_20_above_returnProportion_50), funs(CV))

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
  summarise_at(vars(mean_MaxHeight_50:strata_20_above_returnProportion_50), funs(mean))

mydata50.mean2 <- gather(mydata50.mean,
                       key = "predictor",
                       value = "mean")
#mydata50.mean2$goodmean<-ifelse(mydata50.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count
#Color/classification may not be relevant for LIDAR metrics as much as percent
#cover metrics

tiff("3_outputs/figures/MeanLidarPredictors50m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata50.mean2, aes(x=fct_reorder(predictor, mean), y=mean)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Value of LIDAR predictor")+
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

mydata150<-birdsXY.hab[,c("mean_MaxHeight_150",
                          "mean_MeanHeight_150",
                          "mean_PercCanopyCoverCut0.1m_150",     
                          "mean_PercCanopyCoverCut0.5m_150",
                          "mean_PercCanopyCoverCut1.0m_150",
                          "mean_PercCanopyCoverCut1.5m_150",
                          "mean_PercCanopyDensity_150",
                          "sd_MaxHeight_150",
                          "sd_MeanHeight_150",
                          "sd_PercCanopyCoverCut0.1m_150",     
                          "sd_PercCanopyCoverCut0.5m_150",
                          "sd_PercCanopyCoverCut1.0m_150",
                          "sd_PercCanopyCoverCut1.5m_150",
                          "sd_PercCanopyDensity_150",
                          "cv_MaxHeight_150",
                          "cv_MeanHeight_150",
                          "cv_PercCanopyCoverCut0.1m_150",     
                          "cv_PercCanopyCoverCut0.5m_150",
                          "cv_PercCanopyCoverCut1.0m_150",
                          "cv_PercCanopyCoverCut1.5m_150",
                          "cv_PercCanopyDensity_150",
                          "kurtosis_MaxHeight_150",
                          "kurtosis_MeanHeight_150",
                          "kurtosis_PercCanopyCoverCut0.1m_150",     
                          "kurtosis_PercCanopyCoverCut0.5m_150",
                          "kurtosis_PercCanopyCoverCut1.0m_150",
                          "kurtosis_PercCanopyCoverCut1.5m_150",
                          "kurtosis_PercCanopyDensity_150",
                          "skewness_MaxHeight_150",
                          "skewness_MeanHeight_150",
                          "skewness_PercCanopyCoverCut0.1m_150",     
                          "skewness_PercCanopyCoverCut0.5m_150",
                          "skewness_PercCanopyCoverCut1.0m_150",
                          "skewness_PercCanopyCoverCut1.5m_150",
                          "skewness_PercCanopyDensity_150",
                          "strata_00_to_02_returnProportion_150",
                          "strata_02_to_04_returnProportion_150",
                          "strata_04_to_06_returnProportion_150",
                          "strata_06_to_08_returnProportion_150",
                          "strata_08_to_10_returnProportion_150",
                          "strata_10_to_15_returnProportion_150",
                          "strata_15_to_20_returnProportion_150",
                          "strata_20_above_returnProportion_150")]#just the Beaudoin variables
str(mydata150)

#get coefficients of variation
mydata150.cv<-mydata150%>%
  summarise_at(vars(mean_MaxHeight_150:strata_20_above_returnProportion_150), funs(CV))

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
  summarise_at(vars(mean_MaxHeight_150:strata_20_above_returnProportion_150), funs(mean))

mydata150.mean2 <- gather(mydata150.mean,
                         key = "predictor",
                         value = "mean")
#mydata150.mean2$goodmean<-ifelse(mydata150.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count

tiff("3_outputs/figures/MeanLidarPredictors150m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata150.mean2, aes(x=fct_reorder(predictor, mean), y=mean)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Value of LIDAR Predictor")+
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


mydata500<-birdsXY.hab[,c("mean_MaxHeight_500",
                          "mean_MeanHeight_500",
                          "mean_PercCanopyCoverCut0.1m_500",     
                          "mean_PercCanopyCoverCut0.5m_500",
                          "mean_PercCanopyCoverCut1.0m_500",
                          "mean_PercCanopyCoverCut1.5m_500",
                          "mean_PercCanopyDensity_500",
                          "sd_MaxHeight_500",
                          "sd_MeanHeight_500",
                          "sd_PercCanopyCoverCut0.1m_500",     
                          "sd_PercCanopyCoverCut0.5m_500",
                          "sd_PercCanopyCoverCut1.0m_500",
                          "sd_PercCanopyCoverCut1.5m_500",
                          "sd_PercCanopyDensity_500",
                          "cv_MaxHeight_500",
                          "cv_MeanHeight_500",
                          "cv_PercCanopyCoverCut0.1m_500",     
                          "cv_PercCanopyCoverCut0.5m_500",
                          "cv_PercCanopyCoverCut1.0m_500",
                          "cv_PercCanopyCoverCut1.5m_500",
                          "cv_PercCanopyDensity_500",
                          "kurtosis_MaxHeight_500",
                          "kurtosis_MeanHeight_500",
                          "kurtosis_PercCanopyCoverCut0.1m_500",     
                          "kurtosis_PercCanopyCoverCut0.5m_500",
                          "kurtosis_PercCanopyCoverCut1.0m_500",
                          "kurtosis_PercCanopyCoverCut1.5m_500",
                          "kurtosis_PercCanopyDensity_500",
                          "skewness_MaxHeight_500",
                          "skewness_MeanHeight_500",
                          "skewness_PercCanopyCoverCut0.1m_500",     
                          "skewness_PercCanopyCoverCut0.5m_500",
                          "skewness_PercCanopyCoverCut1.0m_500",
                          "skewness_PercCanopyCoverCut1.5m_500",
                          "skewness_PercCanopyDensity_500",
                          "strata_00_to_02_returnProportion_500",
                          "strata_02_to_04_returnProportion_500",
                          "strata_04_to_06_returnProportion_500",
                          "strata_06_to_08_returnProportion_500",
                          "strata_08_to_10_returnProportion_500",
                          "strata_10_to_15_returnProportion_500",
                          "strata_15_to_20_returnProportion_500",
                          "strata_20_above_returnProportion_500")]#
str(mydata500)

#get coefficients of variation
mydata500.cv<-mydata500%>%
  summarise_at(vars(mean_MaxHeight_500:strata_20_above_returnProportion_500), funs(CV))

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
  summarise_at(vars(mean_MaxHeight_500:strata_20_above_returnProportion_500), funs(mean))

mydata500.mean2 <- gather(mydata500.mean,
                          key = "predictor",
                          value = "mean")
#mydata500.mean2$goodmean<-ifelse(mydata500.mean2$mean>30,1,0)
#30 % used because <30 suggests species is a minor component in any point count

tiff("3_outputs/figures/MeanLidarPredictors500m.tiff", units="in", width=24, height=24, res=300)
ggplot(mydata500.mean2, aes(x=fct_reorder(predictor, mean), y=mean)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y')+
  my.theme2+
  ylab("Mean Value of LIDAR Predictor")+
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
#spatial scales: density of hits>20 m, density of hits from 15-20 m,
#kurtosis/CV/sd of perc canopy cov 0.5, 1, and 1.5 m, skewness of 
#maximum height

#Correlation plots suggest that density of hits is not strongly correlated 
#among different height intervals, max and mean height are strongly (+) correlated
#percent canopy cover at different height intervals is strongly (+) correlated with 
#each other and (-) correlated with skewness

