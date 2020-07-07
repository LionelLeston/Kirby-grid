library(ggplot2)
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

#Read in the bird-habitat data (bird data summarized across visits)
birdsXY.hab<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)

birdsXY.hab$needleleaf.50.s<-scale(birdsXY.hab$SpeciesGroups_Needleleaf_Spp_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$needleleaf.150.s<-scale(birdsXY.hab$SpeciesGroups_Needleleaf_Spp_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$needleleaf.500.s<-scale(birdsXY.hab$SpeciesGroups_Needleleaf_Spp_v1_500, scale=TRUE, center=FALSE)

birdsXY.hab$blackspruce.50.s<-scale(birdsXY.hab$Species_Pice_Mar_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$blackspruce.150.s<-scale(birdsXY.hab$Species_Pice_Mar_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$blackspruce.500.s<-scale(birdsXY.hab$Species_Pice_Mar_v1_500, scale=TRUE, center=FALSE)

birdsXY.hab$volume.50.s<-scale(birdsXY.hab$Structure_Volume_Total_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$volume.150.s<-scale(birdsXY.hab$Structure_Volume_Total_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$volume.500.s<-scale(birdsXY.hab$Structure_Volume_Total_v1_500, scale=TRUE, center=FALSE)

birdsXY.hab$age.50.s<-scale(birdsXY.hab$Structure_Stand_Age_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$age.150.s<-scale(birdsXY.hab$Structure_Stand_Age_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$age.500.s<-scale(birdsXY.hab$Structure_Stand_Age_v1_500, scale=TRUE, center=FALSE)

birdsXY.hab$crownclosure.50.s<-scale(birdsXY.hab$Structure_Stand_CrownClosure_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$crownclosure.150.s<-scale(birdsXY.hab$Structure_Stand_CrownClosure_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$crownclosure.500.s<-scale(birdsXY.hab$Structure_Stand_CrownClosure_v1_500, scale=TRUE, center=FALSE)

birdsXY.hab$vegcover.50.s<-scale(birdsXY.hab$LandCover_Veg_v1_50, scale=TRUE, center=FALSE)
birdsXY.hab$vegcover.150.s<-scale(birdsXY.hab$LandCover_Veg_v1_150, scale=TRUE, center=FALSE)
birdsXY.hab$vegcover.500.s<-scale(birdsXY.hab$LandCover_Veg_v1_500, scale=TRUE, center=FALSE)

#Spatial Autocorrelation for ALFL
library(nlme)
birdsXY.hab$sqrtALFL<-sqrt(birdsXY.hab$ALFL_mean)
m.ALFL.50 <- gls(sqrtALFL ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.ALFL.50 <- Variogram(m.ALFL.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/ALFLVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.ALFL.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.ALFL.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("ALFL:50 m")
dev.off()

summary(m.ALFL.50)#decreases with increasing conifer cover

m.ALFL.150 <- gls(sqrtALFL ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.ALFL.150 <- Variogram(m.ALFL.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/ALFLVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.ALFL.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.ALFL.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("ALFL:150 m")
dev.off()

summary(m.ALFL.150)#decreases with increasing conifer cover

m.ALFL.500 <- gls(sqrtALFL ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.ALFL.500 <- Variogram(m.ALFL.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/ALFLVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.ALFL.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.ALFL.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("ALFL:500 m")
dev.off()

summary(m.ALFL.500)#ALFL doesn't significantly decrease with needle-leaf cover

#500 m scale has lowest AIC for ALFL

#Spatial Autocorrelation for AMRO
birdsXY.hab$sqrtAMRO<-sqrt(birdsXY.hab$AMRO_mean)
m.AMRO.50 <- gls(sqrtAMRO ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.AMRO.50 <- Variogram(m.AMRO.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/AMROVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.AMRO.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.AMRO.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("AMRO:50 m")
dev.off()

summary(m.AMRO.50)
#increases with increasing conifer cover
#decreases with increasing volume

m.AMRO.150 <- gls(sqrtAMRO ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.AMRO.150 <- Variogram(m.AMRO.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/AMROVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.AMRO.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.AMRO.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("AMRO:150 m")
dev.off()

summary(m.AMRO.150)
#increases with increasing conifer cover
#decreases with increasing volume

m.AMRO.500 <- gls(sqrtAMRO ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.AMRO.500 <- Variogram(m.AMRO.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/AMROVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.AMRO.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.AMRO.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("AMRO:500 m")
dev.off()

summary(m.AMRO.500)
#increases with increasing conifer cover
#decreases with increasing volume
#decreases with increasing stand age

#500 m scale has lowest AIC for AMRO

#Spatial Autocorrelation for BOCH_mean 
birdsXY.hab$sqrtBOCH<-sqrt(birdsXY.hab$BOCH_mean)
m.BOCH.50 <- gls(sqrtBOCH ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.BOCH.50 <- Variogram(m.BOCH.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/BOCHVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.BOCH.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.BOCH.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("BOCH:50 m")
dev.off()

summary(m.BOCH.50)
#doesn't significantly vary with variables at 50-m scale

m.BOCH.150 <- gls(sqrtBOCH ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.BOCH.150 <- Variogram(m.BOCH.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/BOCHVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.BOCH.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.BOCH.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("BOCH:150 m")
dev.off()

summary(m.BOCH.150)
#doesn't significantly vary with variables at 150-m scale

m.BOCH.500 <- gls(sqrtBOCH ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.BOCH.500 <- Variogram(m.BOCH.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/BOCHVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.BOCH.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.BOCH.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("BOCH:500 m")
dev.off()

summary(m.BOCH.500)
#doesn't significantly vary with variables at 500-m scale
#AIC for BOCH is lowest at 500-m scale
#semivariance increases with distance

#Spatial Autocorrelation for CEDW_mean 
birdsXY.hab$sqrtCEDW<-sqrt(birdsXY.hab$CEDW_mean)
m.CEDW.50 <- gls(sqrtCEDW ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.CEDW.50 <- Variogram(m.CEDW.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CEDWVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CEDW.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CEDW.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CEDW:50 m")
dev.off()

summary(m.CEDW.50)
#doesn't significantly vary with variables at 50-m scale

m.CEDW.150 <- gls(sqrtCEDW ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.CEDW.150 <- Variogram(m.CEDW.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CEDWVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CEDW.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CEDW.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CEDW:150 m")
dev.off()

summary(m.CEDW.150)
#doesn't significantly vary with variables at 150-m scale

m.CEDW.500 <- gls(sqrtCEDW ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.CEDW.500 <- Variogram(m.CEDW.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CEDWVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CEDW.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CEDW.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CEDW:500 m")
dev.off()

summary(m.CEDW.500)
#doesn't significantly vary with variables at 500-m scale
#AIC for CEDW is lowest at 500-m scale

#Spatial Autocorrelation for CHSP_mean 
birdsXY.hab$sqrtCHSP<-sqrt(birdsXY.hab$CHSP_mean)
m.CHSP.50 <- gls(sqrtCHSP ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.CHSP.50 <- Variogram(m.CHSP.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CHSPVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CHSP.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CHSP.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CHSP:50 m")
dev.off()

summary(m.CHSP.50)
#increases with increasing conifer cover at 50-m scale

m.CHSP.150 <- gls(sqrtCHSP ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.CHSP.150 <- Variogram(m.CHSP.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CHSPVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CHSP.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CHSP.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CHSP:150 m")
dev.off()

summary(m.CHSP.150)
#increases with increasing conifer cover at 150-m scale

m.CHSP.500 <- gls(sqrtCHSP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.CHSP.500 <- Variogram(m.CHSP.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/CHSPVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.CHSP.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.CHSP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("CHSP:500 m")
dev.off()

summary(m.CHSP.500)
#increases with increasing conifer cover at 500-m scale
#decreases with increasing volume at 500-m scale
#500-m scale has lowest AIC

#Spatial Autocorrelation for COYE_mean 
birdsXY.hab$sqrtCOYE<-sqrt(birdsXY.hab$COYE_mean)
m.COYE.50 <- gls(sqrtCOYE ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.COYE.50 <- Variogram(m.COYE.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/COYEVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.COYE.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.COYE.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("COYE:50 m")
dev.off()

summary(m.COYE.50)
#decreases with increasing crown closure at 50-m scale

m.COYE.150 <- gls(sqrtCOYE ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.COYE.150 <- Variogram(m.COYE.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/COYEVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.COYE.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.COYE.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("COYE:150 m")
dev.off()

summary(m.COYE.150)
#decreases with increasing crown closure at 150-m scale
#slight increase with increasing veg cover at 150-m scale

m.COYE.500 <- gls(sqrtCOYE ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.COYE.500 <- Variogram(m.COYE.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/COYEVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.COYE.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.COYE.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("COYE:500 m")
dev.off()

summary(m.COYE.500)
#decreases with increasing crown closure at 150-m scale
#increases with increasing veg cover at 150-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for DEJU_mean 
birdsXY.hab$sqrtDEJU<-sqrt(birdsXY.hab$DEJU_mean)
m.DEJU.50 <- gls(sqrtDEJU ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.DEJU.50 <- Variogram(m.DEJU.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/DEJUVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.DEJU.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.DEJU.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("DEJU:50 m")
dev.off()

summary(m.DEJU.50)
#decreases with increasing black spruce at 50-m scale
#increases with increasing conifer cover at 50-m scale

m.DEJU.150 <- gls(sqrtDEJU ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.DEJU.150 <- Variogram(m.DEJU.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/DEJUVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.DEJU.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.DEJU.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("DEJU:150 m")
dev.off()

summary(m.DEJU.150)
#decreases with increasing black spruce at 150-m scale
#increases with increasing conifer cover at 150-m scale

m.DEJU.500 <- gls(sqrtDEJU ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.DEJU.500 <- Variogram(m.DEJU.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/DEJUVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.DEJU.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.DEJU.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("DEJU:500 m")
dev.off()

summary(m.DEJU.500)
#decreases with increasing black spruce at 150-m scale
#increases with increasing conifer cover at 150-m scale
#AIC comparable at all spatial scales (50-m slightly lower 
#than 500-m which is slightly lower than 150-m scale: all
#within 2 AIC-units of each other) 

#Spatial Autocorrelation for GCKI_mean 
birdsXY.hab$sqrtGCKI<-sqrt(birdsXY.hab$GCKI_mean)
m.GCKI.50 <- gls(sqrtGCKI ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.GCKI.50 <- Variogram(m.GCKI.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GCKIVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GCKI.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GCKI.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GCKI:50 m")
dev.off()

summary(m.GCKI.50)
#increases with increasing black spruce at 50-m scale
#increases with increasing volume at 50-m scale

m.GCKI.150 <- gls(sqrtGCKI ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.GCKI.150 <- Variogram(m.GCKI.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GCKIVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GCKI.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GCKI.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GCKI:150 m")
dev.off()

summary(m.GCKI.150)
#increases with increasing black spruce at 150-m scale
#increases with increasing volume at 150-m scale

m.GCKI.500 <- gls(sqrtGCKI ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.GCKI.500 <- Variogram(m.GCKI.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GCKIVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GCKI.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GCKI.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GCKI:500 m")
dev.off()

summary(m.GCKI.500)
#increases with increasing black spruce at 500-m scale
#increases with increasing volume at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for GRAJ_mean 
birdsXY.hab$sqrtGRAJ<-sqrt(birdsXY.hab$GRAJ_mean)
m.GRAJ.50 <- gls(sqrtGRAJ ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.GRAJ.50 <- Variogram(m.GRAJ.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GRAJVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GRAJ.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GRAJ.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GRAJ:50 m")
dev.off()

summary(m.GRAJ.50)
#decreases with increasing crown closure at 50-m scale
#increases with increasing conifer cover at 50-m scale

m.GRAJ.150 <- gls(sqrtGRAJ ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.GRAJ.150 <- Variogram(m.GRAJ.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GRAJVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GRAJ.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GRAJ.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GRAJ:150 m")
dev.off()

summary(m.GRAJ.150)
#decreases with increasing crown closure at 150-m scale
#increases with increasing conifer cover at 150-m scale

m.GRAJ.500 <- gls(sqrtGRAJ ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.GRAJ.500 <- Variogram(m.GRAJ.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/GRAJVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.GRAJ.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.GRAJ.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("GRAJ:500 m")
dev.off()

summary(m.GRAJ.500)
#decreases with increasing black spruce cover at 500-m scale
#increases with increasing conifer cover at 500-m scale
#AIC comparable at all spatial scales (50-m slightly lower 
#than 150-m which is slightly lower than 500-m scale: all
#within 2 AIC-units of each other) 


#Spatial Autocorrelation for HETH_mean 
birdsXY.hab$sqrtHETH<-sqrt(birdsXY.hab$HETH_mean)
m.HETH.50 <- gls(sqrtHETH ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.HETH.50 <- Variogram(m.HETH.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/HETHVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.HETH.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.HETH.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("HETH:50 m")
dev.off()

summary(m.HETH.50)
#decreases with increasing black spruce cover at 50-m scale
#increases with increasing conifer cover at 50-m scale
#decreases with increasing volume at 50-m scale

m.HETH.150 <- gls(sqrtHETH ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.HETH.150 <- Variogram(m.HETH.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/HETHVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.HETH.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.HETH.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("HETH:150 m")
dev.off()

summary(m.HETH.150)
#decreases with increasing black spruce cover at 150-m scale
#increases with increasing conifer cover at 150-m scale
#decreases with increasing volume at 150-m scale

m.HETH.500 <- gls(sqrtHETH ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.HETH.500 <- Variogram(m.HETH.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/HETHVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.HETH.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.HETH.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("HETH:500 m")
dev.off()

summary(m.HETH.500)
#decreases with increasing black spruce cover at 500-m scale
#increases with increasing conifer cover at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for LCSP_mean 
birdsXY.hab$sqrtLCSP<-sqrt(birdsXY.hab$LCSP_mean)
m.LCSP.50 <- gls(sqrtLCSP ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.LCSP.50 <- Variogram(m.LCSP.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LCSPVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LCSP.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LCSP.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LCSP:50 m")
dev.off()

summary(m.LCSP.50)
#decreases with increasing crown closure at 50-m scale

m.LCSP.150 <- gls(sqrtLCSP ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.LCSP.150 <- Variogram(m.LCSP.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LCSPVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LCSP.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LCSP.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LCSP:150 m")
dev.off()

summary(m.LCSP.150)
#decreases with increasing crown closure at 150-m scale

m.LCSP.500 <- gls(sqrtLCSP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.LCSP.500 <- Variogram(m.LCSP.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LCSPVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LCSP.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LCSP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LCSP:500 m")
dev.off()

summary(m.LCSP.500)
#decreases with increasing crown closure at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for LISP_mean 
birdsXY.hab$sqrtLISP<-sqrt(birdsXY.hab$LISP_mean)
m.LISP.50 <- gls(sqrtLISP ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.LISP.50 <- Variogram(m.LISP.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LISPVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LISP.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LISP.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LISP:50 m")
dev.off()

summary(m.LISP.50)
#decreases with increasing crown closure at 50-m scale

m.LISP.150 <- gls(sqrtLISP ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.LISP.150 <- Variogram(m.LISP.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LISPVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LISP.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LISP.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LISP:150 m")
dev.off()

summary(m.LISP.150)
#decreases with increasing crown closure at 150-m scale

m.LISP.500 <- gls(sqrtLISP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.LISP.500 <- Variogram(m.LISP.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/LISPVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.LISP.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.LISP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("LISP:500 m")
dev.off()

summary(m.LISP.500)
#decreases with increasing crown closure at 500-m scale
#decreases with increasing stand age at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for OSFL_mean 
birdsXY.hab$sqrtOSFL<-sqrt(birdsXY.hab$OSFL_mean)
m.OSFL.50 <- gls(sqrtOSFL ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.OSFL.50 <- Variogram(m.OSFL.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OSFLVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OSFL.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OSFL.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OSFL:50 m")
dev.off()

summary(m.OSFL.50)
#decreases with increasing crown closure at 50-m scale
#semivariance peaks around 4000 m

m.OSFL.150 <- gls(sqrtOSFL ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.OSFL.150 <- Variogram(m.OSFL.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OSFLVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OSFL.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OSFL.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OSFL:150 m")
dev.off()

summary(m.OSFL.150)
#decreases with increasing crown closure at 150-m scale

m.OSFL.500 <- gls(sqrtOSFL ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.OSFL.500 <- Variogram(m.OSFL.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OSFLVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OSFL.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OSFL.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OSFL:500 m")
dev.off()

summary(m.OSFL.500)
#decreases with increasing crown closure at 500-m scale
#increases slightly with increasing volume at 500-m scale
#AIC lowest at 500-m scale
#semivariance peaks around 4000 m

#Spatial Autocorrelation for OVEN_mean 
birdsXY.hab$sqrtOVEN<-sqrt(birdsXY.hab$OVEN_mean)
m.OVEN.50 <- gls(sqrtOVEN ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.OVEN.50 <- Variogram(m.OVEN.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OVENVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OVEN.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OVEN.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OVEN:50 m")
dev.off()

summary(m.OVEN.50)
#decreases with increasing conifer cover at 50-m scale
#increases with increasing stand age at 50-m scale

m.OVEN.150 <- gls(sqrtOVEN ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.OVEN.150 <- Variogram(m.OVEN.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OVENVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OVEN.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OVEN.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OVEN:150 m")
dev.off()

summary(m.OVEN.150)
#decreases with increasing conifer cover at 150-m scale
#increases slightly with increasing stand age at 150-m scale

m.OVEN.500 <- gls(sqrtOVEN ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.OVEN.500 <- Variogram(m.OVEN.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/OVENVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.OVEN.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.OVEN.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("OVEN:500 m")
dev.off()

summary(m.OVEN.500)
#decreases with increasing conifer cover at 500-m scale
#increases  with increasing black spruce cover at 500-m scale
#increases slightly with increasing stand age at 500-m scale
#increases slightly with increasing vegetated cover at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for PAWA_mean 
birdsXY.hab$sqrtPAWA<-sqrt(birdsXY.hab$PAWA_mean)
m.PAWA.50 <- gls(sqrtPAWA ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.PAWA.50 <- Variogram(m.PAWA.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/PAWAVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.PAWA.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.PAWA.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("PAWA:50 m")
dev.off()

summary(m.PAWA.50)
#increases with increasing conifer cover at 50-m scale
#semivariance still increasing with distance

m.PAWA.150 <- gls(sqrtPAWA ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.PAWA.150 <- Variogram(m.PAWA.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/PAWAVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.PAWA.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.PAWA.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("PAWA:150 m")
dev.off()

summary(m.PAWA.150)
#increases with increasing conifer cover at 150-m scale

m.PAWA.500 <- gls(sqrtPAWA ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.PAWA.500 <- Variogram(m.PAWA.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/PAWAVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.PAWA.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.PAWA.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("PAWA:500 m")
dev.off()

summary(m.PAWA.500)
#increases with increasing conifer cover at 500-m scale
#semivariance still increasing with distance
#AIC lowest at 500-m scale

#Spatial Autocorrelation for RBNU_mean 
birdsXY.hab$sqrtRBNU<-sqrt(birdsXY.hab$RBNU_mean)
m.RBNU.50 <- gls(sqrtRBNU ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.RBNU.50 <- Variogram(m.RBNU.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RBNUVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RBNU.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RBNU.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RBNU:50 m")
dev.off()

summary(m.RBNU.50)
#no strong predictors

m.RBNU.150 <- gls(sqrtRBNU ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.RBNU.150 <- Variogram(m.RBNU.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RBNUVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RBNU.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RBNU.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RBNU:150 m")
dev.off()

summary(m.RBNU.150)
#no strong predictors

m.RBNU.500 <- gls(sqrtRBNU ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.RBNU.500 <- Variogram(m.RBNU.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RBNUVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RBNU.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RBNU.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RBNU:500 m")
dev.off()

summary(m.RBNU.500)
#increases with increasing volume at 500-m scale
#increases with increasing black spruce cover at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for RCKI_mean 
birdsXY.hab$sqrtRCKI<-sqrt(birdsXY.hab$RCKI_mean)
m.RCKI.50 <- gls(sqrtRCKI ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.RCKI.50 <- Variogram(m.RCKI.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RCKIVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RCKI.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RCKI.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RCKI:50 m")
dev.off()

summary(m.RCKI.50)
#no strong predictors

m.RCKI.150 <- gls(sqrtRCKI ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.RCKI.150 <- Variogram(m.RCKI.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RCKIVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RCKI.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RCKI.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RCKI:150 m")
dev.off()

summary(m.RCKI.150)
#no strong predictors

m.RCKI.500 <- gls(sqrtRCKI ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.RCKI.500 <- Variogram(m.RCKI.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/RCKIVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.RCKI.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.RCKI.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("RCKI:500 m")
dev.off()

summary(m.RCKI.500)
#no strong predictors
#AIC lowest at 500-m scale

#Spatial Autocorrelation for REVI_mean 
birdsXY.hab$sqrtREVI<-sqrt(birdsXY.hab$REVI_mean)
m.REVI.50 <- gls(sqrtREVI ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.REVI.50 <- Variogram(m.REVI.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/REVIVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.REVI.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.REVI.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("REVI:50 m")
dev.off()

summary(m.REVI.50)
#increased slightly with increasing stand age at 50-m scale
#decreased slightly with increasing volume at 50-m scale

m.REVI.150 <- gls(sqrtREVI ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.REVI.150 <- Variogram(m.REVI.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/REVIVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.REVI.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.REVI.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("REVI:150 m")
dev.off()

summary(m.REVI.150)
#increased with increasing stand age at 150-m scale
#increased slightly with increasing crown closure at 150-m scale
#decreased with increasing volume at 150-m scale


m.REVI.500 <- gls(sqrtREVI ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.REVI.500 <- Variogram(m.REVI.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/REVIVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.REVI.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.REVI.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("REVI:500 m")
dev.off()

summary(m.REVI.500)
#increased with increasing stand age at 500-m scale
#increased with increasing crown closure at 500-m scale
#decreased with increasing volume at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for SWSP_mean 
birdsXY.hab$sqrtSWSP<-sqrt(birdsXY.hab$SWSP_mean)
m.SWSP.50 <- gls(sqrtSWSP ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.SWSP.50 <- Variogram(m.SWSP.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWSPVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWSP.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWSP.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWSP:50 m")
dev.off()

summary(m.SWSP.50)
#increased slightly with increasing vegetation cover at 50-m scale
#decreased with increasing crown closure at 50-m scale

m.SWSP.150 <- gls(sqrtSWSP ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.SWSP.150 <- Variogram(m.SWSP.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWSPVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWSP.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWSP.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWSP:150 m")
dev.off()

summary(m.SWSP.150)
#increased with increasing vegetation cover at 150-m scale
#decreased with increasing crown closure at 150-m scale


m.SWSP.500 <- gls(sqrtSWSP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.SWSP.500 <- Variogram(m.SWSP.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWSPVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWSP.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWSP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWSP:500 m")
dev.off()

summary(m.SWSP.500)
#decreased with increasing crown closure at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for SWTH_mean 
birdsXY.hab$sqrtSWTH<-sqrt(birdsXY.hab$SWTH_mean)
m.SWTH.50 <- gls(sqrtSWTH ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.SWTH.50 <- Variogram(m.SWTH.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWTHVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWTH.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWTH.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWTH:50 m")
dev.off()

summary(m.SWTH.50)
#increased slightly with increasing stand age at 50-m scale

m.SWTH.150 <- gls(sqrtSWTH ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.SWTH.150 <- Variogram(m.SWTH.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWTHVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWTH.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWTH.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWTH:150 m")
dev.off()

summary(m.SWTH.150)
#increased with increasing stand age at 150-m scale


m.SWTH.500 <- gls(sqrtSWTH ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.SWTH.500 <- Variogram(m.SWTH.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/SWTHVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.SWTH.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.SWTH.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("SWTH:500 m")
dev.off()

summary(m.SWTH.500)
#increased with increasing stand age at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for TEWA_mean 
birdsXY.hab$sqrtTEWA<-sqrt(birdsXY.hab$TEWA_mean)
m.TEWA.50 <- gls(sqrtTEWA ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.TEWA.50 <- Variogram(m.TEWA.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TEWAVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TEWA.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TEWA.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TEWA:50 m")
dev.off()

summary(m.TEWA.50)
#decreased slightly with increasing volume at 50-m scale

m.TEWA.150 <- gls(sqrtTEWA ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.TEWA.150 <- Variogram(m.TEWA.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TEWAVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TEWA.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TEWA.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TEWA:150 m")
dev.off()

summary(m.TEWA.150)
#decreased slightly with increasing volume at 150-m scale
#increased slightly with increasing crown closure at 150-m scale


m.TEWA.500 <- gls(sqrtTEWA ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.TEWA.500 <- Variogram(m.TEWA.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TEWAVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TEWA.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TEWA.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TEWA:500 m")
dev.off()

summary(m.TEWA.500)
#increased slightly with increasing crown closure at 500-m scale
#AIC lowest at 500-m scale

#Spatial Autocorrelation for TRES_mean 
birdsXY.hab$sqrtTRES<-sqrt(birdsXY.hab$TRES_mean)
m.TRES.50 <- gls(sqrtTRES ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.TRES.50 <- Variogram(m.TRES.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TRESVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TRES.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TRES.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TRES:50 m")
dev.off()

summary(m.TRES.50)
#decreased with increasing vegetation cover at 50-m scale
#increased with increasing crown closure at 50-m scale

m.TRES.150 <- gls(sqrtTRES ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.TRES.150 <- Variogram(m.TRES.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TRESVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TRES.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TRES.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TRES:150 m")
dev.off()

summary(m.TRES.150)
#decreased with increasing vegetation cover at 150-m scale
#increased with increasing crown closure at 150-m scale


m.TRES.500 <- gls(sqrtTRES ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.TRES.500 <- Variogram(m.TRES.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/TRESVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.TRES.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.TRES.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("TRES:500 m")
dev.off()

summary(m.TRES.500)
#decreased with increasing vegetation cover at 150-m scale
#increased slightly with increasing crown closure at 150-m scale
#AIC slightly lower at 500-m scale than other scales

#Spatial Autocorrelation for WIWR_mean 
birdsXY.hab$sqrtWIWR<-sqrt(birdsXY.hab$WIWR_mean)
m.WIWR.50 <- gls(sqrtWIWR ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.WIWR.50 <- Variogram(m.WIWR.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WIWRVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WIWR.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WIWR.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WIWR:50 m")
dev.off()

summary(m.WIWR.50)
#decreased with increasing stand age at 50-m scale

m.WIWR.150 <- gls(sqrtWIWR ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.WIWR.150 <- Variogram(m.WIWR.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WIWRVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WIWR.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WIWR.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WIWR:150 m")
dev.off()

summary(m.WIWR.150)
#decreased with increasing stand age at 150-m scale
#increased with increasing black spruce cover at 150-m scale
#increased with increasing stand volume at 150-m scale


m.WIWR.500 <- gls(sqrtWIWR ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.WIWR.500 <- Variogram(m.WIWR.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WIWRVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WIWR.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WIWR.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WIWR:500 m")
dev.off()

summary(m.WIWR.500)
#decreased with increasing stand age at 500-m scale
#AIC lowest at 500-m scale


#Spatial Autocorrelation for WTSP_mean 
birdsXY.hab$sqrtWTSP<-sqrt(birdsXY.hab$WTSP_mean)
m.WTSP.50 <- gls(sqrtWTSP ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.WTSP.50 <- Variogram(m.WTSP.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WTSPVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WTSP.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WTSP.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WTSP:50 m")
dev.off()

summary(m.WTSP.50)
#no strong predictors
#semivariance increasing with distance

m.WTSP.150 <- gls(sqrtWTSP ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.WTSP.150 <- Variogram(m.WTSP.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WTSPVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WTSP.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WTSP.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WTSP:150 m")
dev.off()

summary(m.WTSP.150)
#decreased slightly with increasing vegetation cover at 150-m scale


m.WTSP.500 <- gls(sqrtWTSP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.WTSP.500 <- Variogram(m.WTSP.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WTSPVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WTSP.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WTSP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WTSP:500 m")
dev.off()

summary(m.WTSP.500)
#no strong predictors
#semivariance increasing with distance
#AIC lowest at 500-m scale

#Spatial Autocorrelation for YRWA_mean 
birdsXY.hab$sqrtYRWA<-sqrt(birdsXY.hab$YRWA_mean)
m.YRWA.50 <- gls(sqrtYRWA ~ vegcover.50.s+age.50.s+blackspruce.50.s+needleleaf.50.s+crownclosure.50.s+volume.50.s, data = birdsXY.hab)
vario.YRWA.50 <- Variogram(m.YRWA.50, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/YRWAVariogram50.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.YRWA.50, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.YRWA.50$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("YRWA:50 m")
dev.off()

summary(m.YRWA.50)
#no strong predictors

m.YRWA.150 <- gls(sqrtYRWA ~ vegcover.150.s+age.150.s+blackspruce.150.s+needleleaf.150.s+crownclosure.150.s+volume.150.s, data = birdsXY.hab)
vario.YRWA.150 <- Variogram(m.YRWA.150, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/YRWAVariogram150.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.YRWA.150, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.YRWA.150$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("YRWA:150 m")
dev.off()

summary(m.YRWA.150)
#no strong predictors at 150-m scale

m.YRWA.500 <- gls(sqrtYRWA ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s, data = birdsXY.hab)
vario.YRWA.500 <- Variogram(m.YRWA.500, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/YRWAVariogram500.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.YRWA.500, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.YRWA.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("YRWA:500 m")
dev.off()

summary(m.YRWA.500)
#no strong predictors at 500-m scale
#AIC lowest at 500-m scale

#CONCLUSIONS
#500-m scale should be used for modelling effects of Beaudoin variables
#For most species, there is negligible autocorrelation in residuals after
#accounting for vegetation, so regular N-mixture or occupancy models can be
#used

#Exceptions: Olive-sided Flycatcher (4000-m neighborhood)
#            Boreal Chickadee, Palm Warbler, White-throated Sparrow
#            For these 3 species, some missing predictor is causing 
#            a trend in the semivariance

m.BOCH.500B <- gls(sqrtBOCH ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s+EASTING+NORTHING, data = birdsXY.hab)
vario.BOCH.500B <- Variogram(m.BOCH.500B, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/BOCHVariogram500B.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.BOCH.500B, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.BOCH.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("BOCH:500 m")
dev.off()
#including Northing and Easting as predictors reduces the
#increase in semivariance of BOCH abundance with distance

m.PAWA.500B <- gls(sqrtPAWA ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s+EASTING+NORTHING, data = birdsXY.hab)
vario.PAWA.500B <- Variogram(m.PAWA.500B, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/PAWAVariogram500B.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.PAWA.500B, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.PAWA.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("PAWA:500 m")
dev.off()
#including Northing and Easting as predictors reduces the
#increase in semivariance of PAWA abundance with distance

m.WTSP.500B <- gls(sqrtWTSP ~ vegcover.500.s+age.500.s+blackspruce.500.s+needleleaf.500.s+crownclosure.500.s+volume.500.s+EASTING+NORTHING, data = birdsXY.hab)
vario.WTSP.500B <- Variogram(m.WTSP.500B, form = ~birdsXY.hab$EASTING + birdsXY.hab$NORTHING, resType = "pearson")

# Variogram
tiff("3_outputs/figures/WTSPVariogram500B.tiff", units="in", width=6, height=6, res=300)
ggplot(data = vario.WTSP.500B, aes(dist,variog))+
  geom_point(colour = "red", size = 3)+ylim(0,max(vario.WTSP.500$variog))+
  xlab("Distance")+ylab("Semivariogram")+ggtitle("WTSP:500 m")
dev.off()
#including Northing and Easting as predictors reduces the
#increase in semivariance of WTSP abundance with distance
