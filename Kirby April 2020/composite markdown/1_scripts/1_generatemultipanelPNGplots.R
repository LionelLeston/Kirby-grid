library(png)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

names<-c("ALFL")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("AMRO")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("BOCH")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top50m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel50mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("CEDW")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("CHSP")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top50m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel50mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("COYE")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top50m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel50mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("DEJU")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("GRAJ")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("HETH")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}

names<-c("LCSP")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("LISP")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top150m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModelSatellite150mPredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("OSFL")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("OVEN")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("PAWA")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("REVI")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("RCKI")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("SWTH")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("SWSP")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top150m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModelAVI150mPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top150m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModelSatellite150mPredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("TEWA")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("WIWR")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top500m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModel500mLIDARPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("WTSP")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top500m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel500mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top500m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel500mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


names<-c("YRWA")
for (i in names){
  img1 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/figures/",i,"top50m_AVImodel.png"))), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(paste0("AVI analyses/3_outputs/maps/BestModel50mAVIPredicted",i,".png"))), interpolate = FALSE)
  img3 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/figures/",i,"top50m_Beaudoinmodel.png"))), interpolate = FALSE)
  img4 <-  rasterGrob(as.raster(readPNG(paste0("Beaudoin analyses/3_outputs/maps/BestModel50mSatellitePredicted",i,".png"))), interpolate = FALSE)
  img5 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/figures/",i,"top150m_LIDARmodel.png"))), interpolate = FALSE)
  img6 <-  rasterGrob(as.raster(readPNG(paste0("Lidar analyses/3_outputs/maps/BestModelLIDAR150mPredicted",i,".png"))), interpolate = FALSE)
  img7 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/figures/",i,"topmodel.png"))), interpolate = FALSE)
  img8 <-  rasterGrob(as.raster(readPNG(paste0("Composite analyses/3_outputs/maps/BestModelPredicted",i,".png"))), interpolate = FALSE)
  
  P1<-plot_grid(img1, img2, img3, img4, img5, img6, img7, img8,
                align = "h", nrow=4, ncol = 2, rel_widths = c(3/5, 2/5), labels = "AUTO")
  ggsave(paste0("composite markdown/2_outputs/PNGplot_",i,".png"), plot=P1, width=10, height=13, units=c("in"), dpi=300)
  
}


