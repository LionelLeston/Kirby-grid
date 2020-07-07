library(lubridate)
library(unmarked)
library(MuMIn)
library(jpeg)
library(ggplot2)
library(grDevices)
library(dplyr)
library(stringr)
library(nmixgof)
library(spdep)
library(rgdal)
library(tmap)
library(raster)
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

kirby <- raster("0_data/raw/Kirby_CWD_Vol.tif")

Kirby.coord<-read.csv("0_data/raw/Kirby.coord.csv")
LCC <- CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs")
coordinates(Kirby.coord) <- Kirby.coord[,c("EASTING", "NORTHING")] 
proj4string(Kirby.coord) <- LCC


#https://www.rdocumentation.org/packages/MuMIn/versions/1.43.15/topics/pdredge
#a website link showing where someone else has calculated R-squared
#as a goodness-of-fit statistic for an N-mixture model

#other option would be nmixgof package that came out in 2018, which
#I think does not communicate with MuMIn

#so while I don't know exactly why R-squared isn't generally used
#for N-mixture models, it is available


my.theme <- theme_classic() +
  theme(text=element_text(size=10, family="Arial"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
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

# Suppose we want to have set of models that exclude combinations of colinear
# variables, that are significantly (p < 0.05) correlated, with Pearson
# correlation coefficient equal to or larger than r = 0.7.

#https://github.com/cran/MuMIn/blob/master/demo/dredge.subset.R
is.correlated <- function(i, j, data, conf.level = .95, cutoff = .69, ...) {
  if(j >= i) return(NA)
  ct <- cor.test(data[, i], data[, j], conf.level = conf.level, ...)
  ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
}
# Need vectorized function to use with 'outer'
vCorrelated <- Vectorize(is.correlated, c("i", "j"))



birddata<-read.csv(file="0_data/raw/allvisitsBG15.csv", header=TRUE)      
str(birddata)
#make sure the data used doesn't include any stations missing habitat data
#and is sorted by station the same way as the habitat data and the station
#location data was when creating the spatial matrix

#Since we are using mixture models, there should be multiple
#rows (observations) for each station. In contrast to the unmarked
#package, observation and detection covariate data are in long
#format rather than wide format, when modelling with the HSDM package
kirbypoints4shp<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)

alldata<-merge(kirbypoints4shp, birddata, by=c("SS"))
write.csv(alldata, file="0_data/processed/alldata.csv")

alldata<-read.csv("0_data/processed/alldata.csv", header=TRUE)

alldata$MY.DATE1 <- do.call(paste, list(alldata$Month, alldata$Day, alldata$Year))
alldata$MY.DATE1 <- as.Date(alldata$MY.DATE1, format=c("%m %d %Y"))
alldata$Julian <- as.numeric(format(alldata$MY.DATE1, "%j"))

#1. output needs to be "recast so that values associated with separate visits to same point count are put in separate columns
#The same procedure is used for both observations and detection covariates
names = c("ALFL","AMRO","BOCH","CEDW","CHSP","COYE","DEJU","GCKI","GRAJ",
          "HETH","Julian","LCSP","LISP","OSFL","OVEN","PAWA","RBNU","RCKI",
          "REVI","SWSP","SWTH","TEWA","TRES","WIWR","WTSP","YRWA")

alldata$ROUND <- ave(alldata$ALFL_mean, alldata$SS, FUN = seq_along)

for(i in names){ 
  spp<-alldata[,c(i)]  #Species as variable
  
  recasted<-tapply(spp, list(alldata$SS, alldata$ROUND), max)
  write.csv(recasted, paste0("0_data/processed/unmarked/",i,".csv"))
}  

#2. site covariates
lidar<-read.csv("0_data/processed/Kirby.Lidar.2spatscale.csv",header=TRUE)
beaudoin<-read.csv("0_data/processed/Kirby.Beaudoin.3spatscale.csv",header=TRUE)
avi<-read.csv("0_data/processed/Kirby.avi.3spatscale.csv",header=TRUE)
m1<-merge(lidar,beaudoin,by=c("SS"))
m2<-merge(m1,avi,by=c("SS"))
write.csv(m2, file="0_data/processed/Kirby.alllayersandscales.csv")

site_all_cov<-read.csv("0_data/processed/Kirby.alllayersandscales.csv",header=TRUE)
site_all_cov$SS<-NULL
cormat <- round(cor(site_all_cov),2)
head(cormat)
cormat.df<-data.frame(cormat)
write.csv(cormat.df,file="0_data/processed/cormatdf.csv")

# so I can only choose 1 variable at a time in models from the VPD covariates due to strong
# correlations: strongly + (>0.7) between all VPD means at both 150-m and 500-m scales
#              strongly - (<-0.7) between all VPD sd variables at both scales
#              Max height strongly + correlated with mean height and mean canopy density/cover
#                         strongly + with return proportion in higher height intervals
#                         strongly - correlated with CV in mean height and skewness in mean height
#                - correlated (but not extremely) with SD canopy density/cover
# So I can't combine VPD metrics from different spatial scales in same model

Julian<-read.csv("0_data/processed/unmarked/Julian.csv",header=TRUE)
ObsCov<-list(as.matrix(Julian[,2:5]))
sitecov<-read.csv("0_data/processed/Kirby.alllayersandscales.csv",header=TRUE)
sitecov<-sitecov[,-1]

sitecov$Tremblingaspen_50<-sitecov$Tremblingaspen_50+sitecov$TremblingaspenMixCD_50+sitecov$TremblingaspenMixD_50+sitecov$TremblingaspenMixDC_50
sitecov$TremblingaspenMixCD_50<-NULL
sitecov$TremblingaspenMixD_50<-NULL
sitecov$TremblingaspenMixDC_50<-NULL

sitecov$Jackpine_50<-sitecov$Jackpine_50+sitecov$JackpineMixCD_50+sitecov$JackpineMixWet_50
sitecov$Jackpine_50JackpineMixCD_50<-NULL	
sitecov$Jackpine_50JackpineMixWet_50<-NULL

sitecov$Tamarack_50<-sitecov$Tamarack_50+sitecov$TamarackMixCD_50+sitecov$TamarackMixDC_50
sitecov$TamarackMixCD_50<-NULL
sitecov$TamarackMixDC_50<-NULL

sitecov$Blackspruce_50<-sitecov$Blackspruce_50+sitecov$BlackspruceMixCD_50
sitecov$BlackspruceMixCD_50<-NULL

sitecov$Whitebirch_50<-sitecov$Whitebirch_50+sitecov$WhitebirchMixD_50
sitecov$WhitebirchMixD_50<-NULL

sitecov$Whitespruce_50<-sitecov$Whitespruce_50+sitecov$WhitespruceMixWet_50
sitecov$WhitespruceMixWet_50<-NULL

sitecov$Tremblingaspen_150<-sitecov$Tremblingaspen_150+sitecov$TremblingaspenMixCD_150+sitecov$TremblingaspenMixD_150+sitecov$TremblingaspenMixDC_150
sitecov$TremblingaspenMixCD_150<-NULL
sitecov$TremblingaspenMixD_150<-NULL
sitecov$TremblingaspenMixDC_150<-NULL

sitecov$Jackpine_150<-sitecov$Jackpine_150+sitecov$JackpineMixCD_150+sitecov$JackpineMixWet_150
sitecov$Jackpine_150JackpineMixCD_150<-NULL	
sitecov$Jackpine_150JackpineMixWet_150<-NULL

sitecov$Tamarack_150<-sitecov$Tamarack_150+sitecov$TamarackMixCD_150+sitecov$TamarackMixDC_150
sitecov$TamarackMixCD_150<-NULL
sitecov$TamarackMixDC_150<-NULL

sitecov$Blackspruce_150<-sitecov$Blackspruce_150+sitecov$BlackspruceMixCD_150+sitecov$BlackspruceMixDC_150
sitecov$BlackspruceMixCD_150<-NULL
sitecov$BlackspruceMixDC_150<-NULL

sitecov$Whitebirch_150<-sitecov$Whitebirch_150+sitecov$WhitebirchMixD_150+sitecov$WhitebirchMixDC_150
sitecov$WhitebirchMixD_150<-NULL
sitecov$WhitebirchMixDC_150<-NULL

sitecov$Whitespruce_150<-sitecov$Whitespruce_150+sitecov$WhitespruceMixWet_150+sitecov$WhitespruceMixC_150+sitecov$WhitespruceMixCD_150
sitecov$WhitespruceMixWet_150<-NULL
sitecov$WhitespruceMixC_150<-NULL
sitecov$WhitespruceMixCD_150<-NULL

sitecov$Tremblingaspen_500<-sitecov$Tremblingaspen_500+sitecov$TremblingaspenMixCD_500+sitecov$TremblingaspenMixD_500+sitecov$TremblingaspenMixDC_500
sitecov$TremblingaspenMixCD_500<-NULL
sitecov$TremblingaspenMixD_500<-NULL
sitecov$TremblingaspenMixDC_500<-NULL

sitecov$Jackpine_500<-sitecov$Jackpine_500+sitecov$JackpineMixCD_500+sitecov$JackpineMixWet_500#+sitecov$Jackpine_500JackpineMixC_500
#sitecov$Jackpine_500JackpineMixC_500<-NULL	
sitecov$Jackpine_500JackpineMixCD_500<-NULL	
sitecov$Jackpine_500JackpineMixWet_500<-NULL

sitecov$Tamarack_500<-sitecov$Tamarack_500+sitecov$TamarackMixCD_500+sitecov$TamarackMixDC_500
sitecov$TamarackMixCD_500<-NULL
sitecov$TamarackMixDC_500<-NULL

sitecov$Blackspruce_500<-sitecov$Blackspruce_500+sitecov$BlackspruceMixCD_500+sitecov$BlackspruceMixDC_500
sitecov$BlackspruceMixCD_500<-NULL
sitecov$BlackspruceMixDC_500<-NULL

sitecov$Whitebirch_500<-sitecov$Whitebirch_500+sitecov$WhitebirchMixD_500+sitecov$WhitebirchMixDC_500
sitecov$WhitebirchMixD_500<-NULL
sitecov$WhitebirchMixDC_500<-NULL

sitecov$Whitespruce_500<-sitecov$Whitespruce_500+sitecov$WhitespruceMixWet_500+sitecov$WhitespruceMixC_500+sitecov$WhitespruceMixCD_500
sitecov$WhitespruceMixWet_500<-NULL
sitecov$WhitespruceMixC_500<-NULL
sitecov$WhitespruceMixCD_500<-NULL

sitecov$EASTING<-NULL

#Composite model: YRWA
names<-c("YRWA")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  

  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                             ~Jackpine_50+
                              S150.VPD__3.4m_Grid.1x1m.mean+ 
                              S150.VPD__4.5m_Grid.1x1m.mean+
                              Species_Pice_Gla_v1_50+
                              Structure_Biomass_TotalDead_v1_50,
                             mixture="P",
                             data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Jackpine_50",
                            "S150.VPD__3.4m_Grid.1x1m.mean",
                            "S150.VPD__4.5m_Grid.1x1m.mean",
                            "Species_Pice_Gla_v1_50",
                            "Structure_Biomass_TotalDead_v1_50")]
  
  # Create logical matrix
  smat <- outer(1:5, 1:5, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                        subset=smat,
                                        rank=AIC, 
                                        m.lim=c(1,3), 
                                        cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
 
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))

  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                            labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)

  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.8798808 (marginal)
  chat(topfinalmodel, "s")#1.015197  (site-sum)
  chat(topfinalmodel, "o")#0.9641453 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE) 
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2
  dev.off()
  
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
  
}


#Top AVI model for YRWA
topavi<-pcount(~Julian ~ Jackpine_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.9029452 (marginal)
chat(topavi, "s")#1.062528 (site-sum)
chat(topavi, "o")#1.198114 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for YRWA
toplidar<-pcount(~Julian ~ S150.VPD__3.4m_Grid.1x1m.mean + S150.VPD__4.5m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9186023 (marginal)
chat(toplidar, "s")#1.035039 (site-sum)
chat(toplidar, "o")#0.8370036 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for YRWA
topbeaudoin<-pcount(~Julian ~ Species_Pice_Gla_v1_50 + Structure_Biomass_TotalDead_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9088067 (marginal)
chat(topbeaudoin, "s")#1.055288 (site-sum)
chat(topbeaudoin, "o")#0.9816854 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: DEJU
names<-c("DEJU")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Jackpine_150+
                            Tamarack_150+
                            Blackspruce_150+
                            S150.VPD__100.150cm_Grid.1x1m.mean + 
                            S150.VPD__3.4m_Grid.1x1m.mean+
                            Species_Pice_Mar_v1_50+
                            SpeciesGroups_Needleleaf_Spp_v1_50+
                            Structure_Biomass_TotalDead_v1_50,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Jackpine_150",
                            "Tamarack_150",
                            "Blackspruce_150",
                            "S150.VPD__100.150cm_Grid.1x1m.mean",
                            "S150.VPD__3.4m_Grid.1x1m.mean",
                            "Species_Pice_Mar_v1_50",
                            "SpeciesGroups_Needleleaf_Spp_v1_50",
                            "Structure_Biomass_TotalDead_v1_50")]
  
  # Create logical matrix
  smat <- outer(1:8, 1:8, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<19)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<19,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)

  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.867246 (marginal)
  chat(topfinalmodel, "s")#0.8435441 (site-sum)
  chat(topfinalmodel, "o")#0.9804777 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE) 
  
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2
  dev.off()
  
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
}


#Top AVI model for DEJU
topavi<-pcount(~1 ~ Blackspruce_150 + Jackpine_150 + Tamarack_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.8779891 (marginal)
chat(topavi, "s")#0.8795544 (site-sum)
chat(topavi, "o")#1.19876 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for DEJU
toplidar<-pcount(~1 ~ S150.VPD__100.150cm_Grid.1x1m.mean + S150.VPD__3.4m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.8764327 (marginal)
chat(toplidar, "s")#0.8398037 (site-sum)
chat(toplidar, "o")#1.09651 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for DEJU
topbeaudoin<-pcount(~1 ~  Species_Pice_Mar_v1_50 + SpeciesGroups_Needleleaf_Spp_v1_50 + Structure_Biomass_TotalDead_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9513196 (marginal)
chat(topbeaudoin, "s")#0.9050383 (site-sum)
chat(topbeaudoin, "o")#1.096022 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: GRAJ
names<-c("GRAJ")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_150+
                            Whitebirch_150+
                            S500.sd_PercCanopyDensity + 
                            S500.VPD__2.3m_Grid.1x1m.mean+
                            Species_Pinu_Ban_v1_50+
                            Species_Sali_Spp_v1_50+
                            Structure_Stand_CrownClosure_v1_50,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_150",
                            "Whitebirch_150",
                            "S500.sd_PercCanopyDensity",
                            "S500.VPD__2.3m_Grid.1x1m.mean",
                            "Species_Pinu_Ban_v1_50",
                            "Species_Sali_Spp_v1_50",
                            "Structure_Stand_CrownClosure_v1_50")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<11.5)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<11.5,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#1.026925 (marginal)
  chat(topfinalmodel, "s")#1.063135 (site-sum)
  chat(topfinalmodel, "o")#1.195906 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-  tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE) 
  
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2
  dev.off()
  
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for GRAJ
topavi<-pcount(~Julian ~ Tremblingaspen_150 + Whitebirch_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.9362374 (marginal)
chat(topavi, "s")#1.034216 (site-sum)
chat(topavi, "o")#0.9522474 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for GRAJ
toplidar<-pcount(~Julian ~ S500.sd_PercCanopyDensity + S500.VPD__2.3m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9730648 (marginal)
chat(toplidar, "s")#1.034618 (site-sum)
chat(toplidar, "o")#1.442733 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for GRAJ
topbeaudoin<-pcount(~Julian ~ Species_Pinu_Ban_v1_50 + Species_Sali_Spp_v1_50 + Structure_Stand_CrownClosure_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9097504 (marginal)
chat(topbeaudoin, "s")#1.034716 (site-sum)
chat(topbeaudoin, "o")#1.405815 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: LISP
names<-c("LISP")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_150+
                            Tamarack_150+
                            S150.VPD__11.12m_Grid.1x1m.mean + 
                            S150.VPD__2.3m_Grid.1x1m.mean+
                            SpeciesGroups_Needleleaf_Spp_v1_150+
                            Structure_Stand_CrownClosure_v1_150,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_150",
                            "Tamarack_150",
                            "S150.VPD__11.12m_Grid.1x1m.mean", 
                            "S150.VPD__2.3m_Grid.1x1m.mean",
                            "SpeciesGroups_Needleleaf_Spp_v1_150",
                            "Structure_Stand_CrownClosure_v1_150")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<25.47)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<25.47,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.9607402 (marginal)
  chat(topfinalmodel, "s")#1.056749 (site-sum)
  chat(topfinalmodel, "o")#1.424385 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE) 
  
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for LISP
topavi<-pcount(~Julian ~ Tamarack_150 + Tremblingaspen_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.608456 (marginal)
chat(topavi, "s")#2.716749 (site-sum)
chat(topavi, "o")#2.230703 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for LISP
toplidar<-pcount(~Julian ~ S150.VPD__11.12m_Grid.1x1m.mean + S150.VPD__2.3m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9730648 (marginal)
chat(toplidar, "s")#1.034618 (site-sum)
chat(toplidar, "o")#1.442733 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for LISP
topbeaudoin<-pcount(~Julian ~ SpeciesGroups_Needleleaf_Spp_v1_150 + Structure_Stand_CrownClosure_v1_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.246942 (marginal)
chat(topbeaudoin, "s")#1.562005 (site-sum)
chat(topbeaudoin, "o")#1.925797 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: BOCH
names<-c("BOCH")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_50+
                            Water_50+
                            wtage_50+
                            S150.VPD__3.4m_Grid.1x1m.mean+
                            Species_Abie_Bal_v1_500+
                            Species_Sali_Spp_v1_500+
                            Structure_Stand_Age_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_50",
                            "Water_50",
                            "wtage_50",
                            "S150.VPD__3.4m_Grid.1x1m.mean",
                            "Species_Abie_Bal_v1_500",
                            "Species_Sali_Spp_v1_500",
                            "Structure_Stand_Age_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<17.2)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<17.2,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.955396 (marginal)
  chat(topfinalmodel, "s")#0.8953851 (site-sum)
  chat(topfinalmodel, "o")#6.706103 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are fairly close
  #to 1 except for observation residual quantiles
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for BOCH
topavi<-pcount(~1 ~  Tremblingaspen_50 + Water_50 + wtage_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.821688 (marginal)
chat(topavi, "s")#0.7760913 (site-sum)
chat(topavi, "o")#8.664391 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for BOCH
toplidar<-pcount(~1 ~ S150.VPD__3.4m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9852858 (marginal)
chat(toplidar, "s")#0.996103 (site-sum)
chat(toplidar, "o")#2.945198 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for BOCH
topbeaudoin<-pcount(~1 ~ Species_Abie_Bal_v1_500 + Species_Sali_Spp_v1_500 + Structure_Stand_Age_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9315962 (marginal)
chat(topbeaudoin, "s")#0.8899961 (site-sum)
chat(topbeaudoin, "o")#5.618865 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: CHSP
names<-c("CHSP")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_50+
                            S150.mean_MaxHeight + 
                            S150.sd_PercCanopyDensity + 
                            S150.VPD__4.5m_Grid.1x1m.mean+
                            SpeciesGroups_Broadleaf_Spp_v1_500+
                            Structure_Biomass_TotalLiveAboveGround_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_50",
                            "S150.mean_MaxHeight",
                            "S150.sd_PercCanopyDensity",
                            "S150.VPD__4.5m_Grid.1x1m.mean",
                            "SpeciesGroups_Broadleaf_Spp_v1_500",
                            "Structure_Biomass_TotalLiveAboveGround_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<23)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<23,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.9662442 (marginal)
  chat(topfinalmodel, "s")#1.095116 (site-sum)
  chat(topfinalmodel, "o")#1.905918 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for CHSP
topavi<-pcount(~Julian ~  Tremblingaspen_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.101352 (marginal)
chat(topavi, "s")#1.242659 (site-sum)
chat(topavi, "o")#0.7667685 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for CHSP
toplidar<-pcount(~1 ~ S150.mean_MaxHeight + S150.sd_PercCanopyDensity + S150.VPD__4.5m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9950934 (marginal)
chat(toplidar, "s")#1.188448 (site-sum)
chat(toplidar, "o")#1.376169 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for CHSP
topbeaudoin<-pcount(~1 ~ SpeciesGroups_Broadleaf_Spp_v1_500 + Structure_Biomass_TotalLiveAboveGround_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.0074 (marginal)
chat(topbeaudoin, "s")#1.05237 (site-sum)
chat(topbeaudoin, "o")#0.7161943 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: HETH
names<-c("HETH")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_150+
                            Shrub_150+
                            S500.VPD__2.3m_Grid.1x1m.mean+
                            S500.sd_PercCanopyDensity+
                            Species_Pinu_Ban_v1_500+
                            Structure_Biomass_TotalLiveAboveGround_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_150",
                            "Shrub_150",
                            "S500.VPD__2.3m_Grid.1x1m.mean",
                            "S500.sd_PercCanopyDensity",
                            "Species_Pinu_Ban_v1_500",
                            "Structure_Biomass_TotalLiveAboveGround_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<15)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<15,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#1.056298 (marginal)
  chat(topfinalmodel, "s")#1.056822 (site-sum)
  chat(topfinalmodel, "o")#0.8914252 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for HETH
topavi<-pcount(~Julian ~  Shrub_150 + Tremblingaspen_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.244775 (marginal)
chat(topavi, "s")#1.202857 (site-sum)
chat(topavi, "o")#1.352567 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for HETH
toplidar<-pcount(~Julian ~ S500.sd_PercCanopyDensity + S500.VPD__2.3m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.083159 (marginal)
chat(toplidar, "s")#1.032231 (site-sum)
chat(toplidar, "o")#1.441288 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for HETH
topbeaudoin<-pcount(~Julian ~ Species_Pinu_Ban_v1_500 + Structure_Biomass_TotalLiveAboveGround_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.115559 (marginal)
chat(topbeaudoin, "s")#1.09263 (site-sum)
chat(topbeaudoin, "o")#1.207317 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: PAWA
names<-c("PAWA")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_150+
                            Fen_150+
                            Swamp_150+
                            S500.VPD__10.11m_Grid.1x1m.mean + 
                            S500.VPD__100.150cm_Grid.1x1m.mean + 
                            S500.VPD__50.100cm_Grid.1x1m.mean+
                            Species_Pice_Gla_v1_500 + 
                            SpeciesGroups_Needleleaf_Spp_v1_500 + 
                            Structure_Stand_CrownClosure_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_150",
                            "Fen_150",
                            "Swamp_150",
                            "S500.VPD__10.11m_Grid.1x1m.mean", 
                            "S500.VPD__100.150cm_Grid.1x1m.mean", 
                            "S500.VPD__50.100cm_Grid.1x1m.mean",
                            "Species_Pice_Gla_v1_500",
                            "SpeciesGroups_Needleleaf_Spp_v1_500",
                            "Structure_Stand_CrownClosure_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:9, 1:9, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<12.4)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<12.4,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.87 (marginal)
  chat(topfinalmodel, "s")#NaN (site-sum)
  chat(topfinalmodel, "o")#5.794401 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals 
  #suggest problems: overdispersion in detection and site-sum
  #residual quantiles are missing for some reason
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for PAWA
topavi<-pcount(~1 ~  Fen_150 + Swamp_150 + Tremblingaspen_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#2.012103 (marginal)
chat(topavi, "s")#NA (site-sum)
chat(topavi, "o")#4.869671 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for PAWA
toplidar<-pcount(~1 ~ S500.VPD__10.11m_Grid.1x1m.mean + S500.VPD__100.150cm_Grid.1x1m.mean + S500.VPD__50.100cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9826479 (marginal)
chat(toplidar, "s")#1.068609 (site-sum)
chat(toplidar, "o")#5.314741 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for PAWA
topbeaudoin<-pcount(~1 ~ Species_Pice_Gla_v1_500 + SpeciesGroups_Needleleaf_Spp_v1_500 + Structure_Stand_CrownClosure_v1_500 , umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9559754 (marginal)
chat(topbeaudoin, "s")#1.03687 (site-sum)
chat(topbeaudoin, "o")#5.126827 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: RCKI
names<-c("RCKI")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Whitespruce_150+
                            S150.VPD__5.6m_Grid.1x1m.mean+
                            Species_Abie_Bal_v1_500 + 
                            SpeciesGroups_Needleleaf_Spp_v1_500, 
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Whitespruce_150",
                            "Species_Abie_Bal_v1_500",
                            "S150.VPD__5.6m_Grid.1x1m.mean",
                            "SpeciesGroups_Needleleaf_Spp_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:4, 1:4, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.7933058 (marginal)
  chat(topfinalmodel, "s")#0.837124 (site-sum)
  chat(topfinalmodel, "o")#1.255975 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for RCKI
topavi<-pcount(~Julian ~  Whitespruce_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.797262 (marginal)
chat(topavi, "s")#0.896631 (site-sum)
chat(topavi, "o")#1.110761 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for RCKI
toplidar<-pcount(~Julian ~  S150.VPD__5.6m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.8191948 (marginal)
chat(toplidar, "s")#0.8145767 (site-sum)
chat(toplidar, "o")#1.25397 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for RCKI
topbeaudoin<-pcount(~Julian ~ Species_Abie_Bal_v1_500 + SpeciesGroups_Needleleaf_Spp_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.7954621 (marginal)
chat(topbeaudoin, "s")#0.8922284 (site-sum)
chat(topbeaudoin, "o")#1.420046 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: SWTH
names<-c("SWTH")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Whitespruce_50+
                            Water_500+
                            Shrub_500+
                            Species_Sali_Spp_v1_500 + 
                            SpeciesGroups_Broadleaf_Spp_v1_500+
                            S500.VPD__100.150cm_Grid.1x1m.mean + 
                            S500.VPD__150.200cm_Grid.1x1m.mean + 
                            S500.VPD__2.3m_Grid.1x1m.mean, 
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Whitespruce_150",
                            "Water_500",
                            "Shrub_500",
                            "Species_Sali_Spp_v1_500",
                            "SpeciesGroups_Broadleaf_Spp_v1_500",
                            "S500.VPD__100.150cm_Grid.1x1m.mean", 
                            "S500.VPD__150.200cm_Grid.1x1m.mean", 
                            "S500.VPD__2.3m_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:8, 1:8, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#1.010571 (marginal)
  chat(topfinalmodel, "s")#1.109437 (site-sum)
  chat(topfinalmodel, "o")#0.8539998 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for SWTH
topavi<-pcount(~1 ~ Shrub_500 + Water_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.049401 (marginal)
chat(topavi, "s")#1.170345 (site-sum)
chat(topavi, "o")#0.8875022 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for SWTH
toplidar<-pcount(~1 ~ S500.VPD__100.150cm_Grid.1x1m.mean + S500.VPD__150.200cm_Grid.1x1m.mean + S500.VPD__2.3m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.038285 (marginal)
chat(toplidar, "s")#1.139284 (site-sum)
chat(toplidar, "o")#1.053962 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for SWTH
topbeaudoin<-pcount(~1 ~ Species_Sali_Spp_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.031671 (marginal)
chat(topbeaudoin, "s")#1.103413 (site-sum)
chat(topbeaudoin, "o")#0.8393579 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: TEWA
names<-c("TEWA")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Fen_500+
                            Swamp_500+
                            Tremblingaspen_500+
                            S150.VPD__100.150cm_Grid.1x1m.mean + 
                            S150.VPD__150.200cm_Grid.1x1m.mean + 
                            S150.VPD__3.4m_Grid.1x1m.mean+
                            Species_Sali_Spp_v1_500+
                            SpeciesGroups_Broadleaf_Spp_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Fen_500",
                            "Swamp_500",
                            "Tremblingaspen_500",
                            "S150.VPD__100.150cm_Grid.1x1m.mean",
                            "S150.VPD__150.200cm_Grid.1x1m.mean",
                            "S150.VPD__3.4m_Grid.1x1m.mean",
                            "Species_Sali_Spp_v1_50",
                            "SpeciesGroups_Broadleaf_Spp_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:8, 1:8, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#1.115619 (marginal)
  chat(topfinalmodel, "s")#1.194322 (site-sum)
  chat(topfinalmodel, "o")#0.6868311 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for TEWA
topavi<-pcount(~1 ~ Fen_500 + Swamp_500 + Tremblingaspen_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.049401 (marginal)
chat(topavi, "s")#1.170345 (site-sum)
chat(topavi, "o")#0.8875022 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for TEWA
toplidar<-pcount(~1 ~ S150.VPD__100.150cm_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean + S150.VPD__3.4m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.217391 (marginal)
chat(toplidar, "s")#1.473638 (site-sum)
chat(toplidar, "o")#0.6631111 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for TEWA
topbeaudoin<-pcount(~1 ~  SpeciesGroups_Broadleaf_Spp_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.217388 (marginal)
chat(topbeaudoin, "s")#1.411709 (site-sum)
chat(topbeaudoin, "o")#0.8417961 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: WIWR
names<-c("WIWR")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Whitespruce_500+
                            wtage_500+
                            S500.VPD__11.12m_Grid.1x1m.mean + 
                            S500.VPD__8.9m_Grid.1x1m.mean + 
                            S500.VPD__9.10m_Grid.1x1m.mean+
                            Structure_Biomass_TotalLiveAboveGround_v1_500,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Whitespruce_500",
                            "wtage_500",
                            "S500.VPD__11.12m_Grid.1x1m.mean",
                            "S500.VPD__8.9m_Grid.1x1m.mean",
                            "S500.VPD__9.10m_Grid.1x1m.mean",
                            "Structure_Biomass_TotalLiveAboveGround_v1_500")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10.1)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10.1,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.859358 (marginal)
  chat(topfinalmodel, "s")#0.8424213 (site-sum)
  chat(topfinalmodel, "o")#1.11674 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for WIWR
topavi<-pcount(~1 ~  Whitespruce_500 + wtage_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.006254 (marginal)
chat(topavi, "s")#1.099046 (site-sum)
chat(topavi, "o")#0.7645517 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for WIWR
toplidar<-pcount(~1 ~ S500.VPD__11.12m_Grid.1x1m.mean + S500.VPD__8.9m_Grid.1x1m.mean + S500.VPD__9.10m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.859358 (marginal)
chat(toplidar, "s")#0.8424213 (site-sum)
chat(toplidar, "o")#1.266902 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for WIWR
topbeaudoin<-pcount(~1 ~  Structure_Biomass_TotalLiveAboveGround_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.003909 (marginal)
chat(topbeaudoin, "s")#0.9643066 (site-sum)
chat(topbeaudoin, "o")#1.177261 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: WTSP
names<-c("WTSP")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_500+
                            Water_500+
                            wtage_500+
                            LandCover_NonVeg_v1_500+
                            Species_Popu_Bal_v1_500+
                            Species_Sali_Spp_v1_500+
                            S150.sd_PercCanopyDensity,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_500",
                            "Water_500",
                            "wtage_500",
                            "LandCover_NonVeg_v1_500",
                            "Species_Popu_Bal_v1_500",
                            "Species_Sali_Spp_v1_500",
                            "S150.sd_PercCanopyDensity")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<24.5)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<24.5,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.935812 (marginal)
  chat(topfinalmodel, "s")#0.9500872 (site-sum)
  chat(topfinalmodel, "o")#0.9702225 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for WTSP
topavi<-pcount(~1 ~  Tremblingaspen_500 + Water_500 + wtage_500 , umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.006254 (marginal)
chat(topavi, "s")#1.099046 (site-sum)
chat(topavi, "o")#0.7645517 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for WTSP
toplidar<-pcount(~Julian ~ S150.sd_PercCanopyDensity, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.859358 (marginal)
chat(toplidar, "s")#0.8424213 (site-sum)
chat(toplidar, "o")#1.266902 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for WTSP
topbeaudoin<-pcount(~1 ~ LandCover_NonVeg_v1_500 + Species_Popu_Bal_v1_500 + Species_Sali_Spp_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9852962 (marginal)
chat(topbeaudoin, "s")#1.012288 (site-sum)
chat(topbeaudoin, "o")#0.934012 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: SWSP
names<-c("SWSP")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Shrub_150+
                            Tamarack_150+
                            LandCover_NonVeg_v1_150+
                            Structure_Biomass_TotalLiveAboveGround_v1_150+
                            Structure_Stand_CrownClosure_v1_150+
                            S150.mean_MaxHeight+ 
                            S150.VPD__100.150cm_Grid.1x1m.mean + 
                            S150.VPD__150.200cm_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Shrub_150",
                            "Tamarack_150",
                            "LandCover_NonVeg_v1_150",
                            "Structure_Biomass_TotalLiveAboveGround_v1_150",
                            "Structure_Stand_CrownClosure_v1_150",
                            "S150.mean_MaxHeight",
                            "S150.VPD__100.150cm_Grid.1x1m.mean", 
                            "S150.VPD__150.200cm_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:8, 1:8, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<12.5)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<12.5,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.9381911 (marginal)
  chat(topfinalmodel, "s")#0.9126356 (site-sum)
  chat(topfinalmodel, "o")#6.928417 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals: overdispersion
  #in detection
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for WTSP
topavi<-pcount(~1 ~  Shrub_150 + Tamarack_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.006254 (marginal)
chat(topavi, "s")#1.099046 (site-sum)
chat(topavi, "o")#0.7645517 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for SWSP
toplidar<-pcount(~1 ~ S150.mean_MaxHeight + S150.VPD__100.150cm_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.328075 (marginal)
chat(toplidar, "s")#1.281335 (site-sum)
chat(toplidar, "o")#12.30556 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for SWSP
topbeaudoin<-pcount(~1 ~ LandCover_NonVeg_v1_150 + Structure_Biomass_TotalLiveAboveGround_v1_150 + Structure_Stand_CrownClosure_v1_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.158624 (marginal)
chat(topbeaudoin, "s")#1.422012 (site-sum)
chat(topbeaudoin, "o")#2.809512 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: OVEN
names<-c("OVEN")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_500+
                            Whitebirch_500+
                            Species_Pice_Gla_v1_500+
                            SpeciesGroups_Broadleaf_Spp_v1_500+
                            Structure_Stand_Age_v1_500+
                            S500.mean_MaxHeight,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_500",
                            "Whitebirch_500",
                            "Species_Pice_Gla_v1_500",
                            "SpeciesGroups_Broadleaf_Spp_v1_500",
                            "Structure_Stand_Age_v1_500",
                            "S500.mean_MaxHeight")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<12.75)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<12.75,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.7371942 (marginal)
  chat(topfinalmodel, "s")#0.6945616 (site-sum)
  chat(topfinalmodel, "o")#1.114903 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}

#Top AVI model for OVEN
topavi<-pcount(~Julian ~Tremblingaspen_500 + Whitebirch_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.7371942 (marginal)
chat(topavi, "s")#0.6945616 (site-sum)
chat(topavi, "o")#1.822173 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for OVEN
toplidar<-pcount(~Julian ~ S500.mean_MaxHeight, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.7924934 (marginal)
chat(toplidar, "s")#0.7704697 (site-sum)
chat(toplidar, "o")#1.054496 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for OVEN
topbeaudoin<-pcount(~1 ~ Species_Pice_Gla_v1_500 + SpeciesGroups_Broadleaf_Spp_v1_500 + Structure_Stand_Age_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9772815 (marginal)
chat(topbeaudoin, "s")#0.8809036 (site-sum)
chat(topbeaudoin, "o")#1.088194 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: OSFL
names<-c("OSFL")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_500+
                            Shrub_500+
                            Jackpine_500+
                            Species_Pice_Gla_v1_500+
                            LandCover_NonVeg_v1_500+
                            Structure_Stand_CrownClosure_v1_500+
                            S500.VPD__100.150cm_Grid.1x1m.mean + 
                            S500.VPD__2.3m_Grid.1x1m.mean + 
                            S500.VPD__9.10m_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_500",
                            "Shrub_500",
                            "Jackpine_500",
                            "Species_Pice_Gla_v1_500",
                            "LandCover_NonVeg_v1_500",
                            "Structure_Stand_CrownClosure_v1_500",
                            "S500.VPD__100.150cm_Grid.1x1m.mean",
                            "S500.VPD__2.3m_Grid.1x1m.mean", 
                            "S500.VPD__9.10m_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:9, 1:9, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.7814778 (marginal)
  chat(topfinalmodel, "s")#0.5939955 (site-sum)
  chat(topfinalmodel, "o")#0.7599534 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for OSFL
topavi<-pcount(~1 ~Jackpine_500 + Shrub_500 + Tremblingaspen_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.035216 (marginal)
chat(topavi, "s")#0.8393777 (site-sum)
chat(topavi, "o")#0.8244579 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for OSFL
toplidar<-pcount(~1 ~ S500.VPD__100.150cm_Grid.1x1m.mean + S500.VPD__2.3m_Grid.1x1m.mean + S500.VPD__9.10m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.7814778 (marginal)
chat(toplidar, "s")#0.5939955 (site-sum)
chat(toplidar, "o")#0.750606 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for OSFL
topbeaudoin<-pcount(~1 ~ LandCover_NonVeg_v1_500 + Species_Pice_Gla_v1_500 + Structure_Stand_CrownClosure_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.7956099 (marginal)
chat(topbeaudoin, "s")#0.6930604 (site-sum)
chat(topbeaudoin, "o")#2.448985 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: COYE
names<-c("COYE")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Tremblingaspen_50+
                            Whitespruce_50+
                            Shrub_50+
                            SpeciesGroups_Needleleaf_Spp_v1_500+
                            LandCover_NonVeg_v1_500+
                            Structure_Stand_CrownClosure_v1_500+
                            S150.mean_MaxHeight + 
                            S150.VPD__100.150cm_Grid.1x1m.mean + 
                            S150.VPD__150.200cm_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Tremblingaspen_50",
                            "Whitespruce_50",
                            "Shrub_50",
                            "SpeciesGroups_Needleleaf_Spp_v1_500",
                            "LandCover_NonVeg_v1_500",
                            "Structure_Stand_CrownClosure_v1_500",
                            "S150.mean_MaxHeight",
                            "S150.VPD__100.150cm_Grid.1x1m.mean", 
                            "S150.VPD__150.200cm_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:9, 1:9, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<12)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<12,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.5489239 (marginal)
  chat(topfinalmodel, "s")#0.5071303 (site-sum)
  chat(topfinalmodel, "o")#0.9423108 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for COYE
topavi<-pcount(~1 ~Shrub_50 + Tremblingaspen_50 + Whitespruce_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.9788665 (marginal)
chat(topavi, "s")#1.041639 (site-sum)
chat(topavi, "o")#1.469194 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for COYE
toplidar<-pcount(~1 ~ S150.mean_MaxHeight + S150.VPD__100.150cm_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.68 (marginal)
chat(toplidar, "s")#0.6256 (site-sum)
chat(toplidar, "o")#0.99 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for COYE
topbeaudoin<-pcount(~1 ~ LandCover_NonVeg_v1_500 + SpeciesGroups_Needleleaf_Spp_v1_500 + Structure_Stand_CrownClosure_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.6505185 (marginal)
chat(topbeaudoin, "s")#0.5897802 (site-sum)
chat(topbeaudoin, "o")#0.7807436 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)



#Composite model: ALFL
names<-c("ALFL")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Blackspruce_500+
                            Species_Popu_Tre_v1_50+
                            Structure_Biomass_TotalLiveAboveGround_v1_50+
                            S150.sd_PercCanopyDensity + 
                            S150.VPD__100.150cm_Grid.1x1m.mean + 
                            S150.VPD__150.200cm_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Blackspruce_500",
                            "Species_Popu_Tre_v1_50",
                            "Structure_Biomass_TotalLiveAboveGround_v1_50",
                            "S150.sd_PercCanopyDensity",
                            "S150.VPD__100.150cm_Grid.1x1m.mean", 
                            "S150.VPD__150.200cm_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:6, 1:6, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<32.1)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<32.1,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.6638707 (marginal)
  chat(topfinalmodel, "s")#0.7722949 (site-sum)
  chat(topfinalmodel, "o")#1.186202 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for ALFL
topavi<-pcount(~Julian ~Blackspruce_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.158007 (marginal)
chat(topavi, "s")#1.268217 (site-sum)
chat(topavi, "o")#0.852423 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for ALFL
toplidar<-pcount(~1 ~ S150.sd_PercCanopyDensity + S150.VPD__100.150cm_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.7542503 (marginal)
chat(toplidar, "s")#0.9089271 (site-sum)
chat(toplidar, "o")#1.826852 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for ALFL
topbeaudoin<-pcount(~Julian ~ Species_Popu_Tre_v1_50 + Structure_Biomass_TotalLiveAboveGround_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.8477653 (marginal)
chat(topbeaudoin, "s")#0.9718551 (site-sum)
chat(topbeaudoin, "o")#1.172518 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)




#Composite model: AMRO
names<-c("AMRO")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Bog_500 + Shrub_500 + wtage_500+
                            Species_Abie_Bal_v1_500 + 
                            Structure_Biomass_TotalDead_v1_500+
                            S150.mean_MaxHeight + 
                            S150.VPD__4.5m_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Bog_500",
                            "Shrub_500",
                            "wtage_500",
                            "Species_Abie_Bal_v1_500",
                            "Structure_Biomass_TotalDead_v1_500",
                            "S150.VPD__4.5m_Grid.1x1m.mean",
                            "S150.mean_MaxHeight")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.8850566 (marginal)
  chat(topfinalmodel, "s")#1.107229 (site-sum)
  chat(topfinalmodel, "o")#2.385914 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for AMRO
topavi<-pcount(~1 ~Bog_500 + Shrub_500 + wtage_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#1.017597 (marginal)
chat(topavi, "s")#0.9974801 (site-sum)
chat(topavi, "o")#1.33354 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for AMRO
toplidar<-pcount(~Julian ~ S150.mean_MaxHeight + S150.VPD__4.5m_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.9943492 (marginal)
chat(toplidar, "s")#1.107168 (site-sum)
chat(toplidar, "o")#1.512605 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for AMRO
topbeaudoin<-pcount(~Julian ~ Species_Abie_Bal_v1_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#0.9365323 (marginal)
chat(topbeaudoin, "s")#1.194596 (site-sum)
chat(topbeaudoin, "o")#1.282342 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)




#Composite model: CEDW
names<-c("CEDW")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Blackspruce_150 + Whitebirch_150+
                            S500.VPD__150.200cm_Grid.1x1m.mean + 
                            S500.VPD__50.100cm_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Blackspruce_150",
                            "Whitebirch_150",
                            "S500.VPD__150.200cm_Grid.1x1m.mean",
                            "S500.VPD__50.100cm_Grid.1x1m.mean")]
  
  # Create logical matrix
  smat <- outer(1:4, 1:4, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<10)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<10,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.9804641 (marginal)
  chat(topfinalmodel, "s")#1.01373 (site-sum)
  chat(topfinalmodel, "o")#8.658941 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}



#Top AVI model for CEDW
topavi<-pcount(~Julian ~Blackspruce_150 + Whitebirch_150, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.9804641 (marginal)
chat(topavi, "s")#1.01373 (site-sum)
chat(topavi, "o")#13.95168 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for CEDW
toplidar<-pcount(~Julian ~ S500.VPD__150.200cm_Grid.1x1m.mean + S500.VPD__50.100cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.193881 (marginal)
chat(toplidar, "s")#1.293343 (site-sum)
chat(toplidar, "o")#11.5361 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: LCSP
names<-c("LCSP")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Swamp_500 + 
                            Tamarack_500 + 
                            LandCover_NonVeg_v1_50 + 
                            Structure_Stand_CrownClosure_v1_50+
                            S150.sd_PercCanopyDensity + 
                            S150.VPD__10.11m_Grid.1x1m.mean + 
                            S150.VPD__150.200cm_Grid.1x1m.mean,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Swamp_500", 
                            "Tamarack_500",
                            "LandCover_NonVeg_v1_50", 
                            "Structure_Stand_CrownClosure_v1_50",
                            "S150.VPD__10.11m_Grid.1x1m.mean", 
                            "S150.VPD__150.200cm_Grid.1x1m.mean", 
                            "S150.mean_MaxHeight")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<22.5)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<22.5,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
 
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))

  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  

  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.5804501 (marginal)
  chat(topfinalmodel, "s")#0.5256867 (site-sum)
  chat(topfinalmodel, "o")#1.745138 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for LCSP
topavi<-pcount(~Julian ~Shrub_500 + Tamarack_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.9788665 (marginal)
chat(topavi, "s")#1.041639 (site-sum)
chat(topavi, "o")#1.469194 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for LCSP
toplidar<-pcount(~1 ~ S150.sd_PercCanopyDensity + S150.VPD__10.11m_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#0.69 (marginal)
chat(toplidar, "s")#0.73 (site-sum)
chat(toplidar, "o")#1.685 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for LCSP
topbeaudoin<-pcount(~Julian ~ LandCover_NonVeg_v1_50 + Structure_Stand_CrownClosure_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.230648 (marginal)
chat(topbeaudoin, "s")#1.209896 (site-sum)
chat(topbeaudoin, "o")#1.759559 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


#Composite model: LCSP
names<-c("REVI")
for(i in names){ 
  birddata<-read.csv(paste0("0_data/processed/unmarked/",i,".csv"),header=TRUE)
  count=birddata[,2:5]
  #same variable names in both data frames
  
  #3. Now construct unmarkedPCount frame for mixture models
  umf <- unmarkedFramePCount(y=count, siteCovs=sitecov, 
                             obsCovs=list(Julian=Julian[,2:5]))# organize data
  umf                            # take a look
  summary(umf)   
  obsCovs(umf) <- scale(obsCovs(umf))
  siteCovs(umf) <- scale(siteCovs(umf))
  
  
  #parallel processing and using dredge
  clusterType<-if(length(find.package("snow",
                                      quiet=TRUE))) "SOCK" else "PSOCK"
  
  clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
  clusterEvalQ(clust, library(unmarked))
  clusterExport(clust, "umf")
  
  
  
  #5. Run mixture models
  
  #to dredge with parallel processing:
  
  ###global multilayer model
  global_pcount <-pcount( ~ Julian
                          ~Whitespruce_500 + 
                            Whitebirch_500 +
                            Tremblingaspen_500 +
                            Species_Sali_Spp_v1_500+
                            Structure_Biomass_TotalLiveAboveGround_v1_500+ 
                            Structure_Stand_CrownClosure_v1_500+
                            S500.mean_MaxHeight,
                          mixture="P",
                          data = umf)
  print(paste0('Global multilayer model run for ',i))
  
  site.covs.mat<-sitecov[,c("Whitespruce_500", 
                            "Whitebirch_500",
                            "Tremblingaspen_500", 
                            "Species_Sali_Spp_v1_500",
                            "Structure_Biomass_TotalLiveAboveGround_v1_500", 
                            "Structure_Stand_CrownClosure_v1_500", 
                            "S500.mean_MaxHeight")]
  
  # Create logical matrix
  smat <- outer(1:7, 1:7, vCorrelated, data = site.covs.mat)
  nm <- colnames(site.covs.mat)#[-1]
  dimnames(smat) <- list(nm, nm)
  
  system.time(dredge_pcount<-pdredge(global_pcount, 
                                     subset=smat,
                                     rank=AIC, 
                                     m.lim=c(1,3), 
                                     cluster=clust,
                                     extra = "R^2"))#,fixed=c("p(Julian)")
  print(paste0('Multilayer model dredge run for ',i))
  
  length(dredge_pcount)
  nrow(dredge_pcount)#
  bestmodels<-get.models(dredge_pcount, subset=delta<22.5)#x
  
  #absolute model fit
  df.dredge<-data.frame(dredge_pcount)
  df.dredge<-df.dredge[df.dredge$delta<22.5,]
  r2<-df.dredge$R.2
  #write.csv(df.dredge, file=paste0("0_data/processed/unmarked/results/",i,"dredge.csv"))
  
  
  ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                                      labels = getAllTerms(global_pcount)))))
  ms.coef<-coef(ms)
  ms.SE<-SE(ms)
  output<-as(ms,"data.frame")
  outputB<-cbind(output,r2)
  write.csv(outputB, file=paste0("0_data/processed/unmarked/results/",i,"bestmodels.csv"))
  print(paste0('Multilayer model outputs printed for ',i))
  
  #get variables and estimates of their uncertainty from top model 
  topfinalmodel<-get.models(dredge_pcount, subset = 1)[[1]]
  Estimate<-coef(topfinalmodel, type='state', altNames = TRUE)
  SE<-SE(topfinalmodel, type='state', altNames = TRUE)
  lam.df<-data.frame(Estimate,SE)
  lam.df$Predictor<-row.names(lam.df)
  Estimate<-coef(topfinalmodel, type='det', altNames = TRUE)
  SE<-SE(topfinalmodel, type='det', altNames = TRUE)
  P.df<-data.frame(Estimate,SE)
  P.df$Predictor<-row.names(P.df)
  total.df<-bind_rows(lam.df,P.df)
  total.df$LCL<-total.df$Estimate-1.96*total.df$SE
  total.df$UCL<-total.df$Estimate+1.96*total.df$SE
  total.df<-total.df[,c("Predictor","Estimate","SE","LCL","UCL")]
  write.csv(total.df, file=paste0("3_outputs/tables/topmodel",i,".csv"))
  
  #box plot
  #shorten predictor names
  total.df$Predictor<- gsub("_v1_", "_", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Structure_Stand_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("Species_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("SpeciesGroups_", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  total.df$Predictor<- gsub("_Grid.1x1m.mean", "", total.df$Predictor) # new descriptor for harvest class in v.2018 cure4insect
  #total.df<-total.df[!total.df$Predictor=="lam(Int)",]  
  #total.df<-total.df[!total.df$Predictor=="p(Int)",]  
  
  GG<-ggplot(total.df, aes(x=Predictor, y=Estimate))+
    geom_point(aes(x=Predictor, y=Estimate))+
    geom_errorbar(aes(ymin=LCL,ymax=UCL))+
    geom_hline(yintercept=0)+
    xlab("Predictor")+
    ylab(paste0("Effect on lam(",i,") or det(",i,")"))+my.theme+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggsave(paste0("3_outputs/figures/",i,"topmodel.png"), plot=GG, width=12, height=6, units=c("in"), dpi=300)
  
  #An estimate of overdispersion relative to the fitted model.
  chat(topfinalmodel, "m")#0.86 (marginal)
  chat(topfinalmodel, "s")#1.00 (site-sum)
  chat(topfinalmodel, "o")#1.33 (observation)
  
  residcov(topfinalmodel, xlim=c(-10,10))
  
  #plot residuals against fitted values
  residfit(topfinalmodel, type="m") # (marginal)
  residfit(topfinalmodel, "s") # (site-sum)
  residfit(topfinalmodel, "o") #(observation)
  
  #randomized quantile residuals vs. normal standardized quantiles 
  residqq(topfinalmodel, type = "site-sum", main = "Residual qq plot",
          plotLine = TRUE)
  residqq(topfinalmodel, type = "o", main = "Residual qq plot",
          plotLine = TRUE)
  
  #residuals look good: hug the centre line of various Q-Q plots, do
  #not vary in any obvious way with increasing fitted values, and
  #c-hat values for the 3 kinds of quantile residuals are approximately 1
  
  #get predictions from best model and validate best model
  umf_val<-umf#same data set set up as an occupancy modelling frame
  p.hat <- predict(topfinalmodel, newdata=umf_val, type="det")[,1] 
  lam.hat <- predict(topfinalmodel, newdata=umf_val, type="state")[,1]
  pred.abund<-data.frame(lam.hat)
  pred.abund$SS<-Kirby.coord$SS
  
  
  stationdata_sp <- merge(Kirby.coord, pred.abund, by.x = "SS", by.y = "SS")
  str(stationdata_sp)
  plot(stationdata_sp, col=stationdata_sp$lam.hat)
  stationdata_sp@bbox
  stationdata_sp@coords
  
  #plot(kirby)
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red','green'))
  plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  
  # Replace point boundary extent with that of Kirby grid
  coords = matrix(c(488000, 6131500,
                    488000, 6137500,
                    496000, 6137500,
                    496000, 6131500,
                    488000, 6131500), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
  #plot(Ps1, axes = TRUE)
  #plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata_sp$lam.hat, breaks = 10))])
  stationdata_sp@bbox<-Ps1@bbox
  
  tm_shape(Ps1) + tm_polygons() +
    tm_shape(stationdata_sp) +
    tm_dots(col="lam.hat", palette = "RdBu", auto.palette.mapping = FALSE,
            title=paste0("Predicted ",i," abund \n(per station)"), size=0.7) +
    tm_legend(legend.outside=TRUE)
  
  #IDW Interpolation
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
  names(grd)       <- c("EASTING", "NORTHING")
  coordinates(grd) <- c("EASTING", "NORTHING")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(stationdata_sp)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(lam.hat ~ 1, stationdata_sp, newdata=grd, idp=2.0)
  
  # Convert to raster object 
  r <- raster(P.idw)
  #plot(r)
  
  # Plot
  GG2<-tm_shape(r) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title=paste0("Predicted ",i," \n abundance \n (best model when \n comparing/combining \n layers)")) + 
    tm_shape(stationdata_sp) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  tiff(paste0('3_outputs/figures/BestModelPredicted',i,'.tiff'), units="in", width=6, height=4.5, res=75)
  GG2 
  dev.off()
  tmap_save(GG2, width = 1000, height = 1000, units="px", filename = paste0("3_outputs/maps/BestModelPredicted",i,".png"))
  
}


#Top AVI model for REVI
topavi<-pcount(~Julian ~Whitebirch_500 +Whitespruce_500+Tremblingaspen_500, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topavi, "m")#0.86 (marginal)
chat(topavi, "s")#0.96 (site-sum)
chat(topavi, "o")#1.42 (observation)

residcov(topavi, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topavi, type="m") # (marginal)
residfit(topavi, "s") # (site-sum)
residfit(topavi, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topavi, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topavi, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top LIDAR model for LCSP
toplidar<-pcount(~1 ~ S150.sd_PercCanopyDensity + S150.VPD__10.11m_Grid.1x1m.mean + S150.VPD__150.200cm_Grid.1x1m.mean, umf)
#An estimate of overdispersion relative to the fitted model.
chat(toplidar, "m")#1.2 (marginal)
chat(toplidar, "s")#1.50 (site-sum)
chat(toplidar, "o")#1.06 (observation)

residcov(toplidar, xlim=c(-10,10))

#plot residuals against fitted values
residfit(toplidar, type="m") # (marginal)
residfit(toplidar, "s") # (site-sum)
residfit(toplidar, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(toplidar, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(toplidar, type = "o", main = "Residual qq plot",
        plotLine = TRUE)

#Top Beaudoin model for REVI
topbeaudoin<-pcount(~Julian ~ LandCover_NonVeg_v1_50 + Structure_Stand_CrownClosure_v1_50, umf)
#An estimate of overdispersion relative to the fitted model.
chat(topbeaudoin, "m")#1.13 (marginal)
chat(topbeaudoin, "s")#1.56 (site-sum)
chat(topbeaudoin, "o")#1.37 (observation)

residcov(topbeaudoin, xlim=c(-10,10))

#plot residuals against fitted values
residfit(topbeaudoin, type="m") # (marginal)
residfit(topbeaudoin, "s") # (site-sum)
residfit(topbeaudoin, "o") #(observation)

#randomized quantile residuals vs. normal standardized quantiles 
residqq(topbeaudoin, type = "site-sum", main = "Residual qq plot",
        plotLine = TRUE)
residqq(topbeaudoin, type = "o", main = "Residual qq plot",
        plotLine = TRUE)


