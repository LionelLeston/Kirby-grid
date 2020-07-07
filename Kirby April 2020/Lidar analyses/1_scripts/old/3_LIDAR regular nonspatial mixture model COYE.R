#Examine this github page for how to compare multiple models in hSDM using
#the Deviance Information Criterion
#https://github.com/adammwilson/SpatialAnalysisTutorials/blob/master/SDM_intro2/hSDM_intro.md


#== Load libraries
library(hSDM)
library(lubridate)

####SPATIAL MIXTURE MODEL EXAMPLE####
kirbypoints4shp<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)
#just the points with both bird and habitat data, sorted by SS
#the order of the stations in "SS" must be the same in the
#individual visits (birddata), the habitat data (kirbypoints4shp), 
#and the points used to create the graph file accounting for spatial 
#correlation later (kirbypoints4shp again)

birddata<-read.csv(file="0_data/raw/allvisitsBG15.csv", header=TRUE)      
str(birddata)
#make sure the data used doesn't include any stations missing habitat data
#and is sorted by station the same way as the habitat data and the station
#location data was when creating the spatial matrix

#Since we are using mixture models, there should be multiple
#rows (observations) for each station. In contrast to the unmarked
#package, observation and detection covariate data are in long
#format rather than wide format, when modelling with the HSDM package
alldata<-merge(kirbypoints4shp, birddata, by=c("SS"))
write.csv(alldata, file="0_data/processed/alldata.csv")

alldata<-read.csv("0_data/processed/alldata.csv", header=TRUE)
SS<-alldata$X
alldata$ID<-alldata$X
Y<-alldata$COYE

alldata$MY.DATE1 <- do.call(paste, list(alldata$Month, alldata$Day, alldata$Year))
alldata$MY.DATE1 <- as.Date(alldata$MY.DATE1, format=c("%m %d %Y"))
alldata$Julian <- as.numeric(format(alldata$MY.DATE1, "%j"))

w1<-scale(alldata$Julian, center=TRUE, scale=TRUE)

alldata$mean_ht_50.s<-scale(alldata$mean_MeanHeight_50, scale=TRUE, center=FALSE)
alldata$mean_ht_150.s<-scale(alldata$mean_MeanHeight_150, scale=TRUE, center=FALSE)
alldata$mean_ht_500.s<-scale(alldata$mean_MeanHeight_500, scale=TRUE, center=FALSE)

alldata$sd_ht_50.s<-scale(alldata$sd_MeanHeight_50, scale=TRUE, center=FALSE)
alldata$sd_ht_150.s<-scale(alldata$sd_MeanHeight_150, scale=TRUE, center=FALSE)
alldata$sd_ht_500.s<-scale(alldata$sd_MeanHeight_500, scale=TRUE, center=FALSE)

alldata$mean_canopypc_50.s<-scale(alldata$mean_PercCanopyDensity_50, scale=TRUE, center=FALSE)
alldata$mean_canopypc_150.s<-scale(alldata$mean_PercCanopyDensity_150, scale=TRUE, center=FALSE)
alldata$mean_canopypc_500.s<-scale(alldata$mean_PercCanopyDensity_500, scale=TRUE, center=FALSE)

alldata$sd_canopypc_50.s<-scale(alldata$sd_PercCanopyDensity_50, scale=TRUE, center=FALSE)
alldata$sd_canopypc_150.s<-scale(alldata$sd_PercCanopyDensity_150, scale=TRUE, center=FALSE)
alldata$sd_canopypc_500.s<-scale(alldata$sd_PercCanopyDensity_500, scale=TRUE, center=FALSE)

alldata$mean_shrub.50.s<-scale((alldata$strata_00_to_02_returnProportion_50+alldata$strata_02_to_04_returnProportion_50), scale=TRUE, center=FALSE)
alldata$mean_shrub.150.s<-scale((alldata$strata_00_to_02_returnProportion_150+alldata$strata_02_to_04_returnProportion_150), scale=TRUE, center=FALSE)
alldata$mean_shrub.500.s<-scale((alldata$strata_00_to_02_returnProportion_500+alldata$strata_02_to_04_returnProportion_500), scale=TRUE, center=FALSE)

#alldata$sd_shrub.50.s<-scale(alldata$sd_PercCanopyCoverCut1.0m_50, scale=TRUE, center=FALSE)
#alldata$sd_shrub.150.s<-scale(alldata$sd_PercCanopyCoverCut1.0m_150, scale=TRUE, center=FALSE)
#alldata$sd_shrub.500.s<-scale(alldata$sd_PercCanopyCoverCut1.0m_500, scale=TRUE, center=FALSE)


stationdata<-unique(alldata[,c("ID",
                               "SS",
                               "mean_ht_150.s",
                               "sd_ht_150.s",
                               "mean_canopypc_150.s",
                               "sd_canopypc_150.s",
                               "mean_shrub.150.s")])      
str(stationdata)

mean_ht_150.s<-stationdata$mean_ht_150.s
sd_ht_150.s<-stationdata$sd_ht_150.s
mean_canopypc_150.s<-stationdata$mean_canopypc_150.s
sd_canopypc_150.s<-stationdata$sd_canopypc_150.s
mean_shrub.150.s<-stationdata$mean_shrub.150.s

#= Data-sets
data.obs <- data.frame(Y,w1,site=SS)#90 stations, up to 4 visits per station
data.suit <- data.frame(mean_ht_150.s,
                        sd_ht_150.s,
                        mean_canopypc_150.s,
                        sd_canopypc_150.s,
                        mean_shrub.150.s)#98 obs
#================================
#== Parameter inference with hSDM
Start <- Sys.time() # Start the clock
mod.hSDM.Nmixture <- hSDM.Nmixture(# Observations
  counts=data.obs$Y,
  observability=~w1,
  site=data.obs$site,
  data.observability=data.obs,
  # Habitat
  suitability=~mean_ht_150.s+sd_ht_150.s+mean_canopypc_150.s+sd_canopypc_150.s+mean_shrub.150.s,
  data.suitability=data.suit,
  # Predictions
  suitability.pred=NULL,
  # Chains
  burnin = 140000, mcmc = 115000, thin = 25,
  # Starting values
  beta.start=0,
  gamma.start=0,
  # Priors
  mubeta=0, Vbeta=1.0E6,
  mugamma=0, Vgamma=1.0E6,
  # Various
  seed=1234, verbose=1,
  save.p=0, save.N=1)
Time.hSDM <- difftime(Sys.time(),Start,units="sec") # Time difference
#= Computation time
Time.hSDM
#Time difference of ~176 secs

#== Outputs
#= Parameter estimates
summary(mod.hSDM.Nmixture$mcmc)
#Iterations = 140001:289976
# Thinning interval = 25 
# Number of chains = 1 
# Sample size per chain = 6000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#                             Mean      SD Naive SE Time-series SE
#beta.(Intercept)           28.432   7.911  0.11664         2.2455
# beta.mean_ht_150.s        -33.158   6.549  0.09656         1.4143
# beta.sd_ht_150.s            8.507   2.316  0.03415         0.1768
# beta.mean_canopypc_150.s    9.565   3.128  0.04612         0.4357
# beta.sd_canopypc_150.s     -2.374   2.545  0.03753         0.2943
# beta.mean_shrub.150.s     -22.486   5.398  0.07959         1.4413
# gamma.(Intercept)        1085.163 582.060  8.58201        10.6479
# gamma.w1                 -323.368 589.610  8.69332        10.4010
# Deviance                  138.691   7.555  0.11140         0.2324
# 
# 2. Quantiles for each variable:
#   
#                               2.5%      25%      50%       75%    97.5%
# beta.(Intercept)            14.113   22.756   28.631   33.7398   44.027
# beta.mean_ht_150.s         -44.553  -38.343  -33.348  -28.1383  -20.607
# beta.sd_ht_150.s             4.084    6.954    8.413   10.0934   13.260
# beta.mean_canopypc_150.s     4.315    7.353    9.307   11.4341   16.706
# beta.sd_canopypc_150.s      -7.191   -4.230   -2.344   -0.6031    2.567
# beta.mean_shrub.150.s      -31.839  -26.681  -22.658  -18.0304  -11.961
# gamma.(Intercept)          170.975  648.005 1004.648 1431.6789 2426.997
# gamma.w1                 -1507.433 -714.717 -300.730   70.3699  806.546
# Deviance                   130.835  133.590  136.279  141.1723  160.314

# COYE abundance (+) related to increasing canopy cover and variation in canopy height and 
#(-) related to increasing canopy height and increasing shrub cover

## check if posterior distributions of betas and gammas are normally distributed
g = mod.hSDM.Nmixture$mcmc[,2]#e.g. canopy height mean
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="veg cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,3]#e.g. canopy height sd
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="black spruce cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,4]#e.g. canopy cover mean
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand age",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,5]#canopy cover sd
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand volume",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,6]#shrub cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="crown closure",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


library(coda)
params= data.frame(parameter=colnames(mod.hSDM.Nmixture$mcmc),
                   mean=summary(mod.hSDM.Nmixture$mcmc)$statistics[,"Mean"],
                   sd=summary(mod.hSDM.Nmixture$mcmc)$statistics[,"SD"],
                   median=summary(mod.hSDM.Nmixture$mcmc)$quantiles[,"50%"],
                   HPDinterval(mcmc(as.matrix(mod.hSDM.Nmixture$mcmc))),
                   RejectionRate=rejectionRate(mod.hSDM.Nmixture$mcmc),
                   ModelName= "Lidar habitat")

# save model parameters
params1= params

#assign probability of betas being < 0 
params1$probabilitymin <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=T)
#intercept:1.627027e-04 
#mean_ht:0.99 
#sd_ht:1.198863e-04
#mean_canopypc:1.115080e-03
#sd_canopypc:8.244920e-01
#shrubcover:0.99
 
params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:0.99 
#mean_ht:2.061562e-07 
#sd_ht:0.99 
#mean_canopypc:0.99 
#sd_canopypc:1.755080e-01
#shrubcover:1.554064e-05

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture$lambda.latent)
#    Min.   1st Qu.  Median     Mean   3rd Qu.    Max. 
#0.0000000 0.0002369 0.0235314 0.0584983 0.0682517 0.6168624 

#50 % of stations are predicted to have <0.02 COYE
#50 % of stations are predicted to have >0.02 COYE

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture$delta.latent)
#50 % of stations have detection probability COYE < 1
#50 % of stations have detection probability COYE > 1
#maximum detection probability predicted to be 1

pdf(file="3_outputs/pdfs/COYEPosteriors_hSDM.Nmixture.pdf")
plot(mod.hSDM.Nmixture$mcmc)
dev.off()

#= Predictions
plot(stationdata$crownclosure.150.s,
     mod.hSDM.Nmixture.iCAR$lambda.pred, 
     xlab="Amount of Seismic", 
     ylab="Predicted Olive-sided Flycatcher Abundance", 
     pch=20, col="blue")#possible nonlinear effect


#Generate prediction surface: add predicted values to spdf "points"
stationdata$lambda.pred<-mod.hSDM.Nmixture$lambda.pred
stationdata$id<-as.numeric(stationdata$SS)

BG15<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)
LCC <- CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs")
coordinates(BG15) <- BG15[,c("EASTING", "NORTHING")] 
proj4string(BG15) <- LCC

stationdata_sp <- merge(BG15, stationdata, by.x = "SS", by.y = "SS")
str(stationdata_sp)
plot(stationdata_sp, col=stationdata$lambda.pred*10)
stationdata_sp@bbox
stationdata_sp@coords

library(tmap)

# kirby <- raster("E:/BERA Mentoring/Kirby Oct 2019/Kirby_CWD_Vol.tif")
# plot(kirby)
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','green'))
plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata$lambda.pred, breaks = 10))])

# Replace point boundary extent with that of Kirby grid
coords = matrix(c(488000, 6131150,
                  488000, 6140000,
                  496000, 6140000,
                  496000, 6131150,
                  488000, 6131150), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
plot(Ps1, axes = TRUE)
plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata$lambda.pred, breaks = 10))])
stationdata_sp@bbox<-Ps1@bbox

tm_shape(Ps1) + tm_polygons() +
  tm_shape(stationdata_sp) +
  tm_dots(col="lambda.pred", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Predicted COYE abund \n(per station)", size=0.7) +
  tm_legend(legend.outside=TRUE)

#IDW Interpolation - Need to work the bugs out
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
stationdata_sp <- merge(kirbypoints4shp, stationdata, by.x = c("SS","ID"), by.y = c("SS","ID"))
grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=15000))
names(grd)       <- c("EASTING", "NORTHING")
coordinates(grd) <- c("EASTING", "NORTHING")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(stationdata_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(lambda.pred ~ 1, stationdata_sp, newdata=grd, idp=2.0)

# Convert to raster object 
r       <- raster(P.idw)
plot(r)

# Plot
tm_shape(r) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted COYE \n abundance") + 
  tm_shape(stationdata_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

summary(mod.hSDM.Nmixture.iCAR$lambda.latent)
summary(mod.hSDM.Nmixture.iCAR$delta.latent)
summary(mod.hSDM.Nmixture.iCAR$lambda.pred)
pdf(file="Pred-Init.iCAR.pdf")
plot(ymax.station$WTSPmax,
     mod.hSDM.Nmixture$lambda.pred,
     xlab="Maximum WTSP count per station",
     ylab="Predicted WTSP abundance per station",
     pch=20, col="blue")
#replaced lambda with ymax.station$WTSPmax
abline(a=0,b=1,col="red")
dev.off()
#= MCMC for latent variable N
pdf(file="MCMC_N.iCAR.pdf")
plot(mod.hSDM.Nmixture.iCAR$N.pred)
dev.off()
#= Check that Ns are correctly estimated
M <- as.matrix(mod.hSDM.Nmixture.iCAR$N.pred)
N.est <- apply(M,2,mean)
Y.by.site <- tapply(Y,siteID,mean) # Mean by site
pdf(file="Check_N.pdf",width=10,height=5)
plot(Y.by.site, N.est) ## More individuals are expected (N > Y) due to detection process
#Error: x and y differ
abline(a=0,b=1,col="red")
dev.off()
## End(Not run)
