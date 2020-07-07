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
Y<-alldata$OSFL

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
                               "mean_ht_500.s",
                               "sd_ht_500.s",
                               "mean_canopypc_500.s",
                               "sd_canopypc_500.s",
                               "mean_shrub.500.s")])      
str(stationdata)

mean_ht_500.s<-stationdata$mean_ht_500.s
sd_ht_500.s<-stationdata$sd_ht_500.s
mean_canopypc_500.s<-stationdata$mean_canopypc_500.s
sd_canopypc_500.s<-stationdata$sd_canopypc_500.s
mean_shrub.500.s<-stationdata$mean_shrub.500.s

#= Data-sets
data.obs <- data.frame(Y,w1,site=SS)#90 stations, up to 4 visits per station
data.suit <- data.frame(mean_ht_500.s,
                        sd_ht_500.s,
                        mean_canopypc_500.s,
                        sd_canopypc_500.s,
                        mean_shrub.500.s)#98 obs
#================================
#== Parameter inference with hSDM
Start <- Sys.time() # Start the clock
mod.hSDM.Nmixture <- hSDM.Nmixture(# Observations
  counts=data.obs$Y,
  observability=~w1,
  site=data.obs$site,
  data.observability=data.obs,
  # Habitat
  suitability=~mean_ht_500.s+sd_ht_500.s+mean_canopypc_500.s+sd_canopypc_500.s+mean_shrub.500.s,
  data.suitability=data.suit,
  # Predictions
  suitability.pred=NULL,
  # Chains
  burnin = 140000, mcmc = 150000, thin = 25,
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
#Time difference of ~120 secs

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
# beta.(Intercept)         -29.6119 19.1107 0.246718       11.92014
# beta.mean_ht_500.s         7.0076  6.6919 0.086392        1.83790
# beta.sd_ht_500.s          -2.0085  2.0866 0.026937        0.15672
# beta.mean_canopypc_500.s   0.7740  3.0344 0.039174        0.25760
# beta.sd_canopypc_500.s     5.9777  4.5067 0.058181        0.78144
# beta.mean_shrub.500.s     15.6365  9.5411 0.123175        4.33526
# gamma.(Intercept)         -0.9653  0.5423 0.007001        0.02642
# gamma.w1                  -0.3472  0.3402 0.004392        0.01103
# Deviance                 177.5538 27.7783 0.358617        1.42036
# 
# 2. Quantiles for each variable:
#   
#   2.5%      25%      50%      75%    97.5%
# beta.(Intercept)         -60.236 -46.4635 -31.0019 -12.6293   2.7133
# beta.mean_ht_500.s        -6.169   1.7692   7.5315  12.3161  18.1762
# beta.sd_ht_500.s          -6.945  -3.1997  -1.6416  -0.4882   1.0330
# beta.mean_canopypc_500.s  -5.058  -1.2784   0.7211   2.7778   6.7650
# beta.sd_canopypc_500.s    -2.461   2.5917   5.8768   9.5421  13.7711
# beta.mean_shrub.500.s     -1.261   7.8844  16.2193  23.5862  31.6344
# gamma.(Intercept)         -2.317  -1.2326  -0.9004  -0.6009  -0.1231
# gamma.w1                  -1.029  -0.5725  -0.3418  -0.1123   0.3011
# Deviance                 144.466 159.3920 171.3771 188.0084 253.7128

####NOW ADD SPATIAL COMPONENT

# ----------------------------------------------------------------------------
# Create a shapefile from a data frame of only the points you will use in analysis
# ----------------------------------------------------------------------------
#############################
####Generate neighborhood####
#############################
library(spdep)
library(maptools)
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
library(raster)
library(reshape2)
kirbypoints4shp<-read.csv("0_data/processed/birdsXY.hab.csv", header=TRUE)
#just the points with both bird and habitat data, sorted by SS
#actual bird and habitat data will be taken from other data frames

gridlocs<-kirbypoints4shp[,c("ID","SS","EASTING","NORTHING")]
coordinates(gridlocs)=~EASTING+NORTHING #column names of coords
proj4string(gridlocs)<- CRS("+init=epsg:32748") # set coordinate system to UTMs
gridlocs.df <- SpatialPointsDataFrame(gridlocs, data.frame(id=as.numeric(gridlocs$SS)))
writeOGR(gridlocs.df, dsn=paste0(getwd(),"/0_data/processed"),layer="KirbyShapefile",driver="ESRI Shapefile")

points<-readOGR(dsn=paste0(getwd(),"/0_data/processed"), layer="KirbyShapefile")
class(points)
head(points)

coords<-coordinates(points)
plot(points)

#for Olive-sided Flycatcher model: 4000-m neighborhood 
nb_4000<-dnearneigh(points, d1=0, d2=4000, longlat = NULL, row.names=points$id)

# Create the 4000-meter neighborhood matrix
nb_4000_wb<-nb2mat(nb_4000, style="B", zero.policy=TRUE)#98 rows X 98 columns for 98 ARU stations
row.names(nb_4000_wb)
write.csv(nb_4000_wb, file="0_data/processed/nb_4000_wb.csv")  #data check

nb_4000_wb<-read.csv("0_data/processed/nb_4000_wb.csv", header=TRUE)
str(nb_4000_wb)
nb_4000_wb$numneighbors<-rowSums(nb_4000_wb[,2:99])
min(nb_4000_wb$numneighbors)#39
#There should be at least 1 neighbor for each stations within
#its 4000-m neighborhood for the spatial model to work
#in this analysis, every station has at least 39 neighbors

#now the 1's in the weight matrix should be converted to the siteID names,
#so that the hSDM package knows which stations are neighbours for each point
#count

x<-nb_4000_wb[,2:99]
#From https://stackoverflow.com/questions/36295412/replace-some-variable-values-with-variable-names:
#convert the columns of 'x' to character class from factor and use Map to replace 1 in each column with the corresponding column name.
x[] <- lapply(x, as.character)
x[] <- Map(function(y,z) replace(y, y==1, z), x, colnames(x))
x[] <- Map(function(y,z) replace(y, y==0, z), x, "")

#need a "Find and Replace" within the data frame (because I can't fully load CSV file) to get rid of the V's
#then need to drop the zeros in each row
#then cbind node_CAR, numneighbours, and x to create graph file
id<-rownames(nb_4000_wb)
numneighbors<-nb_4000_wb$numneighbors
x.df<-data.frame(id,numneighbors,x)
write.csv(x.df, file="0_data/processed/neighborsIDed4000.csv")
#V's and blanks in data removed in Excel.Header removed


#= Data-sets
data.obs <- data.frame(w1)#1095

graph<-read.csv("0_data/processed/neighborsIDed4000.csv",header=F)
str(graph)#98 obs. of 98 variables, up to 96 neighbours at a station
forvector<-graph[,3:98]
names(forvector)<-NULL
forvector.u<-as.vector(t(forvector))
d <- forvector.u[!is.na(forvector.u)]

siteID<-as.numeric(SS)
Start <- Sys.time() # Start the clock
mod.hSDM.Nmixture.iCAR <-hSDM.Nmixture.iCAR(# Observations
  Y, ~w1, siteID, data.observability = data.obs,
  # Habitat
  ~mean_ht_500.s+sd_ht_500.s+mean_canopypc_500.s+sd_canopypc_500.s+mean_shrub.500.s,
  data.suitability=data.suit,
  # Spatial structure
  spatial.entity=graph[,1],
  n.neighbors=graph[,2], 
  neighbors=c(d),
  # Predictions
  suitability.pred = NULL, spatial.entity.pred = NULL,
  # Chains
  burnin = 140000, mcmc = 150000, thin = 25,
  # Starting values
  beta.start=0,
  gamma.start=0,
  Vrho.start=1,
  # Priors
  mubeta = 0, Vbeta = 1.0E6,
  mugamma = 0, Vgamma = 1.0E6,
  priorVrho = "1/Gamma",
  shape = 0.5, rate = 0.0005,
  Vrho.max = 1000,
  # Various
  seed = 1234, verbose = 1,
  save.rho = 0, save.p = 0, save.N = 0)
Time.hSDM <- difftime(Sys.time(),Start,units="sec") # Time difference
#= Computation time
Time.hSDM
#Time difference of ~132 secs

#== Outputs
#= Parameter estimates
summary(mod.hSDM.Nmixture.iCAR$mcmc)
#Iterations = 140001:289976
# Thinning interval = 25 
# Number of chains = 1 
# Sample size per chain = 6000 
# 
#1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#                             Mean      SD Naive SE Time-series SE
# beta.(Intercept)         -1.141e+02 5.558e+01 7.175e-01      3.312e+01
# beta.mean_ht_500.s       -1.212e+02 1.702e+02 2.198e+00      1.346e+02
# beta.sd_ht_500.s         -5.597e+01 5.326e+01 6.876e-01      4.042e+01
# beta.mean_canopypc_500.s  3.645e+01 3.131e+01 4.042e-01      1.947e+01
# beta.sd_canopypc_500.s    4.427e+01 4.319e+01 5.576e-01      3.110e+01
# beta.mean_shrub.500.s     7.839e+01 5.103e+01 6.588e-01      4.350e+01
# gamma.(Intercept)        -1.945e+00 7.495e-01 9.676e-03      7.338e-02
# gamma.w1                 -1.637e-01 4.256e-01 5.495e-03      2.213e-02
# Vrho                      4.561e+05 7.517e+05 9.704e+03      3.470e+05
# Deviance                  1.695e+02 6.364e+01 8.215e-01      2.645e+00
# 
# 2. Quantiles for each variable:
#   
#   2.5%       25%        50%        75%      97.5%
# beta.(Intercept)         -234.23795 -153.9994  -118.3451 -5.973e+01 -4.663e+01
# beta.mean_ht_500.s       -476.04657 -241.3251   -20.5053  1.252e+01  2.064e+01
# beta.sd_ht_500.s         -134.78613 -111.0860   -39.9217 -3.429e+00  4.605e-01
# beta.mean_canopypc_500.s   -2.95428    6.1095    29.4030  6.644e+01  9.066e+01
# beta.sd_canopypc_500.s      0.06029   10.2615    25.3932  7.643e+01  1.388e+02
# beta.mean_shrub.500.s      23.85881   31.6624    65.9260  1.142e+02  1.708e+02
# gamma.(Intercept)          -3.43888   -2.4593    -1.9551 -1.403e+00 -5.445e-01
# gamma.w1                   -1.02393   -0.4344    -0.1515  1.231e-01  6.724e-01
# Vrho                       12.08487  208.2537 24513.7309  6.731e+05  2.646e+06
# Deviance                  118.93604  139.6003   157.4467  1.810e+02  2.927e+02

# OSFL abundance (-) related to variation in canopy cover (more than in nonspatial model)
# OSFL abundance (+) related to shrub cover (more than in nonspatial model)

## check if posterior distributions of betas and gammas are normally distributed
g = mod.hSDM.Nmixture.iCAR$mcmc[,2]#e.g. veg cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="veg cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture.iCAR$mcmc[,3]#e.g. black spruce cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="black spruce cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture.iCAR$mcmc[,4]#e.g. stand age
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand age",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture.iCAR$mcmc[,5]#stand volume
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand volume",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture.iCAR$mcmc[,6]#crown closure
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="crown closure",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture.iCAR$mcmc[,7]#needleleaf spp cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="needleleaf spp cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


library(coda)
params= data.frame(parameter=colnames(mod.hSDM.Nmixture.iCAR$mcmc),
                   mean=summary(mod.hSDM.Nmixture.iCAR$mcmc)$statistics[,"Mean"],
                   sd=summary(mod.hSDM.Nmixture.iCAR$mcmc)$statistics[,"SD"],
                   median=summary(mod.hSDM.Nmixture.iCAR$mcmc)$quantiles[,"50%"],
                   HPDinterval(mcmc(as.matrix(mod.hSDM.Nmixture.iCAR$mcmc))),
                   RejectionRate=rejectionRate(mod.hSDM.Nmixture.iCAR$mcmc),
                   ModelName= "Beaudoin habitat")

# save model parameters
params1= params

#assign probability of betas being < 0 
params1$probabilitymin <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=T)
#intercept:0.98 
#mean_ht:0.76 
#sd_ht:0.85 
#mean_canopypc:0.12 
#sd_canopypc:0.15
#shrubcover:0.06
 
params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:0.02 
#mean_ht:0.24 
#sd_ht:0.15 
#mean_canopypc:0.88 
#sd_canopypc:0.85
#shrubcover:0.94

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture.iCAR$lambda.latent)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000398  0.018276  0.052959  0.777175  0.108488 14.577537

#50 % of stations are predicted to have <0.05 OSFL
#50 % of stations are predicted to have >0.05 OSFL

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture.iCAR$delta.latent)
#50 % of stations have detection probability OSFL < 0.14
#50 % of stations have detection probability OSFL > 0.14
#maximum detection probability predicted to be 0.21


mu<-mean(mod.hSDM.Nmixture.iCAR$rho.pred)
va<-var(mod.hSDM.Nmixture.iCAR$rho.pred)
mu-(1.96*sqrt(va)/sqrt(90))#-6.11
mu+(1.96*sqrt(va)/sqrt(90))#6.11
#mostly similar results to nonspatial model: 
#Vrho is positive, but rho.pred is a mix of negative and positive
#values, suggesting little or no spatial autocorrelation
pdf(file="3_outputs/pdfs/OSFLPosteriors_hSDM.Nmixture.iCAR.pdf")
plot(mod.hSDM.Nmixture.iCAR$mcmc)
dev.off()

#= Predictions
plot(stationdata$crownclosure.500.s,
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
coords = matrix(c(488000, 6131500,
                  488000, 6140000,
                  496000, 6140000,
                  496000, 6131500,
                  488000, 6131500), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +units=m +no_defs"))
plot(Ps1, axes = TRUE)
plot(stationdata_sp, add=TRUE, pch=20, col=rbPal(10)[as.numeric(cut(stationdata$lambda.pred, breaks = 10))])
stationdata_sp@bbox<-Ps1@bbox

tm_shape(Ps1) + tm_polygons() +
  tm_shape(stationdata_sp) +
  tm_dots(col="lambda.pred", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Predicted OSFL abund \n(per station)", size=0.7) +
  tm_legend(legend.outside=TRUE)

#IDW Interpolation - Need to work the bugs out
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
stationdata_sp <- merge(kirbypoints4shp, stationdata, by.x = c("SS","ID"), by.y = c("SS","ID"))
grd              <- as.data.frame(spsample(stationdata_sp, "regular", n=50000))
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
            title="Predicted OSFL \n abundance") + 
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
