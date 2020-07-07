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

alldata$needleleaf.50.s<-scale(alldata$SpeciesGroups_Needleleaf_Spp_v1_50, scale=TRUE, center=FALSE)
alldata$needleleaf.150.s<-scale(alldata$SpeciesGroups_Needleleaf_Spp_v1_150, scale=TRUE, center=FALSE)
alldata$needleleaf.500.s<-scale(alldata$SpeciesGroups_Needleleaf_Spp_v1_500, scale=TRUE, center=FALSE)

alldata$blackspruce.50.s<-scale(alldata$Species_Pice_Mar_v1_50, scale=TRUE, center=FALSE)
alldata$blackspruce.150.s<-scale(alldata$Species_Pice_Mar_v1_150, scale=TRUE, center=FALSE)
alldata$blackspruce.500.s<-scale(alldata$Species_Pice_Mar_v1_500, scale=TRUE, center=FALSE)

alldata$volume.50.s<-scale(alldata$Structure_Volume_Total_v1_50, scale=TRUE, center=FALSE)
alldata$volume.150.s<-scale(alldata$Structure_Volume_Total_v1_150, scale=TRUE, center=FALSE)
alldata$volume.500.s<-scale(alldata$Structure_Volume_Total_v1_500, scale=TRUE, center=FALSE)

alldata$age.50.s<-scale(alldata$Structure_Stand_Age_v1_50, scale=TRUE, center=FALSE)
alldata$age.150.s<-scale(alldata$Structure_Stand_Age_v1_150, scale=TRUE, center=FALSE)
alldata$age.500.s<-scale(alldata$Structure_Stand_Age_v1_500, scale=TRUE, center=FALSE)

alldata$crownclosure.50.s<-scale(alldata$Structure_Stand_CrownClosure_v1_50, scale=TRUE, center=FALSE)
alldata$crownclosure.150.s<-scale(alldata$Structure_Stand_CrownClosure_v1_150, scale=TRUE, center=FALSE)
alldata$crownclosure.500.s<-scale(alldata$Structure_Stand_CrownClosure_v1_500, scale=TRUE, center=FALSE)

alldata$vegcover.50.s<-scale(alldata$LandCover_Veg_v1_50, scale=TRUE, center=FALSE)
alldata$vegcover.150.s<-scale(alldata$LandCover_Veg_v1_150, scale=TRUE, center=FALSE)
alldata$vegcover.500.s<-scale(alldata$LandCover_Veg_v1_500, scale=TRUE, center=FALSE)


stationdata<-unique(alldata[,c("ID",
                               "SS",
                               "vegcover.500.s",
                               "blackspruce.500.s",
                               "age.500.s",
                               "volume.500.s",
                               "crownclosure.500.s",
                               "needleleaf.500.s")])      
str(stationdata)

vegcover.500.s<-stationdata$vegcover.500.s
blackspruce.500.s<-stationdata$blackspruce.500.s
age.500.s<-stationdata$age.500.s
volume.500.s<-stationdata$volume.500.s
crownclosure.500.s<-stationdata$crownclosure.500.s
needleleaf.500.s<-stationdata$needleleaf.500.s

#= Data-sets
data.obs <- data.frame(Y,w1,site=SS)#90 stations, up to 4 visits per station
data.suit <- data.frame(vegcover.500.s,blackspruce.500.s,age.500.s,volume.500.s,crownclosure.500.s,needleleaf.500.s)#98 obs
#================================
#== Parameter inference with hSDM
Start <- Sys.time() # Start the clock
mod.hSDM.Nmixture <- hSDM.Nmixture(# Observations
  counts=data.obs$Y,
  observability=~w1,
  site=data.obs$site,
  data.observability=data.obs,
  # Habitat
  suitability=~vegcover.500.s+blackspruce.500.s+age.500.s+volume.500.s+crownclosure.500.s+needleleaf.500.s,
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
#Time difference of ~387 secs

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
# beta.(Intercept)          0.4759  2.1113 0.027256         0.2104
# beta.vegcover.500.s       8.4569  5.1611 0.066629         1.0776
# beta.blackspruce.500.s   -5.7219  5.1042 0.065895         1.0852
# beta.age.500.s            2.5764  5.2496 0.067772         0.9263
# beta.volume.500.s         3.7224  4.9621 0.064060         1.2091
# beta.crownclosure.500.s -16.4804  5.3668 0.069285         1.0843
# beta.needleleaf.500.s     4.0668  7.0914 0.091549         1.6302
# gamma.(Intercept)        -1.2874  0.8818 0.011384         0.1389
# gamma.w1                 -0.4490  0.3458 0.004464         0.0196
# Deviance                194.2519 52.1101 0.672739         9.1217
# 
# 2. Quantiles for each variable:
#   
#                            2.5%      25%      50%      75%    97.5%
# beta.(Intercept)         -3.921  -0.8166   0.5457   1.9038   4.5510
# beta.vegcover.500.s      -1.310   4.9851   8.2408  11.9230  19.2479
# beta.blackspruce.500.s  -16.657  -9.0231  -5.6446  -2.6060   5.4617
# beta.age.500.s           -9.050  -0.4522   2.6969   5.9143  12.4976
# beta.volume.500.s        -6.005   0.3934   3.3712   6.7287  14.3455
# beta.crownclosure.500.s -27.768 -19.5195 -16.5346 -12.8798  -5.9964
# beta.needleleaf.500.s    -8.392  -1.2556   3.2960   9.8884  17.1082
# gamma.(Intercept)        -3.869  -1.5460  -1.0898  -0.7300  -0.1820
# gamma.w1                 -1.153  -0.6784  -0.4358  -0.2075   0.1985
# Deviance                145.565 162.9305 177.9394 201.8909 361.8117
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
  ~vegcover.500.s+blackspruce.500.s+age.500.s+volume.500.s+crownclosure.500.s+needleleaf.500.s,
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
#Time difference of ~138 secs

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
# beta.(Intercept)         -0.2453   4.740 0.061192        0.97182
# beta.vegcover.500.s      15.5334   7.430 0.095925        1.96716
# beta.blackspruce.500.s   -1.3131   6.982 0.090134        2.00814
# beta.age.500.s           -7.0790  10.131 0.130794        3.90036
# beta.volume.500.s        11.3721   8.151 0.105227        2.89105
# beta.crownclosure.500.s -26.4805   9.457 0.122084        2.95913
# beta.needleleaf.500.s     4.7218   5.204 0.067185        1.25647
# gamma.(Intercept)        -2.2379   1.061 0.013699        0.18946
# gamma.w1                 -0.1175   0.421 0.005435        0.04034
# Vrho                    235.0901 323.366 4.174643       49.01954
# Deviance                205.2106  54.154 0.699122        7.53394
# 
# 2. Quantiles for each variable:
#   
#                             2.5%      25%      50%      75%     97.5%
# beta.(Intercept)        -12.7421  -2.3879   0.8438   3.0937    6.5379
# beta.vegcover.500.s       3.8255  10.1373  14.7158  19.6646   32.8068
# beta.blackspruce.500.s  -18.2705  -4.9798  -1.2852   2.5296   13.1397
# beta.age.500.s          -24.0901 -14.8137  -6.8304  -0.4272   14.1654
# beta.volume.500.s        -6.5139   6.8161  11.2958  16.6742   27.6794
# beta.crownclosure.500.s -47.3188 -32.5194 -25.8566 -19.8947   -8.7067
# beta.needleleaf.500.s    -5.1236   1.2819   4.2515   8.5869   14.9476
# gamma.(Intercept)        -4.0563  -3.1461  -2.2196  -1.3401   -0.4469
# gamma.w1                 -0.9673  -0.3828  -0.1126   0.1607    0.6888
# Vrho                      2.1960  29.8542 126.2240 302.0416 1157.9987
# Deviance                133.9424 163.7313 191.9237 237.5552  337.7278

# OSFL abundance (-) related to crown closure (as in nonspatial model)
# OSFL abundance (+) related to veg closure (more than in nonspatial model)
# OSFL abundance slightly (+) related to conifer cover (more than in nonspatial model)
# OSFL abundance slightly (-) related to stand age (more than in nonspatial model)

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
#intercept:5.206374e-01 
#vegcover:1.828486e-02 (very low probability of negative effect)
#blacksprucecover:5.745922e-01 
#standage:7.576374e-01 
#standvolume:8.147641e-02 (8% probability of negative effect)
#crownclosure:9.974466e-01 (very high probability of negative effect)
#needleleafsppcover:1.821175e-01 (18% probability of negative effect)
 
params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:0.479362597 
#vegcover:0.981715142 (very high probability of positive effect)
#blacksprucecover:0.425407824 
#standage:0.242362623 
#standvolume:0.918523586 (91% probability of positive effect of stand volume)
#crownclosure:0.002553392 (very low probability of positive effect)
#needleleafsppcover:0.817882485 (81% probability of positive effect of conifer cover)

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture.iCAR$lambda.latent)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000658  0.107497  0.298252  1.143296  0.592234 20.670950 

#50 % of stations are predicted to have <0.3 OSFL
#50 % of stations are predicted to have >0.3 OSFL

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture.iCAR$delta.latent)
#50 % of stations have detection probability OSFL < 0.13
#50 % of stations have detection probability OSFL > 0.13
#maximum detection probability predicted to be 0.18


mu<-mean(mod.hSDM.Nmixture.iCAR$rho.pred)
va<-var(mod.hSDM.Nmixture.iCAR$rho.pred)
mu-(1.96*sqrt(va)/sqrt(90))#-0.13
mu+(1.96*sqrt(va)/sqrt(90))#0.13
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
