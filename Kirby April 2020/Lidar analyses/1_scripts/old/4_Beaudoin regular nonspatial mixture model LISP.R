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
Y<-alldata$LISP

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
data.obs <- data.frame(Y,w1,site=SS)#98 stations, up to 4 visits per station
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
#Time difference of ~155 secs

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
# beta.(Intercept)          0.8531  1.0404 0.013431       0.166708
# beta.vegcover.500.s       1.4853  2.2250 0.028724       0.681645
# beta.blackspruce.500.s    0.3720  2.2589 0.029162       0.806860
# beta.age.500.s           -0.9071  1.8905 0.024407       0.440775
# beta.volume.500.s         0.4093  1.9440 0.025097       0.691057
# beta.crownclosure.500.s  -5.4164  2.7326 0.035277       0.886241
# beta.needleleaf.500.s     4.1876  2.7378 0.035344       1.001987
# gamma.(Intercept)        -2.7511  0.8127 0.010492       0.220073
# gamma.w1                 -0.4621  0.1277 0.001649       0.004611
# Deviance                803.0237 97.7385 1.261799      25.036737
# 
# 2. Quantiles for each variable:
#   
#   2.5%       25%       50%      75%     97.5%
# beta.(Intercept)         -1.5261   0.23548   0.97933   1.5320    2.6165
# beta.vegcover.500.s      -2.9565  -0.04483   1.87004   3.3268    4.8507
# beta.blackspruce.500.s   -3.1867  -1.45952  -0.06681   2.4250    4.4010
# beta.age.500.s           -4.5653  -2.10199  -0.80620   0.2433    3.1808
# beta.volume.500.s        -3.1115  -1.27917   0.54761   2.1552    3.6559
# beta.crownclosure.500.s  -9.5824  -7.79553  -5.56513  -3.0844   -0.2653
# beta.needleleaf.500.s    -0.3307   1.40007   4.76449   6.4525    8.3693
# gamma.(Intercept)        -4.4223  -3.29403  -2.48467  -2.1447   -1.6730
# gamma.w1                 -0.7189  -0.54532  -0.45767  -0.3769   -0.2203
# Deviance                670.5122 729.02103 773.70104 871.0646 1008.2223

## check if posterior distributions of betas and gammas are normally distributed
g = mod.hSDM.Nmixture$mcmc[,2]#e.g. veg cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="veg cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,3]#e.g. black spruce cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="black spruce cover",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,4]#e.g. stand age
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand age",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,5]#stand volume
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="stand volume",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,6]#crown closure
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="crown closure",  
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = mod.hSDM.Nmixture$mcmc[,7]#needleleaf spp cover
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=30, breaks=20, prob=TRUE, 
     xlab="needleleaf spp cover",  
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
                   ModelName= "Beaudoin habitat")

# save model parameters
params1= params

#assign probability of betas being < 0 
params1$probabilitymin <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=T)
#intercept:2.061089e-01
#vegcover:2.522114e-01 (25% probability of negative effect)
#blacksprucecover:4.345962e-01 (43% probability of negative effect) 
#standage:6.843145e-01 (68% probability of negative effect)
#standvolume:4.166214e-01 (42% probability of negative effect)
#crownclosure:9.762696e-01 (98% probability of negative effect)
#needleleafsppcover:6.306093e-02 (63% probability of negative effect)

params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:0.7938911354 
#vegcover:0.7477885829 (75% probability of positive effect)
#blacksprucecover:0.5654038371 (56% probability of positive effect of stand volume) 
#standage:0.3156855297  (32% probability of positive effect of stand volume)
#standvolume:0.5833786083 (58% probability of positive effect of stand volume)
#crownclosure:0.0237304084 (2% probability of positive effect of crown closure)
#needleleafsppcover:0.9369390664 (94% probability of positive effect of conifer cover)

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture$lambda.latent)
# Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
#0.2018  2.3847  4.1259  5.8463  8.4843 22.8790  

#50 % of stations are predicted to have <4.1 LISP
#50 % of stations are predicted to have >4.1 LISP

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture$delta.latent)
#50 % of stations have detection probability LISP < 0.07
#50 % of stations have detection probability LISP > 0.07
#maximum detection probability predicted to be 0.15

pdf(file="3_outputs/pdfs/LISPPosteriors_hSDM.Nmixture.pdf")
plot(mod.hSDM.Nmixture$mcmc)
dev.off()