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
Y<-alldata$DEJU

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
#Time difference of ~248 secs

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
# beta.(Intercept)         -2.1969  2.7623 0.035661       0.631601
# beta.vegcover.500.s       3.4119  2.7886 0.036000       0.600760
# beta.blackspruce.500.s   -3.1077  1.3159 0.016988       0.194608
# beta.age.500.s           -2.0084  3.3469 0.043208       0.847030
# beta.volume.500.s         0.6297  1.3606 0.017566       0.234650
# beta.crownclosure.500.s  -2.3743  2.1334 0.027542       0.346799
# beta.needleleaf.500.s     5.8184  2.8097 0.036273       0.655815
# gamma.(Intercept)        -1.6586  0.4062 0.005243       0.031399
# gamma.w1                  0.3151  0.1698 0.002191       0.005757
# Deviance                653.4311 56.8174 0.733509       4.673345
# 
# 2. Quantiles for each variable:
#   
#                              2.5%      25%      50%      75%    97.5%
# beta.(Intercept)         -8.01525  -3.8462  -1.9777  -0.2739   3.0357
# beta.vegcover.500.s      -2.01061   1.3707   3.2117   5.2917   9.6367
# beta.blackspruce.500.s   -5.82486  -4.0105  -3.0194  -2.1543  -0.7321
# beta.age.500.s           -7.59978  -4.5484  -2.0405   0.2983   5.0953
# beta.volume.500.s        -1.91133  -0.3223   0.6173   1.5539   3.3281
# beta.crownclosure.500.s  -6.51527  -3.9505  -2.2898  -0.7932   1.5555
# beta.needleleaf.500.s     0.07624   4.1116   5.7618   7.5951  11.2617
# gamma.(Intercept)        -2.73651  -1.8694  -1.6106  -1.3852  -1.0067
# gamma.w1                 -0.01914   0.2011   0.3156   0.4258   0.6469
# Deviance                565.92634 613.9192 645.6157 683.3339 806.2642

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
#intercept:7.867872e-01 
#vegcover:1.105660e-01 (11% probability of negative effect)
#blacksprucecover:9.909027e-01 (99% probability of negative effect) 
#standage:7.257747e-01 (73% probability of negative effect)
#standvolume:3.217484e-01 (32% probability of negative effect)
#crownclosure:8.671300e-01 (87% probability of negative effect)
#needleleafsppcover:1.918816e-02 (1.9% probability of negative effect)

params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:2.132128e-01 
#vegcover:8.894340e-01  (89% probability of positive effect)
#blacksprucecover:9.097334e-03 (0.9% probability of positive effect of stand volume) 
#standage:2.742253e-01  (27% probability of positive effect of stand volume)
#standvolume:6.782516e-01 (68% probability of positive effect of stand volume)
#crownclosure:1.328700e-01 (13% probability of positive effect of crown closure)
#needleleafsppcover:9.808118e-01 (98% probability of positive effect of conifer cover)

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture$lambda.latent)
#   Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
#0.4898  1.0393  1.4646  1.5236  1.9225  3.5371 

#50 % of stations are predicted to have <1.45 DEJU
#50 % of stations are predicted to have >1.45 DEJU

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture$delta.latent)
#50 % of stations have detection probability DEJU < 0.18
#50 % of stations have detection probability DEJU > 0.18
#maximum detection probability predicted to be 0.23

pdf(file="3_outputs/pdfs/DEJUPosteriors_hSDM.Nmixture.pdf")
plot(mod.hSDM.Nmixture$mcmc)
dev.off()