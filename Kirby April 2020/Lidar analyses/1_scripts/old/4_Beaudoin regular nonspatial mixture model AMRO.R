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
Y<-alldata$AMRO

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
#Time difference of ~143 secs

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
# beta.(Intercept)         -7.0522  4.2902 0.055387        1.66373
# beta.vegcover.500.s       8.8738  4.3587 0.056270        1.67828
# beta.blackspruce.500.s   -1.7428  1.6633 0.021474        0.27969
# beta.age.500.s           -2.1392  3.8414 0.049593        1.23055
# beta.volume.500.s        -2.5405  1.5373 0.019846        0.28450
# beta.crownclosure.500.s  -0.1046  1.9724 0.025463        0.32427
# beta.needleleaf.500.s     4.6627  2.2255 0.028731        0.41933
# gamma.(Intercept)        -2.4418  0.5948 0.007679        0.06784
# gamma.w1                 -0.3396  0.1871 0.002415        0.01020
# Deviance                522.9209 69.6169 0.898750        6.90091
# 
# 2. Quantiles for each variable:
#   
#                             2.5%      25%      50%      75%    97.5%
# beta.(Intercept)        -18.0222  -9.1304  -6.6301  -4.1069   0.3303
# beta.vegcover.500.s       1.3848   5.4989   8.7633  11.8840  18.1978
# beta.blackspruce.500.s   -4.7810  -2.9268  -1.7814  -0.5570   1.6888
# beta.age.500.s          -10.1059  -4.6799  -1.6242   0.8625   4.0657
# beta.volume.500.s        -5.4804  -3.5491  -2.6419  -1.5378   0.3996
# beta.crownclosure.500.s  -3.9213  -1.4829  -0.1310   1.3668   3.5979
# beta.needleleaf.500.s     0.0174   3.1110   4.8172   6.1266   9.0338
# gamma.(Intercept)        -3.8399  -2.7804  -2.3716  -2.0162  -1.4777
# gamma.w1                 -0.7202  -0.4656  -0.3384  -0.2134   0.0269
# Deviance                412.0107 471.2229 514.0623 562.9642 680.5486

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
#intercept:9.498887e-01 
#vegcover:2.088124e-02 (low (2%) probability of negative effect)
#blacksprucecover:8.526257e-01 (85% probability of negative effect) 
#standage:7.111945e-01 (71% probability of negative effect)
#standvolume:9.507947e-01 (95% probability of negative effect)
#crownclosure:5.211540e-01
#needleleafsppcover:1.807990e-02 (1.8% probability of negative effect)

params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:5.011128e-02 
#vegcover:9.791188e-01  (98% probability of positive effect)
#blacksprucecover:1.473743e-01 
#standage:2.888055e-01 
#standvolume:4.920528e-02 (5% probability of positive effect of stand volume)
#crownclosure:4.788460e-01
#needleleafsppcover:9.819201e-01 (98% probability of positive effect of conifer cover)

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture$lambda.latent)
#   Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
#0.05438 0.85051 1.47483 1.82409 2.12360 7.70215 

#50 % of stations are predicted to have <1.47 AMRO
#50 % of stations are predicted to have >1.47 AMRO

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture$delta.latent)
#50 % of stations have detection probability AMRO < 0.08
#50 % of stations have detection probability AMRO > 0.08
#maximum detection probability predicted to be 0.15

pdf(file="3_outputs/pdfs/AMROPosteriors_hSDM.Nmixture.pdf")
plot(mod.hSDM.Nmixture$mcmc)
dev.off()