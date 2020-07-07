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
Y<-alldata$CHSP

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
#Time difference of ~263 secs

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
# beta.(Intercept)          1.8838  1.1359 0.014665       0.207087
# beta.vegcover.500.s      -3.6881  1.7711 0.022864       0.368650
# beta.blackspruce.500.s   -2.3291  1.2948 0.016715       0.227828
# beta.age.500.s            0.5057  1.6749 0.021623       0.343163
# beta.volume.500.s        -2.6347  1.0695 0.013808       0.172356
# beta.crownclosure.500.s   1.6409  1.4653 0.018917       0.264330
# beta.needleleaf.500.s     5.5845  2.3010 0.029706       0.674284
# gamma.(Intercept)        -2.1240  0.5972 0.007709       0.093700
# gamma.w1                 -0.1039  0.1262 0.001629       0.006694
# Deviance                896.3539 80.0149 1.032989      11.601489
# 
# 2. Quantiles for each variable:
#   
#                             2.5%      25%      50%       75%     97.5%
# beta.(Intercept)         -0.3132   1.0795   1.8639   2.69940    3.9810
# beta.vegcover.500.s      -7.2335  -4.8592  -3.4800  -2.41118   -0.4600
# beta.blackspruce.500.s   -4.7518  -3.2750  -2.3748  -1.41067    0.1522
# beta.age.500.s           -3.3722  -0.3561   0.6549   1.61185    3.5045
# beta.volume.500.s        -4.6617  -3.3765  -2.6666  -1.84824   -0.6174
# beta.crownclosure.500.s  -1.2230   0.5759   1.7966   2.65718    4.3238
# beta.needleleaf.500.s     1.8157   3.6374   5.6575   7.42331    9.7429
# gamma.(Intercept)        -3.4249  -2.4373  -1.9741  -1.68895   -1.2552
# gamma.w1                 -0.3577  -0.1867  -0.1012  -0.01715    0.1369
# Deviance                776.8044 837.0366 878.6833 944.54145 1062.7363

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
#intercept:4.862026e-02 
#vegcover:9.813484e-01 (very high (98%) probability of negative effect)
#blacksprucecover:9.639827e-01 (96% probability of negative effect) 
#standage:3.813604e-01 (38% probability of negative effect)
#standvolume:9.931192e-01 (very high (99%) probability of negative effect)
#crownclosure:1.313857e-01
#needleleafsppcover:7.613097e-03 (0.8% probability of negative effect)

params1$probabilitymax <- pnorm(0, mean= params1$mean, sd=params1$sd, lower.tail=F)
#intercept:0.9513797428 
#vegcover:0.0186516271  (2% probability of positive effect)
#blacksprucecover:0.0360173266 (4% probability of positive effect of stand volume) 
#standage:0.6186395519  (62% probability of positive effect of stand volume)
#standvolume:0.0068808231 (0.6% probability of positive effect of stand volume)
#crownclosure:0.8686143396 (87% probability of positive effect of crown closure)
#needleleafsppcover:0.9923869032 (99% probability of positive effect of conifer cover)

##############################################################################################################
##############################################################################################################
##############################################################################################################


#= Predictions
#Distribution of predicted abundances
summary(mod.hSDM.Nmixture$lambda.latent)
#   Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
#0.6992  2.4338  3.6731  3.9338  5.3297  9.3185 

#50 % of stations are predicted to have <3.7 CHSP
#50 % of stations are predicted to have >3.7 CHSP

#Distribution of predicted detectability
summary(mod.hSDM.Nmixture$delta.latent)
#50 % of stations have detection probability CHSP < 0.12
#50 % of stations have detection probability CHSP > 0.12
#maximum detection probability predicted to be 0.14

pdf(file="3_outputs/pdfs/CHSPPosteriors_hSDM.Nmixture.pdf")
plot(mod.hSDM.Nmixture$mcmc)
dev.off()