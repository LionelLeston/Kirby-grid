library(lubridate)
library(unmarked)
library(MuMIn)
library(jpeg)
library(ggplot2)
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
site_lidar_cov_50m<-read.csv("0_data/raw/site_lidar_cov_50m.csv",header=TRUE)
site_lidar_cov_150m<-read.csv("0_data/raw/site_lidar_cov_150m.csv",header=TRUE)
site_lidar_cov_500m<-read.csv("0_data/raw/site_lidar_cov_500m.csv",header=TRUE)
VPD_mean_50m<-read.csv("0_data/raw/VPD_mean_50m.csv",header=TRUE)
VPD_mean_150m<-read.csv("0_data/raw/VPD_mean_150m.csv",header=TRUE)
VPD_mean_500m<-read.csv("0_data/raw/VPD_mean_500m.csv",header=TRUE)
VPD_sd_50m<-read.csv("0_data/raw/VPD_sd_50m.csv",header=TRUE)
VPD_sd_150m<-read.csv("0_data/raw/VPD_sd_150m.csv",header=TRUE)
VPD_sd_500m<-read.csv("0_data/raw/VPD_sd_500m.csv",header=TRUE)

sitecov.150<-cbind(site_lidar_cov_150m,VPD_mean_150m,VPD_sd_150m)
#sitecov.150<-merge(sitecov,VPD_sd_150m,by=c("SS"))
colnames(sitecov.150) <- gsub("1x1m.x", "1x1m.mean", colnames(sitecov.150))
colnames(sitecov.150) <- gsub("1x1m.y", "1x1m.sd", colnames(sitecov.150))
write.csv(sitecov.150, file="0_data/processed/unmarked/sitecov.150.csv")

sitecov.500<-cbind(site_lidar_cov_500m,VPD_mean_500m,VPD_sd_500m)
#sitecov.500<-merge(sitecov,VPD_sd_500m,by=c("SS"))
colnames(sitecov.500) <- gsub("1x1m.x", "1x1m.mean", colnames(sitecov.500))
colnames(sitecov.500) <- gsub("1x1m.y", "1x1m.sd", colnames(sitecov.500))
write.csv(sitecov.500, file="0_data/processed/unmarked/sitecov.500.csv")

WTSP<-read.csv("0_data/processed/unmarked/WTSP.csv",header=TRUE)
WTSPcount=WTSP[,2:5]
Julian<-read.csv("0_data/processed/unmarked/Julian.csv",header=TRUE)
ObsCov<-list(as.matrix(Julian[,2:5]))
sitecov.150<-read.csv("0_data/processed/unmarked/sitecov.150.csv",header=TRUE)
sitecov.500<-read.csv("0_data/processed/unmarked/sitecov.500.csv",header=TRUE)
#same variable names in both data frames

# Change names of columns 4 to N in sitecov.150
colnames(sitecov.150)[4:ncol(sitecov.150)] <- paste0("S150.",colnames(sitecov.150)[4:ncol(sitecov.150)])
names(sitecov.150)

# Change names of columns 4 to N in sitecov.500
colnames(sitecov.500)[4:ncol(sitecov.500)] <- paste0("S500.",colnames(sitecov.500)[4:ncol(sitecov.500)])
names(sitecov.500)

sitecov.150$X.1<-NULL
sitecov.150$X<-NULL
sitecov.500$X.1<-NULL
sitecov.500$X<-NULL

site.covs<-merge(sitecov.150,sitecov.500,by=c("SS"))
str(site.covs)

#3. Now construct unmarkedPCount frame for mixture models
umf <- unmarkedFramePCount(y=WTSPcount, siteCovs=site.covs, 
                           obsCovs=list(Julian=Julian[,2:5]))# organize data
umf                            # take a look
summary(umf)   

#to dredge with parallel processing, use this section
global_pcount <-pcount( ~ Julian 
                        ~S150.mean.VPD__0.1m_Grid.1x1m+
                          S150.mean.VPD__1.2m_Grid.1x1m+
                          S150.mean.VPD__2.3m_Grid.1x1m+
                          S150.mean.VPD__3.4m_Grid.1x1m+
                          S150.mean.VPD__4.5m_Grid.1x1m+
                          S150.mean_MaxHeight+
                          S150.mean_PercCanopyDensity+
                          S500.mean.VPD__0.1m_Grid.1x1m+
                          S500.mean.VPD__1.2m_Grid.1x1m+
                          S500.mean.VPD__2.3m_Grid.1x1m+
                          S500.mean.VPD__3.4m_Grid.1x1m+
                          S500.mean.VPD__4.5m_Grid.1x1m+
                          S500.mean_MaxHeight+
                          S500.mean_PercCanopyDensity, 
                        mixture="P", 
                        data = umf)

#parallel processing and using dredge
clusterType<-if(length(find.package("snow",
                                    quiet=TRUE))) "SOCK" else "PSOCK"

clust<-try(makeCluster(getOption("cl.cores",4), type=clusterType))
clusterEvalQ(clust, library(unmarked))
clusterExport(clust, "umf")

system.time(dredge_pcount<-pdredge(global_pcount, rank=AIC, m.lim=c(1,6), cluster=clust, fixed=c("p(Julian)")))
dredge_pcount
length(dredge_pcount)
nrow(dredge_pcount)#638 models
bestmodels<-get.models(dredge_pcount, subset=delta<4)

ms<-modSel(fitList(fits = structure(bestmodels, names = model.names(bestmodels,
                                                            labels = getAllTerms(global_pcount)))))
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="0_data/processed/unmarked/WTSPbestmodels.csv")

#4. Run mixture models

#Example
Julian.nullsite <- pcount(~Julian ~1, data = umf, K=50)              
Julian.nullsite  

Julian.shrubbery <- pcount(~Julian ~mean.VPD__1.2m_Grid.1x1m+sd.VPD__1.2m_Grid.1x1m, data = umf, K=50)              
Julian.shrubbery

#5. Model selection
results.stage1 <- model.sel('Local'=psi.local, 'LocalSA200'=psi.localSA200, 'LocalSA400'=psi.localSA400, 'LocalSA600'=psi.localSA600,'logging.2s'=psi.logging.2s, 'logging.2sSA200'=psi.logging.2sSA200, 'logging.2sSA400'=psi.logging.2sSA400, 'logging.2sSA600'=psi.logging.2sSA600, 'conif.2s'=psi.conif.2s, 'conif.2sSA200'=psi.conif.2sSA200, 'conif.2sSA400'=psi.conif.2sSA400, 'conif.2sSA600'=psi.conif.2sSA600, 'Full'=psi.full, 'FullSA200'=psi.fullSA200, 'FullSA400'=psi.fullSA400, 'FullSA600'=psi.fullSA600, 'Clustering200'=autolog.200, 'Clustering400'=autolog.400, 'Clustering600'=autolog.600, 'Occu:conif50'=psi.conif50, 'Occu:conif50SA200'=psi.conif50SA200, 'Occu:conif50SA400'=psi.conif50SA400, 'Occu:conif50SA600'=psi.conif50SA600, 'Occu:conif600'=psi.conif600, 'Occu:conif600SA200'=psi.conif600SA200, 'Occu:conif600SA400'=psi.conif600SA400, 'Occu:conif600SA600'=psi.conif600SA600, 'Occu:logging500m'=psi.logging500m, 'Occu:logging500mSA200'=psi.logging500mSA200, 'Occu:logging500mSA400'=psi.logging500mSA400, 'Occu:logging500mSA600'=psi.logging500mSA600, 'Null model' = null,'Occu:cutblocks'=psi.cut, 'Occu:cutblocksSA200'=psi.cutSA200, 'Occu:cutblocksSA400'=psi.cutSA400, 'Occu:cutblocksSA600'=psi.cutSA600, 'Occu:newvsoldcut'=psi.cut3, 'Occu:Series'=psi.series, rank=AIC)           
bestmodel.stage1<-get.models(results.stage1, subset = 1)[[1]]

fms <- fitList('Local'=psi.local, 
               'LocalSA200'=psi.localSA200, 
               'LocalSA400'=psi.localSA400, 
               'LocalSA600'=psi.localSA600,
               'logging.2s'=psi.logging.2s, 
               'logging.2sSA200'=psi.logging.2sSA200, 
               'logging.2sSA400'=psi.logging.2sSA400, 
               'logging.2sSA600'=psi.logging.2sSA600, 
               'conif.2s'=psi.conif.2s, 
               'conif.2sSA200'=psi.conif.2sSA200, 
               'conif.2sSA400'=psi.conif.2sSA400, 
               'conif.2sSA600'=psi.conif.2sSA600, 
               'Full'=psi.full, 
               'FullSA200'=psi.fullSA200, 
               'FullSA400'=psi.fullSA400, 
               'FullSA600'=psi.fullSA600, 
               'Clustering200'=autolog.200, 
               'Clustering400'=autolog.400, 
               'Clustering600'=autolog.600, 
               'Occu:conif50'=psi.conif50, 
               'Occu:conif50SA200'=psi.conif50SA200, 
               'Occu:conif50SA400'=psi.conif50SA400, 
               'Occu:conif50SA600'=psi.conif50SA600, 
               'Occu:conif600'=psi.conif600, 
               'Occu:conif600SA200'=psi.conif600SA200, 
               'Occu:conif600SA400'=psi.conif600SA400, 
               'Occu:conif600SA600'=psi.conif600SA600, 
               'Occu:logging500m'=psi.logging500m, 
               'Occu:logging500mSA200'=psi.logging500mSA200, 
               'Occu:logging500mSA400'=psi.logging500mSA400, 
               'Occu:logging500mSA600'=psi.logging500mSA600, 
               'Null model' = null,
               'Occu:cutblocks'=psi.cut, 
               'Occu:cutblocksSA200'=psi.cutSA200, 
               'Occu:cutblocksSA400'=psi.cutSA400, 
               'Occu:cutblocksSA600'=psi.cutSA600, 
               'Occu:newvsoldcut'=psi.cut3, 
               'Occu:Series'=psi.series)          
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="BBWA.stage1modeltableJuly.csv")

#6. Pick best model from Stage 1 for new round of model selection
gamma.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= update(bestmodel.stage1@gamformula, ~.+Age.class), epsilonformula= bestmodel.stage1@epsformula, pformula= ~1, data=bird))

