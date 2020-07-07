library(unmarked)
library(MuMIn)
library(jpeg)
library(ggplot2)

#site level covariates
BBWA200<-read.csv("BBWA_weights_200.csv",header=TRUE)
BBWA400<-read.csv("BBWA_weights_400.csv",header=TRUE)
BBWA600<-read.csv("BBWA_weights_600.csv",header=TRUE)
BBWA200sa<-as.data.frame(BBWA200[,2])
BBWA400sa<-as.data.frame(BBWA400[,2])
BBWA600sa<-as.data.frame(BBWA600[,2])

sitecovs<-read.csv("sitecovs.csv",header=TRUE)  # 
site.cov<-c(BBWA200sa, BBWA400sa, BBWA600sa, sitecovs[,c("Cut","Cut3class","Series","TotalAmtLogged1994.200m","TotalAmtLogged1994.500m","Sb.50","Sw.50","Sb.100","Sw.100","Sb.200","Sw.200","Sb.400","Sw.400","Sb.600","Sw.600","height.50","height.100")])
write.csv(site.cov, file="site.cov.check.csv") #data frame check

site.cov<-read.csv("site.cov.check.csv",header=TRUE)  # 
site.cov$conif.50<-scale(site.cov$Sb.50+site.cov$Sw.50)
site.cov$conif.100<-scale(site.cov$Sb.100+site.cov$Sw.100)
site.cov$conif.200<-scale(site.cov$Sb.200+site.cov$Sw.200)
site.cov$conif.400<-scale(site.cov$Sb.400+site.cov$Sw.400)
site.cov$conif.600<-scale(site.cov$Sb.600+site.cov$Sw.600)
site.cov$TotalAmtLogged1994.500m<-scale(site.cov$TotalAmtLogged1994.500m)
#standardize potential variables affecting initial site occupancy

site.cov19<-read.csv("sitecovs_19years.csv",header=TRUE)  # 
conif.50<-scale(site.cov19$Sb.50+site.cov19$Sw.50)
conif.600<-scale(site.cov19$Sb.600+site.cov19$Sw.600)
TotalAmtLogged1994.500m<-scale(site.cov19$TotalAmtLogged1994.500m)

Year<-rep(c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,22,23),72)
Cut<-c(rep(rep(0,19),24),rep(rep(1,19),24),rep(rep(1,19),24))#assign values to site-year based on if a site was a cutblock or not

Age<-c(rep(rep(60,19),24),rep(c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,22,23),24),rep(c(11,12,14,15,16,17,18,19,20,23,24,26,27,28,29,30,31,32,33),24))#assign values to site-year based on if a station occurred in a cutblock or not
Age.class<-c(rep(rep("3",19),24),rep(c("0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","2","2","2","2"),24),rep(c("1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","2"),24))#age class 0(<10), 1(10-20), 2(20-40), 3(>40)
#Age.class<-c(rep(rep("4",18),24),rep(c("0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","2","2","2"),21),rep(rep("4",18),54),rep(c("1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","3","3","3"),24))#age class 0(<10), 1(10-20), 2(20-30), 3(30-40), 4(>40)

YearXCut<-Year*Cut

meanJulT.0<-rep(c(16.5,14.5,16.5,17.1,14.4,16,15.5,16.2,16.6,17.4,14.9,10.8,12.05,12.1,14.9,15.7,17.25,18.05,17),72)
meanJulT.1<-rep(c(14.1,16.5,14.7,16.5,17.1,14.4,16,15.5,16.2,15.4,19.4,14.9,10.8,12.05,12.1,14.9,15.7,17.25,18.05),72)

maxJulT.0<-rep(c(29,25,28,29.5,29,28,27.5,31.5,29.5,32,24.2,19,19.9,19.5,20.5,30.4,30.6,32.7,29.2),72)
maxJulT.1<-rep(c(25.5,29,26.5,28,29.5,29,28,27.5,31.5,26.5,32,24.2,19,19.9,19.5,20.5,30.4,30.6,32.7),72)
  
minJulT.0<-rep(c(6,5,4.5,2.5,-2,4,3,0,6,7,5.6,2.6,4.2,4.7,9.4,68.1,52.1,45.4,40.2),72)
minJulT.1<-rep(c(1.5,6,4.5,4.5,2.5,-2,4,3,0,3,5,5.6,2.6,4.2,4.7,9.3,1,3.9,3.4),72)

Julprecip.0<-rep(c(84.2,120.7,58.3,70.2,25.9,152.5,108,121.1,93,124,0.4,3.8,1.1,0,9.4,68.1,52.1,45.4,40.2),72)
Julprecip.1<-rep(c(98.9,84.2,146.7,58.3,70.2,25.9,152.5,108,121.1,90.5,45,0.4,3.8,1.1,0,9.4,68.1,52.1,45.4),72)

elnino.DJF.0<-rep(c(0.1,0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.9,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5,0.6,2.2),72)
elnino.DJF.1<-rep(c(0.2,0.1,-0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.7,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5,0.6),72)

elnino.0Xcut<-elnino.DJF.0*Cut
elnino.1Xcut<-elnino.DJF.1*Cut

logging.0<-rep(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),72)#same year effect of logging
logging.1<-rep(c(0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),72)#delayed effect of logging
#amount of logged habitat within 200 m does not change over 25 years, but amount of logged
#habitat within 500 m changes at a few 40-ha control site stations after 2006.
Year.scaled<-scale(Year, center = FALSE, scale = TRUE)#as numeric variable
Year.centered<-scale(Year, center = TRUE, scale = FALSE)#as numeric variable
Year.centeredXCut<-Year.centered*Cut
Year.squared<-Year.centered^2
Year.squaredXCut<-Year.squared*Cut
Year.factor<-as.factor(Year)#as factor

BBWAwts200<-read.csv("BBWA_weights_200.stacked.csv", header=TRUE)
autolog.200<-BBWAwts200$values

BBWAwts400<-read.csv("BBWA_weights_400.stacked.csv", header=TRUE)
autolog.400<-BBWAwts400$values

BBWAwts600<-read.csv("BBWA_weights_600.stacked.csv", header=TRUE)
autolog.600<-BBWAwts600$values

yearly.cov<-data.frame(conif.50, conif.600, TotalAmtLogged1994.500m, autolog.200, autolog.400, autolog.600, Year, Year.scaled, Year.centered, Year.centeredXCut, Year.squared, Year.squaredXCut, Year.factor, Age, Age.class, Cut, YearXCut, meanJulT.0, maxJulT.0, minJulT.0, Julprecip.0, meanJulT.1, maxJulT.1, minJulT.1, Julprecip.1, elnino.DJF.0, elnino.DJF.1, logging.0, logging.1)

recast.julian<-read.csv("recast.julian.csv",header=TRUE)
recast.jcen<-read.csv("recast.jcen.csv",header=TRUE)
recast.jcen2<-read.csv("recast.jcen2.csv",header=TRUE)

recast.time<-read.csv("recast.time.csv",header=TRUE)
recast.tcen<-read.csv("recast.tcen.csv",header=TRUE)
recast.tcen2<-read.csv("recast.tcen2.csv",header=TRUE)

"'logLik.try-error'"<-function(object,...) structure(-Inf, nobs = 1L, df = 1L, class = "logLik")

#purpose of this line is to assign a ridiculously small log-likelihood to a model if the model fails to run, so that
#the model can still be compared to other models within the model.sel function as part of automated model selection.

#Example: Single Species

data<-read.csv("recast.BBWA.csv",header=TRUE)
y<-data[,c(2:77)]
S <- nrow(data) # number of sites  (9)
J <- 4 # number of secondary sampling occasions    (4 per year)
T <- 19 # number of primary periods    (19 years of observations over 23 years)

bird<-unmarkedMultFrame(y=y,siteCovs=site.cov,numPrimary=19,yearlySiteCovs=yearly.cov,
                        obsCovs=list(Julian=recast.julian[,c(2:77)], Time=recast.time[,c(2:77)],
                                     jcen=recast.jcen[,c(2:77)], jcen2=recast.jcen2[,c(2:77)], 
                                     tcen=recast.tcen[,c(2:77)], tcen2=recast.tcen2[,c(2:77)]))  


pairs(cbind(site.cov$conif.50, site.cov$conif.100, site.cov$conif.200, site.cov$conif.400, site.cov$conif.600))
pairs(cbind(site.cov$conif.50, site.cov$conif.600, site.cov$TotalAmtLogged1994.200m, site.cov$TotalAmtLogged1994.500m))
pairs(cbind(site.cov$height.50, site.cov$height.100, site.cov$TotalAmtLogged1994.200m, site.cov$TotalAmtLogged1994.500m))
pairs(cbind(site.cov$height.50, site.cov$height.100, site.cov$conif.50, site.cov$conif.600))

#cannot use dredge function because there are missing values

#test for evidence of spatial autocorrelation
null<-try(colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #null model
autolog.200<-try(colext(psiformula = ~BBWA200, gammaformula = ~autolog.200, epsilonformula = ~autolog.200, pformula = ~1, data=bird))    #null model
autolog.400<-try(colext(psiformula = ~BBWA400, gammaformula = ~autolog.400, epsilonformula = ~autolog.400, pformula = ~1, data=bird))    #null model
autolog.600<-try(colext(psiformula = ~BBWA600, gammaformula = ~autolog.600, epsilonformula = ~autolog.600, pformula = ~1, data=bird))    #null model
#aggregation hypothesis (Mattson et al. 2013)
psi.cut<-try(colext(psiformula = ~Cut, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cutSA200<-try(colext(psiformula = ~Cut+BBWA200, gammaformula = ~autolog.200, epsilonformula = ~autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cutSA400<-try(colext(psiformula = ~Cut+BBWA400, gammaformula = ~autolog.400, epsilonformula = ~autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cutSA600<-try(colext(psiformula = ~Cut+BBWA600, gammaformula = ~autolog.600, epsilonformula = ~autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cut3<-try(colext(psiformula = ~Cut3class, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.series<-try(colext(psiformula = ~Series, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging200m<-try(colext(psiformula = ~TotalAmtLogged1994.200m, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging500m<-try(colext(psiformula = ~TotalAmtLogged1994.500m, gammaformula = ~TotalAmtLogged1994.500m, epsilonformula = ~TotalAmtLogged1994.500m, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging500mSA200<-try(colext(psiformula = ~TotalAmtLogged1994.500m+BBWA200, gammaformula = ~TotalAmtLogged1994.500m+autolog.200, epsilonformula = ~TotalAmtLogged1994.500m+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging500mSA400<-try(colext(psiformula = ~TotalAmtLogged1994.500m+BBWA400, gammaformula = ~TotalAmtLogged1994.500m+autolog.400, epsilonformula = ~TotalAmtLogged1994.500m+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging500mSA600<-try(colext(psiformula = ~TotalAmtLogged1994.500m+BBWA600, gammaformula = ~TotalAmtLogged1994.500m+autolog.600, epsilonformula = ~TotalAmtLogged1994.500m+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif50<-try(colext(psiformula = ~conif.50, gammaformula = ~conif.50, epsilonformula = ~conif.50, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif50SA200<-try(colext(psiformula = ~conif.50+BBWA200, gammaformula = ~conif.50+autolog.200, epsilonformula = ~conif.50+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif50SA400<-try(colext(psiformula = ~conif.50+BBWA400, gammaformula = ~conif.50+autolog.400, epsilonformula = ~conif.50+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif50SA600<-try(colext(psiformula = ~conif.50+BBWA600, gammaformula = ~conif.50+autolog.600, epsilonformula = ~conif.50+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif100<-try(colext(psiformula = ~conif.100, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif200<-try(colext(psiformula = ~conif.200, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif400<-try(colext(psiformula = ~conif.400, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif600<-try(colext(psiformula = ~conif.600, gammaformula = ~conif.600, epsilonformula = ~conif.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif600SA200<-try(colext(psiformula = ~conif.600+BBWA200, gammaformula = ~conif.600+autolog.200, epsilonformula = ~conif.600+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif600SA400<-try(colext(psiformula = ~conif.600+BBWA400, gammaformula = ~conif.600+autolog.400, epsilonformula = ~conif.600+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif600SA600<-try(colext(psiformula = ~conif.600+BBWA600, gammaformula = ~conif.600+autolog.600, epsilonformula = ~conif.600+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.height50<-try(colext(psiformula = ~height.50, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.height100<-try(colext(psiformula = ~height.100, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.full<-try(colext(psiformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.fullSA200<-try(colext(psiformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+BBWA200, gammaformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.200, epsilonformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.fullSA400<-try(colext(psiformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+BBWA400, gammaformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.400, epsilonformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.fullSA600<-try(colext(psiformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+BBWA600, gammaformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.600, epsilonformula = ~conif.50+conif.600+Cut+TotalAmtLogged1994.500m+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested

psi.logging.2s<-try(colext(psiformula = ~Cut+TotalAmtLogged1994.500m, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging.2sSA200<-try(colext(psiformula = ~Cut+TotalAmtLogged1994.500m+BBWA200, gammaformula = ~Cut+TotalAmtLogged1994.500m+autolog.200, epsilonformula = ~Cut+TotalAmtLogged1994.500m+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging.2sSA400<-try(colext(psiformula = ~Cut+TotalAmtLogged1994.500m+BBWA400, gammaformula = ~Cut+TotalAmtLogged1994.500m+autolog.400, epsilonformula = ~Cut+TotalAmtLogged1994.500m+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.logging.2sSA600<-try(colext(psiformula = ~Cut+TotalAmtLogged1994.500m+BBWA600, gammaformula = ~Cut+TotalAmtLogged1994.500m+autolog.600, epsilonformula = ~Cut+TotalAmtLogged1994.500m+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested

psi.conif.2s<-try(colext(psiformula = ~conif.50+conif.600, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif.2sSA200<-try(colext(psiformula = ~conif.50+conif.600+BBWA200, gammaformula = ~conif.50+conif.600+autolog.200, epsilonformula = ~conif.50+conif.600+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif.2sSA400<-try(colext(psiformula = ~conif.50+conif.600+BBWA400, gammaformula = ~conif.50+conif.600+autolog.400, epsilonformula = ~conif.50+conif.600+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.conif.2sSA600<-try(colext(psiformula = ~conif.50+conif.600+BBWA600, gammaformula = ~conif.50+conif.600+autolog.600, epsilonformula = ~conif.50+conif.600+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested

psi.local<-try(colext(psiformula = ~conif.50+Cut, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.localSA200<-try(colext(psiformula = ~conif.50+Cut+BBWA200, gammaformula = ~conif.50+Cut+autolog.200, epsilonformula = ~conif.50+Cut+autolog.200, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.localSA400<-try(colext(psiformula = ~conif.50+Cut+BBWA400, gammaformula = ~conif.50+Cut+autolog.400, epsilonformula = ~conif.50+Cut+autolog.400, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.localSA600<-try(colext(psiformula = ~conif.50+Cut+BBWA600, gammaformula = ~conif.50+Cut+autolog.600, epsilonformula = ~conif.50+Cut+autolog.600, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested


#land cover at smaller scales as alternate explanation for apparent clustering?
#or global combination of exogeneous and endogeneous factors?

results.stage1 <- model.sel('Local'=psi.local, 'LocalSA200'=psi.localSA200, 'LocalSA400'=psi.localSA400, 'LocalSA600'=psi.localSA600,'logging.2s'=psi.logging.2s, 'logging.2sSA200'=psi.logging.2sSA200, 'logging.2sSA400'=psi.logging.2sSA400, 'logging.2sSA600'=psi.logging.2sSA600, 'conif.2s'=psi.conif.2s, 'conif.2sSA200'=psi.conif.2sSA200, 'conif.2sSA400'=psi.conif.2sSA400, 'conif.2sSA600'=psi.conif.2sSA600, 'Full'=psi.full, 'FullSA200'=psi.fullSA200, 'FullSA400'=psi.fullSA400, 'FullSA600'=psi.fullSA600, 'Clustering200'=autolog.200, 'Clustering400'=autolog.400, 'Clustering600'=autolog.600, 'Occu:conif50'=psi.conif50, 'Occu:conif50SA200'=psi.conif50SA200, 'Occu:conif50SA400'=psi.conif50SA400, 'Occu:conif50SA600'=psi.conif50SA600, 'Occu:conif600'=psi.conif600, 'Occu:conif600SA200'=psi.conif600SA200, 'Occu:conif600SA400'=psi.conif600SA400, 'Occu:conif600SA600'=psi.conif600SA600, 'Occu:logging500m'=psi.logging500m, 'Occu:logging500mSA200'=psi.logging500mSA200, 'Occu:logging500mSA400'=psi.logging500mSA400, 'Occu:logging500mSA600'=psi.logging500mSA600, 'Null model' = null,'Occu:cutblocks'=psi.cut, 'Occu:cutblocksSA200'=psi.cutSA200, 'Occu:cutblocksSA400'=psi.cutSA400, 'Occu:cutblocksSA600'=psi.cutSA600, 'Occu:newvsoldcut'=psi.cut3, 'Occu:Series'=psi.series, rank=AIC)           
bestmodel.stage1<-get.models(results.stage1, subset = 1)[[1]]

fms <- fitList('Local'=psi.local, 'LocalSA200'=psi.localSA200, 'LocalSA400'=psi.localSA400, 'LocalSA600'=psi.localSA600,'logging.2s'=psi.logging.2s, 'logging.2sSA200'=psi.logging.2sSA200, 'logging.2sSA400'=psi.logging.2sSA400, 'logging.2sSA600'=psi.logging.2sSA600, 'conif.2s'=psi.conif.2s, 'conif.2sSA200'=psi.conif.2sSA200, 'conif.2sSA400'=psi.conif.2sSA400, 'conif.2sSA600'=psi.conif.2sSA600, 'Full'=psi.full, 'FullSA200'=psi.fullSA200, 'FullSA400'=psi.fullSA400, 'FullSA600'=psi.fullSA600, 'Clustering200'=autolog.200, 'Clustering400'=autolog.400, 'Clustering600'=autolog.600, 'Occu:conif50'=psi.conif50, 'Occu:conif50SA200'=psi.conif50SA200, 'Occu:conif50SA400'=psi.conif50SA400, 'Occu:conif50SA600'=psi.conif50SA600, 'Occu:conif600'=psi.conif600, 'Occu:conif600SA200'=psi.conif600SA200, 'Occu:conif600SA400'=psi.conif600SA400, 'Occu:conif600SA600'=psi.conif600SA600, 'Occu:logging500m'=psi.logging500m, 'Occu:logging500mSA200'=psi.logging500mSA200, 'Occu:logging500mSA400'=psi.logging500mSA400, 'Occu:logging500mSA600'=psi.logging500mSA600, 'Null model' = null,'Occu:cutblocks'=psi.cut, 'Occu:cutblocksSA200'=psi.cutSA200, 'Occu:cutblocksSA400'=psi.cutSA400, 'Occu:cutblocksSA600'=psi.cutSA600, 'Occu:newvsoldcut'=psi.cut3, 'Occu:Series'=psi.series)          
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="BBWA.stage1modeltableJuly.csv")

#Stage 2 - singular fixed effects over time - cutblock recovery or weather
gamma.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= update(bestmodel.stage1@gamformula, ~.+Age.class), epsilonformula= bestmodel.stage1@epsformula, pformula= ~1, data=bird))
epsilon.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= bestmodel.stage1@gamformula, epsilonformula= update(bestmodel.stage1@epsformula, ~.+Age.class), pformula= ~1, data=bird))
gamma.yearxcut<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year+Cut+YearXCut), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year+Cut+YearXCut), pformula = ~1, data=bird))   
gamma.yearxcut2<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut2<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut), pformula = ~1, data=bird))   

ext.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+elnino.DJF.0), pformula = ~1, data=bird))     
ext.elninoB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+elnino.DJF.1), pformula = ~1, data=bird))     
ext.elninoC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+elnino.DJF.0+elnino.DJF.1), pformula = ~1, data=bird))     
gamma.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+elnino.DJF.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.elninoB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+elnino.DJF.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.elninoC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+elnino.DJF.0+elnino.DJF.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     

ext.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Julprecip.0), pformula = ~1, data=bird))     
ext.JulprecipB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Julprecip.1), pformula = ~1, data=bird))     
ext.JulprecipC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Julprecip.0+Julprecip.1), pformula = ~1, data=bird))     
gamma.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Julprecip.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.JulprecipB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Julprecip.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.JulprecipC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Julprecip.0+Julprecip.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     

ext.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+meanJulT.0), pformula = ~1, data=bird))     
ext.meanJulTB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+meanJulT.1), pformula = ~1, data=bird))     
ext.meanJulTC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+meanJulT.0+meanJulT.1), pformula = ~1, data=bird))     
gamma.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+meanJulT.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.meanJulTB<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+meanJulT.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     
gamma.meanJulTC<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+meanJulT.0+meanJulT.1), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))     

results.stage2 <- model.sel('July T - same year - ext' = ext.meanJulT, 'July T - lag 1 - ext' = ext.meanJulTB, 'July T - same-year and lagged effects -ext' = ext.meanJulTC, 'July T - same year - gamma' = gamma.meanJulT, 'July T - lag 1 - gamma' = gamma.meanJulTB, 'July T - same-year and lagged effects -gamma' = gamma.meanJulTC, 'El Nino - same year - ext' = ext.elnino, 'El Nino - lag 1 - ext' = ext.elninoB, 'El Nino - same-year and lagged effects -ext' = ext.elninoC, 'El Nino - same year - gamma' = gamma.elnino, 'El Nino - lag 1 - gamma' = gamma.elninoB, 'El Nino - same-year and lagged effects -gamma' = gamma.elninoC, 'July precip - same year - ext' = ext.Julprecip, 'July precip - lag 1 - ext' = ext.JulprecipB, 'July precip - same-year and lagged effects -ext' = ext.JulprecipC, 'July precip - same year - gamma' = gamma.Julprecip, 'July precip - lag 1 - gamma' = gamma.JulprecipB, 'July precip - same-year and lagged effects -gamma' = gamma.JulprecipC, 'Best Stage 1'= bestmodel.stage1, 'Age class - Gamma' = gamma.ageclass, 'Age class - Epsilon' = epsilon.ageclass, 'YearXCut - Gamma'=gamma.yearxcut, 'YearXCut - Epsilon'=epsilon.yearxcut, 'YearXCut2 - Gamma'=gamma.yearxcut2, 'YearXCut2 - Epsilon'=epsilon.yearxcut2, rank=AIC)           
bestmodel.stage2<-get.models(results.stage2, subset = 1)[[1]]
 
fms <- fitList('July T - same year - ext' = ext.meanJulT, 'July T - lag 1 - ext' = ext.meanJulTB, 'July T - same-year and lagged effects -ext' = ext.meanJulTC, 'July T - same year - gamma' = gamma.meanJulT, 'July T - lag 1 - gamma' = gamma.meanJulTB, 'July T - same-year and lagged effects -gamma' = gamma.meanJulTC, 'El Nino - same year - ext' = ext.elnino, 'El Nino - lag 1 - ext' = ext.elninoB, 'El Nino - same-year and lagged effects -ext' = ext.elninoC, 'El Nino - same year - gamma' = gamma.elnino, 'El Nino - lag 1 - gamma' = gamma.elninoB, 'El Nino - same-year and lagged effects -gamma' = gamma.elninoC, 'July precip - same year - ext' = ext.Julprecip, 'July precip - lag 1 - ext' = ext.JulprecipB, 'July precip - same-year and lagged effects -ext' = ext.JulprecipC, 'July precip - same year - gamma' = gamma.Julprecip, 'July precip - lag 1 - gamma' = gamma.JulprecipB, 'July precip - same-year and lagged effects -gamma' = gamma.JulprecipC, 'Best Stage 1'= bestmodel.stage1, 'Age class - Gamma' = gamma.ageclass, 'Age class - Epsilon' = epsilon.ageclass, 'YearXCut - Gamma'=gamma.yearxcut, 'YearXCut - Epsilon'=epsilon.yearxcut, 'YearXCut2 - Gamma'=gamma.yearxcut2, 'YearXCut2 - Epsilon'=epsilon.yearxcut2)           
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="BBWA.stage2modeltableJuly.csv")

#Stage 3 - - multiple fixed effects over time - combined effects of cutblock recovery and weather/weather events
gamma.ageclass.elnino<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= update(bestmodel.stage1@gamformula, ~.+Age.class+elnino.DJF.0), epsilonformula= bestmodel.stage1@epsformula, pformula= ~1, data=bird))
epsilon.ageclass.elnino<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= bestmodel.stage1@gamformula, epsilonformula= update(bestmodel.stage1@epsformula, ~.+Age.class+elnino.DJF.0), pformula= ~1, data=bird))
gamma.yearxcut.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year+Cut+YearXCut+elnino.DJF.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year+Cut+YearXCut+elnino.DJF.0), pformula = ~1, data=bird))   
gamma.yearxcut2.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+elnino.DJF.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut2.elnino<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+elnino.DJF.0), pformula = ~1, data=bird))   

gamma.ageclass.Julprecip<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= update(bestmodel.stage1@gamformula, ~.+Age.class+Julprecip.0), epsilonformula= bestmodel.stage1@epsformula, pformula= ~1, data=bird))
epsilon.ageclass.Julprecip<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= bestmodel.stage1@gamformula, epsilonformula= update(bestmodel.stage1@epsformula, ~.+Age.class+Julprecip.0), pformula= ~1, data=bird))
gamma.yearxcut.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year+Cut+YearXCut+Julprecip.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year+Cut+YearXCut+Julprecip.0), pformula = ~1, data=bird))   
gamma.yearxcut2.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+Julprecip.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut2.Julprecip<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+Julprecip.0), pformula = ~1, data=bird))   

gamma.ageclass.meanJulT<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= update(bestmodel.stage1@gamformula, ~.+Age.class+meanJulT.0), epsilonformula= bestmodel.stage1@epsformula, pformula= ~1, data=bird))
epsilon.ageclass.meanJulT<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= bestmodel.stage1@gamformula, epsilonformula= update(bestmodel.stage1@epsformula, ~.+Age.class+meanJulT.0), pformula= ~1, data=bird))
gamma.yearxcut.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year+Cut+YearXCut+meanJulT.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year+Cut+YearXCut+meanJulT.0), pformula = ~1, data=bird))   
gamma.yearxcut2.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = update(bestmodel.stage1@gamformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+meanJulT.0), epsilonformula = bestmodel.stage1@epsformula, pformula = ~1, data=bird))   
epsilon.yearxcut2.meanJulT<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = bestmodel.stage1@gamformula, epsilonformula = update(bestmodel.stage1@epsformula, ~.+Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut+meanJulT.0), pformula = ~1, data=bird))   


results.stage3 <- {model.sel('bestmodel.stage2'=bestmodel.stage2,
                             'gamma.ageclass.elnino'=gamma.ageclass.elnino, 
                             'epsilon.ageclass.elnino'=epsilon.ageclass.elnino,
                             'gamma.yearxcut.elnino'=gamma.yearxcut.elnino, 
                             'epsilon.yearxcut.elnino'=epsilon.yearxcut.elnino, 
                             'gamma.yearxcut2.elnino'=gamma.yearxcut2.elnino, 
                             'epsilon.yearxcut2.elnino'=epsilon.yearxcut2.elnino, 
                             'gamma.ageclass.Julprecip'=gamma.ageclass.Julprecip, 
                             'epsilon.ageclass.Julprecip'=epsilon.ageclass.Julprecip,
                             'gamma.yearxcut.Julprecip'=gamma.yearxcut.Julprecip, 
                             'epsilon.yearxcut.Julprecip'=epsilon.yearxcut.Julprecip, 
                             'gamma.yearxcut2.Julprecip'=gamma.yearxcut2.Julprecip, 
                             'epsilon.yearxcut2.Julprecip'=epsilon.yearxcut2.Julprecip, 
                             'gamma.ageclass.meanJulT'=gamma.ageclass.meanJulT, 
                             'epsilon.ageclass.meanJulT'=epsilon.ageclass.meanJulT,
                             'gamma.yearxcut.meanJulT'=gamma.yearxcut.meanJulT, 
                             'epsilon.yearxcut.meanJulT'=epsilon.yearxcut.meanJulT, 
                             'gamma.yearxcut2.meanJulT'=gamma.yearxcut2.meanJulT, 
                             'epsilon.yearxcut2.meanJulT'=epsilon.yearxcut2.meanJulT, rank=AIC)}           
bestmodel.stage3<-get.models(results.stage3, subset = 1)[[1]]

fms <- {fitList('bestmodel.stage2'=bestmodel.stage2,
               'gamma.ageclass.elnino'=gamma.ageclass.elnino, 
               'epsilon.ageclass.elnino'=epsilon.ageclass.elnino,
               'gamma.yearxcut.elnino'=gamma.yearxcut.elnino, 
               'epsilon.yearxcut.elnino'=epsilon.yearxcut.elnino, 
               'gamma.yearxcut2.elnino'=gamma.yearxcut2.elnino, 
               'epsilon.yearxcut2.elnino'=epsilon.yearxcut2.elnino, 
               'gamma.ageclass.Julprecip'=gamma.ageclass.Julprecip, 
               'epsilon.ageclass.Julprecip'=epsilon.ageclass.Julprecip,
               'gamma.yearxcut.Julprecip'=gamma.yearxcut.Julprecip, 
               'epsilon.yearxcut.Julprecip'=epsilon.yearxcut.Julprecip, 
               'gamma.yearxcut2.Julprecip'=gamma.yearxcut2.Julprecip, 
               'epsilon.yearxcut2.Julprecip'=epsilon.yearxcut2.Julprecip, 
               'gamma.ageclass.meanJulT'=gamma.ageclass.meanJulT, 
               'epsilon.ageclass.meanJulT'=epsilon.ageclass.meanJulT,
               'gamma.yearxcut.meanJulT'=gamma.yearxcut.meanJulT, 
               'epsilon.yearxcut.meanJulT'=epsilon.yearxcut.meanJulT, 
               'gamma.yearxcut2.meanJulT'=gamma.yearxcut2.meanJulT, 
               'epsilon.yearxcut2.meanJulT'=epsilon.yearxcut2.meanJulT)}
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="BBWA.stage3modeltableJuly.csv")

det.julian<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Julian, data=bird))     
det.julian.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~jcen+jcen2, data=bird))     
det.time<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Time, data=bird))     
det.time.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~tcen+tcen2, data=bird))     
det.juliantime<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~jcen+jcen2+Time, data=bird))     
det.juliantime.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Julian+tcen+tcen2, data=bird))     
results.stage4 <- model.sel('Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'Best Stage 3'= bestmodel.stage3, 'Julian' = det.julian, 'Time' = det.time, 'Julian.sq' = det.julian.sq, 'Time.sq' = det.time.sq, 'Julian+Time' = det.juliantime, rank=AIC)           
bestmodel.stage4<-get.models(results.stage4, subset = 1)[[1]]#'Julian.sq+Time.sq' = det.juliantime.sq, 

fms <- fitList('Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'Best Stage 3'= bestmodel.stage3, 'Julian' = det.julian, 'Julian.sq' = det.julian.sq, 'Time' = det.time, 'Time.sq' = det.time.sq, 'Julian+Time' = det.juliantime)
ms<-modSel(fms)#, 'Julian.sq+Time.sq' = det.juliantime.sq 
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file="BBWA.stage4modeltableJuly.csv")



#use bootstrap to get estimates of psi (occupancy) from derived parameter estimates
bestmodel.stage4.boot<-nonparboot(bestmodel.stage4, B=100)
str(bestmodel.stage4.boot@projected)#342 observations: this is a 3d array whose number is equal to 9(#sites)*19(#years)*2(1st vector for prob. absence,
#2nd vector for prob. occupancy)
#http://www.statmethods.net/advstats/bootstrapping.html for a description of the bootstrap object structure
#boot( ) calls the statistic function R times. Each time, it generates a set of 
#random indices, with replacement, from the integers 1:nrow(data). These indices 
#are used within the statistic function to select a sample. The statistics are 
#calculated on the sample and the results are accumulated in the bootobject. The 
#bootobject structure includes

#basic bootstrap object element description 
#t0           The observed values of k statistics applied to the orginal data.  
#t            An R x k matrix where each row is a bootstrap replicate of the k statistics. 

#You can access these as bootobject$t0 and bootobject$t.

#Once you generate the bootstrap samples, print(bootobject) and plot(bootobject) can be used to examine the 
#results. If the results look reasonable, you can use boot.ci( ) function to obtain confidence intervals for 
#the statistic(s). 

bestmodel.stage3.boot@projected[2,,1]#this is how you call up predicted probability of occupancy based on the bootstrap samples for site 1
#in the colext unmarked Frame (one of the unlogged "control" sites) in each of the 19 years

bestmodel.stage3.boot@projected[2,,9]#this is how you call up predicted probability of occupancy based on the bootstrap samples for site 1
#in the colext unmarked Frame (one of the older cutblock sites) in each of the 19 years
bestmodel.stage3.boot@projected[2,,]#this is how you call up predicted probability of occupancy based on the bootstrap samples for site 1
#in the colext unmarked Frame, for all sites in each of the 18 years. An 19x9 matrix

bestmodel.stage4.boot@projected[1,,1]#this is how you call up predicted probability that site is unoccupied based on the original data for site 1
#in the colext unmarked Frame
smoothed(bestmodel.stage4.boot)[2,] 

bestmodel.stage4.boot@projected.mean.bsse#the standard errors for projected occupancy. Note that SE is same for probability of
#presence and probability of absence
bestmodel.stage4.boot@projected.mean.bsse[2,]#Also note that there is 1 SE calculated for each year but not for each site
#unlike probability of occupancy

#Estimates of occupancy probability in years T > 1 must be derived from the estimates
#of rst-year occupancy and the two parameters governing the dynamics, extinction/survival
#and colonization. unmarked does this automatically in two ways. First, the population-level 
#estimates of occupancy probability  t =  t????1t????1 + (1 ???? t????1)
#are calculated and stored in the slot named projected. Slots can be accessed using the @
#operator, e.g. fm@projected. In some cases, interest may lie in making inference about the
#proportion of the sampled sites that are occupied, rather than the entire population of sites.
#These estimates are contained in the smoothed slot of the tted model. Thus, the projected
#values are estimates of population parameters, and the smoothed estimates are of the
#finite-sample quantities. Discussions of the dierences can be found in Weir et al. (2009).

#e.g.smoothed=smoothed(bestmodel.stage4.boot)[2,], SE=bestmodel.stage4.boot@smoothed.mean.bsse[2,]
#Note that the smoothed estimates of occupancy probability are overall calculations for all sites, not differing 
#among individual sites

psi.unlogged.s1.c40111<-bestmodel.stage4.boot@projected[2,,1]
psi.unlogged.s1.c40112<-bestmodel.stage4.boot@projected[2,,2]
psi.unlogged.s1.c40113<-bestmodel.stage4.boot@projected[2,,3]
psi.unlogged.s1.c40114<-bestmodel.stage4.boot@projected[2,,4]
psi.unlogged.s1.c40121<-bestmodel.stage4.boot@projected[2,,5]
psi.unlogged.s1.c40122<-bestmodel.stage4.boot@projected[2,,6]
psi.unlogged.s1.c40123<-bestmodel.stage4.boot@projected[2,,7]
psi.unlogged.s1.c40124<-bestmodel.stage4.boot@projected[2,,8]

psi.unlogged.s2.c40211<-bestmodel.stage4.boot@projected[2,,9]
psi.unlogged.s2.c40212<-bestmodel.stage4.boot@projected[2,,10]
psi.unlogged.s2.c40213<-bestmodel.stage4.boot@projected[2,,11]
psi.unlogged.s2.c40214<-bestmodel.stage4.boot@projected[2,,12]
psi.unlogged.s2.c40221<-bestmodel.stage4.boot@projected[2,,13]
psi.unlogged.s2.c40222<-bestmodel.stage4.boot@projected[2,,14]
psi.unlogged.s2.c40223<-bestmodel.stage4.boot@projected[2,,15]
psi.unlogged.s2.c40224<-bestmodel.stage4.boot@projected[2,,16]

psi.unlogged.s3.c40311<-bestmodel.stage4.boot@projected[2,,17]
psi.unlogged.s3.c40312<-bestmodel.stage4.boot@projected[2,,18]
psi.unlogged.s3.c40313<-bestmodel.stage4.boot@projected[2,,19]
psi.unlogged.s3.c40314<-bestmodel.stage4.boot@projected[2,,20]
psi.unlogged.s3.c40321<-bestmodel.stage4.boot@projected[2,,21]
psi.unlogged.s3.c40322<-bestmodel.stage4.boot@projected[2,,22]
psi.unlogged.s3.c40323<-bestmodel.stage4.boot@projected[2,,23]
psi.unlogged.s3.c40324<-bestmodel.stage4.boot@projected[2,,24]

psi.newcutblock.s1.cc1_1<-bestmodel.stage4.boot@projected[2,,25]
psi.newcutblock.s1.cc1_2<-bestmodel.stage4.boot@projected[2,,26]
psi.newcutblock.s1.cc1_3<-bestmodel.stage4.boot@projected[2,,27]
psi.newcutblock.s1.cc1_4<-bestmodel.stage4.boot@projected[2,,28]
psi.newcutblock.s1.cc1_5<-bestmodel.stage4.boot@projected[2,,29]
psi.newcutblock.s1.cc1_6<-bestmodel.stage4.boot@projected[2,,30]
psi.newcutblock.s1.cc1_7<-bestmodel.stage4.boot@projected[2,,31]
psi.newcutblock.s1.cc1_8<-bestmodel.stage4.boot@projected[2,,32]

psi.newcutblock.s2.cc2_1<-bestmodel.stage4.boot@projected[2,,33]
psi.newcutblock.s2.cc2_2<-bestmodel.stage4.boot@projected[2,,34]
psi.newcutblock.s2.cc2_3<-bestmodel.stage4.boot@projected[2,,35]
psi.newcutblock.s2.cc2_4<-bestmodel.stage4.boot@projected[2,,36]
psi.newcutblock.s2.cc2_5<-bestmodel.stage4.boot@projected[2,,37]
psi.newcutblock.s2.cc2_6<-bestmodel.stage4.boot@projected[2,,38]
psi.newcutblock.s2.cc2_7<-bestmodel.stage4.boot@projected[2,,39]
psi.newcutblock.s2.cc2_8<-bestmodel.stage4.boot@projected[2,,40]

psi.newcutblock.s3.cc3_1<-bestmodel.stage4.boot@projected[2,,41]
psi.newcutblock.s3.cc3_2<-bestmodel.stage4.boot@projected[2,,42]
psi.newcutblock.s3.cc3_3<-bestmodel.stage4.boot@projected[2,,43]
psi.newcutblock.s3.cc3_4<-bestmodel.stage4.boot@projected[2,,44]
psi.newcutblock.s3.cc3_5<-bestmodel.stage4.boot@projected[2,,45]
psi.newcutblock.s3.cc3_6<-bestmodel.stage4.boot@projected[2,,46]
psi.newcutblock.s3.cc3_7<-bestmodel.stage4.boot@projected[2,,47]
psi.newcutblock.s3.cc3_8<-bestmodel.stage4.boot@projected[2,,48]

psi.oldercutblock.s1.s40111<-bestmodel.stage4.boot@projected[2,,49]
psi.oldercutblock.s1.s40112<-bestmodel.stage4.boot@projected[2,,50]
psi.oldercutblock.s1.s40121<-bestmodel.stage4.boot@projected[2,,51]
psi.oldercutblock.s1.s40122<-bestmodel.stage4.boot@projected[2,,52]
psi.oldercutblock.s1.s40131<-bestmodel.stage4.boot@projected[2,,53]
psi.oldercutblock.s1.s40132<-bestmodel.stage4.boot@projected[2,,54]
psi.oldercutblock.s1.s40141<-bestmodel.stage4.boot@projected[2,,55]
psi.oldercutblock.s1.s40142<-bestmodel.stage4.boot@projected[2,,56]

psi.oldercutblock.s2.s40211<-bestmodel.stage4.boot@projected[2,,57]
psi.oldercutblock.s2.s40212<-bestmodel.stage4.boot@projected[2,,58]
psi.oldercutblock.s2.s40221<-bestmodel.stage4.boot@projected[2,,59]
psi.oldercutblock.s2.s40222<-bestmodel.stage4.boot@projected[2,,60]
psi.oldercutblock.s2.s40231<-bestmodel.stage4.boot@projected[2,,61]
psi.oldercutblock.s2.s40232<-bestmodel.stage4.boot@projected[2,,62]
psi.oldercutblock.s2.s40241<-bestmodel.stage4.boot@projected[2,,63]
psi.oldercutblock.s2.s40242<-bestmodel.stage4.boot@projected[2,,64]

psi.oldercutblock.s3.s40311<-bestmodel.stage4.boot@projected[2,,65]
psi.oldercutblock.s3.s40312<-bestmodel.stage4.boot@projected[2,,66]
psi.oldercutblock.s3.s40321<-bestmodel.stage4.boot@projected[2,,67]
psi.oldercutblock.s3.s40322<-bestmodel.stage4.boot@projected[2,,68]
psi.oldercutblock.s3.s40331<-bestmodel.stage4.boot@projected[2,,69]
psi.oldercutblock.s3.s40332<-bestmodel.stage4.boot@projected[2,,70]
psi.oldercutblock.s3.s40341<-bestmodel.stage4.boot@projected[2,,71]
psi.oldercutblock.s3.s40342<-bestmodel.stage4.boot@projected[2,,72]

BBWA.psi.estim<-cbind(year=c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,22,23),
                      psi.unlogged.s1.c40111,
                      psi.unlogged.s1.c40112,
                      psi.unlogged.s1.c40113,
                      psi.unlogged.s1.c40114,
                      psi.unlogged.s1.c40121,
                      psi.unlogged.s1.c40122,
                      psi.unlogged.s1.c40123,
                      psi.unlogged.s1.c40124,
                      
                      psi.unlogged.s2.c40211,
                      psi.unlogged.s2.c40212,
                      psi.unlogged.s2.c40213,
                      psi.unlogged.s2.c40214,
                      psi.unlogged.s2.c40221,
                      psi.unlogged.s2.c40222,
                      psi.unlogged.s2.c40223,
                      psi.unlogged.s2.c40224,
                      
                      psi.unlogged.s3.c40311,
                      psi.unlogged.s3.c40312,
                      psi.unlogged.s3.c40313,
                      psi.unlogged.s3.c40314,
                      psi.unlogged.s3.c40321,
                      psi.unlogged.s3.c40322,
                      psi.unlogged.s3.c40323,
                      psi.unlogged.s3.c40324,
                      
                      psi.newcutblock.s1.cc1_1,
                      psi.newcutblock.s1.cc1_2,
                      psi.newcutblock.s1.cc1_3,
                      psi.newcutblock.s1.cc1_4,
                      psi.newcutblock.s1.cc1_5,
                      psi.newcutblock.s1.cc1_6,
                      psi.newcutblock.s1.cc1_7,
                      psi.newcutblock.s1.cc1_8,
                      
                      psi.newcutblock.s2.cc2_1,
                      psi.newcutblock.s2.cc2_2,
                      psi.newcutblock.s2.cc2_3,
                      psi.newcutblock.s2.cc2_4,
                      psi.newcutblock.s2.cc2_5,
                      psi.newcutblock.s2.cc2_6,
                      psi.newcutblock.s2.cc2_7,
                      psi.newcutblock.s2.cc2_8,
                      
                      psi.newcutblock.s3.cc3_1,
                      psi.newcutblock.s3.cc3_2,
                      psi.newcutblock.s3.cc3_3,
                      psi.newcutblock.s3.cc3_4,
                      psi.newcutblock.s3.cc3_5,
                      psi.newcutblock.s3.cc3_6,
                      psi.newcutblock.s3.cc3_7,
                      psi.newcutblock.s3.cc3_8,
                      
                      psi.oldercutblock.s1.s40111,
                      psi.oldercutblock.s1.s40112,
                      psi.oldercutblock.s1.s40121,
                      psi.oldercutblock.s1.s40122,
                      psi.oldercutblock.s1.s40131,
                      psi.oldercutblock.s1.s40132,
                      psi.oldercutblock.s1.s40141,
                      psi.oldercutblock.s1.s40142,
                      
                      psi.oldercutblock.s2.s40211,
                      psi.oldercutblock.s2.s40212,
                      psi.oldercutblock.s2.s40221,
                      psi.oldercutblock.s2.s40222,
                      psi.oldercutblock.s2.s40231,
                      psi.oldercutblock.s2.s40232,
                      psi.oldercutblock.s2.s40241,
                      psi.oldercutblock.s2.s40242,
                      
                      psi.oldercutblock.s3.s40311,
                      psi.oldercutblock.s3.s40312,
                      psi.oldercutblock.s3.s40321,
                      psi.oldercutblock.s3.s40322,
                      psi.oldercutblock.s3.s40331,
                      psi.oldercutblock.s3.s40332,
                      psi.oldercutblock.s3.s40341,
                      psi.oldercutblock.s3.s40342,
                      SE=bestmodel.stage4.boot@projected.mean.bsse[2,])#how do I get psi estimates
write.csv(BBWA.psi.estim, file="BBWA.psi.estim.allstations.csv") 

BBWA.psi.estim<-read.csv("BBWA.psi.estim.allstations.csv", header=TRUE)
BBWA.psi.estim$lower.unlogged.s1.c40111<-BBWA.psi.estim$psi.unlogged.s1.c40111-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40111<-BBWA.psi.estim$psi.unlogged.s1.c40111+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40112<-BBWA.psi.estim$psi.unlogged.s1.c40112-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40112<-BBWA.psi.estim$psi.unlogged.s1.c40112+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40113<-BBWA.psi.estim$psi.unlogged.s1.c40113-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40113<-BBWA.psi.estim$psi.unlogged.s1.c40113+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40114<-BBWA.psi.estim$psi.unlogged.s1.c40114-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40114<-BBWA.psi.estim$psi.unlogged.s1.c40114+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40121<-BBWA.psi.estim$psi.unlogged.s1.c40121-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40121<-BBWA.psi.estim$psi.unlogged.s1.c40121+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40122<-BBWA.psi.estim$psi.unlogged.s1.c40122-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40122<-BBWA.psi.estim$psi.unlogged.s1.c40122+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40123<-BBWA.psi.estim$psi.unlogged.s1.c40123-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40123<-BBWA.psi.estim$psi.unlogged.s1.c40123+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s1.c40124<-BBWA.psi.estim$psi.unlogged.s1.c40124-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s1.c40124<-BBWA.psi.estim$psi.unlogged.s1.c40124+1.96*BBWA.psi.estim$SE 


BBWA.psi.estim$lower.newcutblock.s1.cc1_1<-BBWA.psi.estim$psi.newcutblock.s1.cc1_1-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_1<-BBWA.psi.estim$psi.newcutblock.s1.cc1_1+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_2<-BBWA.psi.estim$psi.newcutblock.s1.cc1_2-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_2<-BBWA.psi.estim$psi.newcutblock.s1.cc1_2+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_3<-BBWA.psi.estim$psi.newcutblock.s1.cc1_3-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_3<-BBWA.psi.estim$psi.newcutblock.s1.cc1_3+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_4<-BBWA.psi.estim$psi.newcutblock.s1.cc1_4-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_4<-BBWA.psi.estim$psi.newcutblock.s1.cc1_4+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_5<-BBWA.psi.estim$psi.newcutblock.s1.cc1_5-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_5<-BBWA.psi.estim$psi.newcutblock.s1.cc1_5+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_6<-BBWA.psi.estim$psi.newcutblock.s1.cc1_6-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_6<-BBWA.psi.estim$psi.newcutblock.s1.cc1_6+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_7<-BBWA.psi.estim$psi.newcutblock.s1.cc1_7-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_7<-BBWA.psi.estim$psi.newcutblock.s1.cc1_7+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s1.cc1_8<-BBWA.psi.estim$psi.newcutblock.s1.cc1_8-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s1.cc1_8<-BBWA.psi.estim$psi.newcutblock.s1.cc1_8+1.96*BBWA.psi.estim$SE

BBWA.psi.estim$lower.oldercutblock.s1.s40111<-BBWA.psi.estim$psi.oldercutblock.s1.s40111-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40111<-BBWA.psi.estim$psi.oldercutblock.s1.s40111+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40112<-BBWA.psi.estim$psi.oldercutblock.s1.s40112-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40112<-BBWA.psi.estim$psi.oldercutblock.s1.s40112+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40121<-BBWA.psi.estim$psi.oldercutblock.s1.s40121-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40121<-BBWA.psi.estim$psi.oldercutblock.s1.s40121+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40122<-BBWA.psi.estim$psi.oldercutblock.s1.s40122-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40122<-BBWA.psi.estim$psi.oldercutblock.s1.s40122+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40131<-BBWA.psi.estim$psi.oldercutblock.s1.s40131-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40131<-BBWA.psi.estim$psi.oldercutblock.s1.s40131+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40132<-BBWA.psi.estim$psi.oldercutblock.s1.s40132-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40132<-BBWA.psi.estim$psi.oldercutblock.s1.s40132+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40141<-BBWA.psi.estim$psi.oldercutblock.s1.s40141-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40141<-BBWA.psi.estim$psi.oldercutblock.s1.s40141+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s1.s40142<-BBWA.psi.estim$psi.oldercutblock.s1.s40142-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s1.s40142<-BBWA.psi.estim$psi.oldercutblock.s1.s40142+1.96*BBWA.psi.estim$SE

BBWA.psi.estim$lower.unlogged.s2.c40211<-BBWA.psi.estim$psi.unlogged.s2.c40211-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40211<-BBWA.psi.estim$psi.unlogged.s2.c40211+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40212<-BBWA.psi.estim$psi.unlogged.s2.c40212-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40212<-BBWA.psi.estim$psi.unlogged.s2.c40212+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40213<-BBWA.psi.estim$psi.unlogged.s2.c40213-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40213<-BBWA.psi.estim$psi.unlogged.s2.c40213+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40214<-BBWA.psi.estim$psi.unlogged.s2.c40214-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40214<-BBWA.psi.estim$psi.unlogged.s2.c40214+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40221<-BBWA.psi.estim$psi.unlogged.s2.c40221-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40221<-BBWA.psi.estim$psi.unlogged.s2.c40221+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40222<-BBWA.psi.estim$psi.unlogged.s2.c40222-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40222<-BBWA.psi.estim$psi.unlogged.s2.c40222+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40223<-BBWA.psi.estim$psi.unlogged.s2.c40223-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40223<-BBWA.psi.estim$psi.unlogged.s2.c40223+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s2.c40224<-BBWA.psi.estim$psi.unlogged.s2.c40224-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s2.c40224<-BBWA.psi.estim$psi.unlogged.s2.c40224+1.96*BBWA.psi.estim$SE 

BBWA.psi.estim$lower.newcutblock.s2.cc2_1<-BBWA.psi.estim$psi.newcutblock.s2.cc2_1-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_1<-BBWA.psi.estim$psi.newcutblock.s2.cc2_1+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_2<-BBWA.psi.estim$psi.newcutblock.s2.cc2_2-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_2<-BBWA.psi.estim$psi.newcutblock.s2.cc2_2+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_3<-BBWA.psi.estim$psi.newcutblock.s2.cc2_3-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_3<-BBWA.psi.estim$psi.newcutblock.s2.cc2_3+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_4<-BBWA.psi.estim$psi.newcutblock.s2.cc2_4-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_4<-BBWA.psi.estim$psi.newcutblock.s2.cc2_4+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_5<-BBWA.psi.estim$psi.newcutblock.s2.cc2_5-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_5<-BBWA.psi.estim$psi.newcutblock.s2.cc2_5+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_6<-BBWA.psi.estim$psi.newcutblock.s2.cc2_6-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_6<-BBWA.psi.estim$psi.newcutblock.s2.cc2_6+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_7<-BBWA.psi.estim$psi.newcutblock.s2.cc2_7-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_7<-BBWA.psi.estim$psi.newcutblock.s2.cc2_7+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s2.cc2_8<-BBWA.psi.estim$psi.newcutblock.s2.cc2_8-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s2.cc2_8<-BBWA.psi.estim$psi.newcutblock.s2.cc2_8+1.96*BBWA.psi.estim$SE

BBWA.psi.estim$lower.oldercutblock.s2.s40211<-BBWA.psi.estim$psi.oldercutblock.s2.s40211-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40211<-BBWA.psi.estim$psi.oldercutblock.s2.s40211+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40212<-BBWA.psi.estim$psi.oldercutblock.s2.s40212-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40212<-BBWA.psi.estim$psi.oldercutblock.s2.s40212+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40221<-BBWA.psi.estim$psi.oldercutblock.s2.s40221-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40221<-BBWA.psi.estim$psi.oldercutblock.s2.s40221+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40222<-BBWA.psi.estim$psi.oldercutblock.s2.s40222-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40222<-BBWA.psi.estim$psi.oldercutblock.s2.s40222+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40231<-BBWA.psi.estim$psi.oldercutblock.s2.s40231-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40231<-BBWA.psi.estim$psi.oldercutblock.s2.s40231+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40232<-BBWA.psi.estim$psi.oldercutblock.s2.s40232-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40232<-BBWA.psi.estim$psi.oldercutblock.s2.s40232+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40241<-BBWA.psi.estim$psi.oldercutblock.s2.s40241-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40241<-BBWA.psi.estim$psi.oldercutblock.s2.s40241+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s2.s40242<-BBWA.psi.estim$psi.oldercutblock.s2.s40242-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s2.s40242<-BBWA.psi.estim$psi.oldercutblock.s2.s40242+1.96*BBWA.psi.estim$SE

BBWA.psi.estim$lower.unlogged.s3.c40311<-BBWA.psi.estim$psi.unlogged.s3.c40311-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40311<-BBWA.psi.estim$psi.unlogged.s3.c40311+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40312<-BBWA.psi.estim$psi.unlogged.s3.c40312-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40312<-BBWA.psi.estim$psi.unlogged.s3.c40312+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40313<-BBWA.psi.estim$psi.unlogged.s3.c40313-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40313<-BBWA.psi.estim$psi.unlogged.s3.c40313+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40314<-BBWA.psi.estim$psi.unlogged.s3.c40314-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40314<-BBWA.psi.estim$psi.unlogged.s3.c40314+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40321<-BBWA.psi.estim$psi.unlogged.s3.c40321-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40321<-BBWA.psi.estim$psi.unlogged.s3.c40321+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40322<-BBWA.psi.estim$psi.unlogged.s3.c40322-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40322<-BBWA.psi.estim$psi.unlogged.s3.c40322+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40323<-BBWA.psi.estim$psi.unlogged.s3.c40323-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40323<-BBWA.psi.estim$psi.unlogged.s3.c40323+1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$lower.unlogged.s3.c40324<-BBWA.psi.estim$psi.unlogged.s3.c40324-1.96*BBWA.psi.estim$SE 
BBWA.psi.estim$upper.unlogged.s3.c40324<-BBWA.psi.estim$psi.unlogged.s3.c40324+1.96*BBWA.psi.estim$SE 

BBWA.psi.estim$lower.newcutblock.s3.cc3_1<-BBWA.psi.estim$psi.newcutblock.s3.cc3_1-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_1<-BBWA.psi.estim$psi.newcutblock.s3.cc3_1+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_2<-BBWA.psi.estim$psi.newcutblock.s3.cc3_2-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_2<-BBWA.psi.estim$psi.newcutblock.s3.cc3_2+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_3<-BBWA.psi.estim$psi.newcutblock.s3.cc3_3-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_3<-BBWA.psi.estim$psi.newcutblock.s3.cc3_3+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_4<-BBWA.psi.estim$psi.newcutblock.s3.cc3_4-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_4<-BBWA.psi.estim$psi.newcutblock.s3.cc3_4+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_5<-BBWA.psi.estim$psi.newcutblock.s3.cc3_5-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_5<-BBWA.psi.estim$psi.newcutblock.s3.cc3_5+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_6<-BBWA.psi.estim$psi.newcutblock.s3.cc3_6-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_6<-BBWA.psi.estim$psi.newcutblock.s3.cc3_6+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_7<-BBWA.psi.estim$psi.newcutblock.s3.cc3_7-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_7<-BBWA.psi.estim$psi.newcutblock.s3.cc3_7+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.newcutblock.s3.cc3_8<-BBWA.psi.estim$psi.newcutblock.s3.cc3_8-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.newcutblock.s3.cc3_8<-BBWA.psi.estim$psi.newcutblock.s3.cc3_8+1.96*BBWA.psi.estim$SE

BBWA.psi.estim$lower.oldercutblock.s3.s40311<-BBWA.psi.estim$psi.oldercutblock.s3.s40311-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40311<-BBWA.psi.estim$psi.oldercutblock.s3.s40311+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40312<-BBWA.psi.estim$psi.oldercutblock.s3.s40312-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40312<-BBWA.psi.estim$psi.oldercutblock.s3.s40312+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40321<-BBWA.psi.estim$psi.oldercutblock.s3.s40321-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40321<-BBWA.psi.estim$psi.oldercutblock.s3.s40321+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40322<-BBWA.psi.estim$psi.oldercutblock.s3.s40322-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40322<-BBWA.psi.estim$psi.oldercutblock.s3.s40322+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40331<-BBWA.psi.estim$psi.oldercutblock.s3.s40331-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40331<-BBWA.psi.estim$psi.oldercutblock.s3.s40331+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40332<-BBWA.psi.estim$psi.oldercutblock.s3.s40332-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40332<-BBWA.psi.estim$psi.oldercutblock.s3.s40332+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40341<-BBWA.psi.estim$psi.oldercutblock.s3.s40341-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40341<-BBWA.psi.estim$psi.oldercutblock.s3.s40341+1.96*BBWA.psi.estim$SE
BBWA.psi.estim$lower.oldercutblock.s3.s40342<-BBWA.psi.estim$psi.oldercutblock.s3.s40342-1.96*BBWA.psi.estim$SE
BBWA.psi.estim$upper.oldercutblock.s3.s40342<-BBWA.psi.estim$psi.oldercutblock.s3.s40342+1.96*BBWA.psi.estim$SE

my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

library(ggplot2)
Occu <- ggplot(data=BBWA.psi.estim, aes(year, psi.oldercutblock.s1)) +
  geom_line(aes(x=year, y=psi.unlogged.s1.c40111), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40111), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40111), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40112), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40112), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40112), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40113), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40113), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40113), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40114), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40114), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40114), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40121), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40121), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40121), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40122), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40122), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40122), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40123), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40123), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40123), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s1.c40124), colour="lightgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s1.c40124), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s1.c40124), colour="lightgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40111), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40111), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40111), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40112), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40112), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40112), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40121), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40121), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40121), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40122), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40122), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40122), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40131), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40131), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40131), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40132), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40132), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40132), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40141), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40141), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40141), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s1.s40142), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s1.s40142), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s1.s40142), colour="lightblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_1), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_1), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_1), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_2), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_2), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_2), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_3), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_3), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_3), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_4), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_4), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_4), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_5), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_5), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_5), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_6), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_6), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_6), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_7), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_7), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_7), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_8), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s1.cc1_8), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s1.cc1_8), colour="orange",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.unlogged.s2.c40211), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40211), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40211), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40212), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40212), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40212), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40213), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40213), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40213), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40214), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40214), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40214), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40221), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40221), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40221), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40222), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40222), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40222), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40223), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40223), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40223), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s2.c40224), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged.s2.c40224), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s2.c40224), colour="green",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40211), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40211), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40211), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40212), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40212), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40212), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40221), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40221), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40221), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40222), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40222), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40222), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40231), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40231), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40231), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40232), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40232), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40232), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40241), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40241), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40241), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s2.s40242), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s2.s40242), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s2.s40242), colour="blue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_1), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_1), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_1), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_2), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_2), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_2), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_3), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_3), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_3), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_4), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_4), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_4), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_5), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_5), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_5), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_6), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_6), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_6), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_7), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_7), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_7), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_8), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s2.cc2_8), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s2.cc2_8), colour="red",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.unlogged.s3.c40311), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40311), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40311), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40312), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40312), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40312), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40313), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40313), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40313), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40314), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40314), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40314), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40321), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40321), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40321), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40322), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40322), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40322), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40323), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40323), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40323), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.unlogged.s3.c40324), colour="darkgreen", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=upper.unlogged.s3.c40324), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.unlogged.s3.c40324), colour="darkgreen",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40311), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40311), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40311), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40312), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40312), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40312), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40321), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40321), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40321), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40322), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40322), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40322), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40331), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40331), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40331), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40332), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40332), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40332), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40341), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40341), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40341), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.oldercutblock.s3.s40342), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.oldercutblock.s3.s40342), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.oldercutblock.s3.s40342), colour="darkblue",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_1), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_1), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_1), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_2), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_2), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_2), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_3), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_3), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_3), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_4), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_4), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_4), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_5), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_5), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_5), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_6), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_6), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_6), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_7), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_7), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_7), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_8), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=upper.newcutblock.s3.cc3_8), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=lower.newcutblock.s3.cc3_8), colour="darkred",linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  
  ggtitle("BBWA Occupancy Probability Across Time")+
  labs(x="Year", y=expression(psi)) +my.theme+ geom_hline(yintercept=0.5)
Occu
ggsave(filename="BBWA_psi.allstations.png", plot=Occu)

#rm(list=c("null","psi.cut","psi.cut3","psi.series","psi.cutseries","bestmodel.stage1","gameps.yearfactor","gamma.yearfactor","epsilon.yearfactor","gameps.yearxcut","gamma.yearxcut","epsilon.yearxcut","gameps.yearxcut2","gamma.yearxcut2","epsilon.yearxcut2","gameps.ageclass","gamma.ageclass","epsilon.ageclass","bestmodel.stage2","ext.elnino","ext.elninoB","bestmodel.stage3","det.julian","det.time","det.juliantime","bestmodel.stage4"))



chisq <- function(bestmodel.stage4) {
  umf <- getData(bestmodel.stage4)
  y <- getY(umf)
  sr <- bestmodel.stage4@sitesRemoved
  if(length(sr)>0)
    y <- y[-sr,,drop=FALSE]
  fv <- fitted(bestmodel.stage4, na.rm=TRUE)
  y[is.na(fv)] <- NA
  sum((y-fv)^2/(fv*(1-fv)))
}
set.seed(344)
pb.gof <- parboot(bestmodel.stage4, statistic=chisq, nsim=100)
#finished to here as of April 20, 2017


#bestmodel.stage4 for BBWAbird, Red-breasted Nuthatch,Red-eyed Vireo, Swainson's Thrush
new.cutblock<-data.frame(Cut=rep(1,18), Series=3, YearXCut=c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,23), 
                         Year=c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,23), 
                         Year.centered=c(-10.83,-9.83,-7.83,-6.83,-5.83,-4.83,-3.83,-2.83,-1.83,1.17,2.27,4.17,5.17,6.17,7.17,8.17,9.17,11.17),
                         Year.centeredXCut=c(-10.83,-9.83,-7.83,-6.83,-5.83,-4.83,-3.83,-2.83,-1.83,1.17,2.27,4.17,5.17,6.17,7.17,8.17,9.17,11.17),
                         Year.squared=c(117.4,96.7,61.4,46.7,34.0,23.4,14.7,8.0,3.4,1.4,4.7,17.4,26.7,38.0,51.4,66.7,84.0,124.7),
                         Year.squaredXCut=c(117.4,96.7,61.4,46.7,34.0,23.4,14.7,8.0,3.4,1.4,4.7,17.4,26.7,38.0,51.4,66.7,84.0,124.7),
                         Age.class=c("0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","2","2","2"), 
                         elnino.DJF.0=c(0.1,0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.9,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5,2.2), 
                         elnino.DJF.1=c(0.2,0.1,-0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.7,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5))
E.psi.newcutblock <- predict(bestmodel.stage4, type='psi', newdata=new.cutblock, appendData=TRUE)
unlogged<-data.frame(Cut=0, Age.class=c("0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","2","2","2"), elnino.DJF.0=c(0.1,0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.9,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5,2.2), elnino.DJF.1=c(0.2,0.1,-0.9,-0.5,2.1,-1.4,-1.6,-0.7,-0.2,0.7,-0.7,0.7,-0.7,1.3,-1.3,-0.7,-0.4,-0.5))
E.psi.unlogged <- predict(bestmodel.stage4, type='psi', newdata=unlogged, appendData=TRUE)
#predicts initial probability of occupancy



fx.elnino<-data.frame(Cut=rep(1,72), Series=rep("S3",72), YearXCut=rep(c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,23),4), 
                      Year=rep(c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,23),4), 
                      Year.centered=rep(c(-10.83,-9.83,-7.83,-6.83,-5.83,-4.83,-3.83,-2.83,-1.83,1.17,2.27,4.17,5.17,6.17,7.17,8.17,9.17,11.17),4),
                      Year.centeredXCut=rep(c(-10.83,-9.83,-7.83,-6.83,-5.83,-4.83,-3.83,-2.83,-1.83,1.17,2.27,4.17,5.17,6.17,7.17,8.17,9.17,11.17),4),
                      Year.squared=rep(c(117.4,96.7,61.4,46.7,34.0,23.4,14.7,8.0,3.4,1.4,4.7,17.4,26.7,38.0,51.4,66.7,84.0,124.7),4),
                      Year.squaredXCut=rep(c(117.4,96.7,61.4,46.7,34.0,23.4,14.7,8.0,3.4,1.4,4.7,17.4,26.7,38.0,51.4,66.7,84.0,124.7),4),
                      Age.class=rep(c(rep("0",18),rep("1",18),rep("2",18),rep("3",18))),
                      elnino.DJF.0=rep(c(-1.5,-1.3,-1.1,-0.92,-0.72,-0.52,-0.3,-0.14,0.06,0.25,0.44,0.64,0.83,1.03,1.22,1.42,1.61,1.8),4),
                      elnino.DJF.1=rep(c(-1.69,-1.5,-1.3,-1.1,-0.92,-0.72,-0.52,-0.3,-0.14,0.06,0.25,0.44,0.64,0.83,1.03,1.22,1.42,1.61),4)) 
E.ext <- predict(bestmodel.stage4, type='ext', newdata=fx.elnino)
E.col <- predict(bestmodel.stage4, type='col', newdata=fx.elnino)
fx.julian <- data.frame(Julian=c(136,146,156,166,176,186), Time=5)
E.det.julian <- predict(bestmodel.stage4, type='det', newdata=fx.julian)
fx.time <- data.frame(Julian=156, Time=c(5,6,7,8,9,10))
E.det.time <- predict(bestmodel.stage4, type='det', newdata=fx.time)

#predicts probabilities of extinction, colonization, and detection 

#now plot predicted values
#library(jpeg)
#mypath1 <- file.path("C:","Rplots","MSOMapril2017",paste0(i, "_MSOM.jpg", sep = "")) 
#mypath1 <- file.path("C:","Rplots","MSOMapril2017","BBWA_MSOM.jpg") 
#jpeg(file=mypath1) 

op <- par(mfrow=c(2,2), mai=c(1.02, 0.82, 0.82, 0.42))  # 0.6, 0.6, 0.1, 0.1
with(E.ext, { # Plot for extinction probability
  plot(1:72, Predicted, pch=1, xaxt='n', xlab='El Nino index',
       ylab=expression(paste('Extinction probability ( ', epsilon, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:72, labels=fx.elnino$elnino.DJF.0[1:72])       #72 El Nino measurements used in prediction dataset
  axis(3, at=c(5,25,45,65), labels=c("YClass0","YClass1","YClass2","YClass3"),cex.lab=0.5)#, pos=, lty=, col=, las=, tck=, ...)       #11 El Nino measurements used in prediction dataset
  arrows(1:72, lower, 1:72, upper, code=3, angle=90, length=0.03, col=4)
  #points((1:72)-0.1, 1-phi, col=1, lwd = 1, pch=16) #figure out where I get phi from  
  #legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
  #cex=0.8)
})
with(E.col, { # Plot for colonization probability
  plot(1:72, Predicted, pch=1, xaxt='n', xlab='El Nino index',
       ylab=expression(paste('Colonization probability ( ', gamma, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:72, labels=fx.elnino$elnino.DJF.0[1:72])        #72 values used in prediction dataset
  axis(3, at=c(5,25,45,65), labels=c("YClass0","YClass1","YClass2","YClass3"),cex.lab=0.5)#, pos=, lty=, col=, las=, tck=, ...)       #11 El Nino measurements used in prediction dataset
  arrows(1:72, lower, 1:72, upper, code=3, angle=90, length=0.03, col=4)
  #points((1:72)-0.1, gamma, col=1, lwd = 1, pch=16) #figure out where I get gamma from
  #legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
  #cex=0.8)
})
p<-E.det.julian$Predicted  #Where do I get p from?
with(E.det.julian, { # Plot for detection probability 
  plot(1:6, Predicted, pch=1, xaxt='n', xlab='Julian Date',
       ylab=expression(paste('Detection probability ( ', p, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:6, labels=fx.julian$Julian)        #6 dates used in prediction dataset
  arrows(1:6, lower, 1:6, upper, code=3, angle=90, length=0.03, col=4)
  #points((1:15)-0.1, p, col=1, lwd = 1, pch=16) #figure out where I get p from
  legend(7.5, 1, c('Parameter','Estimate'), col=c(1,4), pch=c(16, 1),
         cex=0.8)
})
p<-E.det.time$Predicted  #Where do I get p from?
with(E.det.time, { # Plot for detection probability 
  plot(1:6, Predicted, pch=1, xaxt='n', xlab='Time (0500-1000 hrs)',
       ylab=expression(paste('Detection probability ( ', p, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:6, labels=fx.time$Time)        #6 dates used in prediction dataset
  arrows(1:6, lower, 1:6, upper, code=3, angle=90, length=0.03, col=4)
  #points((1:15)-0.1, p, col=1, lwd = 1, pch=16) #figure out where I get p from
  legend(7.5, 1, c('Parameter','Estimate'), col=c(1,4), pch=c(16, 1),
         cex=0.8)
})
par(op)
#dev.off()






#Modelling for multiple species
names = c("ALFL","AMRE","AMRO","BBWA","BOCH","BOCH","BRCR","BBWA","BBWA","CMWA","CONW","DEJU","GCKI","GRAJ","HAWO","MAWA","BBWA","LISP","MAWA","MOWA","BBWA","RBGR","RBNU","RCKI","REVI","RUGR","SWSP","BBWA","TEWA","WAVI","WETA","WIWR","WTSP","YBSA","YEWA","YRWA")
names   ## Gives a sequence of the names of the species in Calling Lake cutblocks 
#"AMRE","ALFL",

for(i in names){
data<-read.csv(paste0("recast.",i,".csv"),header=TRUE)
y<-data[,c(2:77)]
S <- nrow(data) # number of sites  (9)
J <- 4 # number of secondary sampling occasions    (4 per year)
T <- 19 # number of primary periods    (19 years of observations over 23 years)

bird<-unmarkedMultFrame(y=y,siteCovs=sitecovs,numPrimary=19,yearlySiteCovs=yearly.cov,
                        obsCovs=list(Julian=recast.julian[,c(2:77)], Time=recast.time[,c(2:77)],
                                     jcen=recast.jcen[,c(2:77)], jcen2=recast.jcen2[,c(2:77)], 
                                     tcen=recast.tcen[,c(2:77)], tcen2=recast.tcen2[,c(2:77)]))

null<-try(colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #null model
psi.cut<-try(colext(psiformula = ~Cut, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cut3<-try(colext(psiformula = ~Cut3class, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.series<-try(colext(psiformula = ~Series, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cutseries<-try(colext(psiformula = ~Cut+Series, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested
psi.cut3series<-try(colext(psiformula = ~Cut3class+Series, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=bird))    #initial site occupancy varies between cutblocks and unharvested

results.stage1 <- model.sel('Null model' = null,'Occu:cutblocks'=psi.cut, 'Occu:newvsoldcut'=psi.cut3, 'Occu:Series'=psi.series, 'Occu:Cut+Series'=psi.cutseries, 'Occu:newvsoldcut+Series'=psi.cut3series, rank=AIC)           
bestmodel.stage1<-get.models(results.stage1, subset = 1)[[1]]


fms <- fitList('Null model' = null,'Occu:cutblocks'=psi.cut, 'Occu:newvsoldcut'=psi.cut3, 'Occu:Series'=psi.series, 'Occu:Cut+Series'=psi.cutseries, 'Occu:newvsoldcut+Series'=psi.cut3series)          
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file=paste0(i,".stage1modeltable.csv"))

gameps.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= ~Age.class, epsilonformula= ~Age.class, pformula= ~1, data=bird))
gamma.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= ~Age.class, epsilonformula= ~1, pformula= ~1, data=bird))
epsilon.ageclass<-try(colext(psiformula =bestmodel.stage1@psiformula, gammaformula= ~1, epsilonformula= ~Age.class, pformula= ~1, data=bird))
gameps.yearxcut<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year+Cut+YearXCut, epsilonformula = ~Year+Cut+YearXCut, pformula = ~1, data=bird))   
gamma.yearxcut<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year+Cut+YearXCut, epsilonformula = ~1, pformula = ~1, data=bird))   
epsilon.yearxcut<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~1, epsilonformula = ~Year+Cut+YearXCut, pformula = ~1, data=bird))   
gameps.yearxcut2<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut, epsilonformula = ~Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut, pformula = ~1, data=bird))   
gamma.yearxcut2<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut, epsilonformula = ~1, pformula = ~1, data=bird))   
epsilon.yearxcut2<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~1, epsilonformula = ~Year.centered+Cut+Year.centeredXCut+Year.squared+Year.squaredXCut, pformula = ~1, data=bird))   
#gameps.yearfactor<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year.factor, epsilonformula = ~Year.factor, pformula = ~1, data=bird))   
#gamma.yearfactor<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~Year.factor, epsilonformula = ~1, pformula = ~1, data=bird))   
#epsilon.yearfactor<-try(colext(psiformula = bestmodel.stage1@psiformula, gammaformula = ~1, epsilonformula = ~Year.factor, pformula = ~1, data=bird))   
results.stage2 <- model.sel('Best Stage 1'= bestmodel.stage1, 'Age class - Gamma Eps' = gameps.ageclass, 'Age class - Gamma' = gamma.ageclass, 'Age class - Epsilon' = epsilon.ageclass, 'YearXCut - Gamma Eps'=gameps.yearxcut, 'YearXCut - Gamma'=gamma.yearxcut, 'YearXCut - Epsilon'=epsilon.yearxcut, 'YearXCut2 - Gamma Eps'=gameps.yearxcut2, 'YearXCut2 - Gamma'=gamma.yearxcut2, 'YearXCut2 - Epsilon'=epsilon.yearxcut2, rank=AIC)           
bestmodel.stage2<-get.models(results.stage2, subset = 1)[[1]]

fms <- fitList('Best Stage 1'= bestmodel.stage1, 'Age class - Gamma Eps' = gameps.ageclass, 'Age class - Gamma' = gamma.ageclass, 'Age class - Epsilon' = epsilon.ageclass, 'YearXCut - Gamma Eps'=gameps.yearxcut, 'YearXCut - Gamma'=gamma.yearxcut, 'YearXCut - Epsilon'=epsilon.yearxcut, 'YearXCut2 - Gamma Eps'=gameps.yearxcut2, 'YearXCut2 - Gamma'=gamma.yearxcut2, 'YearXCut2 - Epsilon'=epsilon.yearxcut2)           
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file=paste0(i,".stage2modeltable.csv"))

ext.elnino<-try(colext(psiformula = bestmodel.stage2@psiformula, gammaformula = bestmodel.stage2@gamformula, epsilonformula = update(bestmodel.stage2@epsformula, ~.+elnino.DJF.0), pformula = ~1, data=bird))     
ext.elninoXcut<-try(colext(psiformula = bestmodel.stage2@psiformula, gammaformula = bestmodel.stage2@gamformula, epsilonformula = update(bestmodel.stage2@epsformula, ~.+elnino.DJF.0+elnino.0Xcut), pformula = ~1, data=bird))     

ext.elninoB<-try(colext(psiformula = bestmodel.stage2@psiformula, gammaformula = bestmodel.stage2@gamformula, epsilonformula = update(bestmodel.stage2@epsformula, ~.+elnino.DJF.0+elnino.DJF.1), pformula = ~1, data=bird))     
ext.elninoBXcut<-try(colext(psiformula = bestmodel.stage2@psiformula, gammaformula = bestmodel.stage2@gamformula, epsilonformula = update(bestmodel.stage2@epsformula, ~.+elnino.DJF.0+elnino.DJF.1+elnino.0Xcut+elnino.1Xcut), pformula = ~1, data=bird))     

results.stage3 <- model.sel('ext.elninoXcut'=ext.elninoXcut, 'ext.elninoBXcut'=ext.elninoBXcut, 'Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'El Nino - 1 year' = ext.elnino, 'El Nino - 2 years' = ext.elninoB, rank=AIC)           
bestmodel.stage3<-get.models(results.stage3, subset = 1)[[1]]

fms <- fitList('ext.elninoXcut'=ext.elninoXcut, 'ext.elninoBXcut'=ext.elninoBXcut, 'Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'El Nino - 1 year' = ext.elnino, 'El Nino - 2 years' = ext.elninoB)
ms<-modSel(fms)
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file=paste0(i,".stage3modeltable.csv"))

det.julian<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Julian, data=bird))     
det.julian.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~jcen+jcen2, data=bird))     
det.time<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Time, data=bird))     
det.time.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~tcen+tcen2, data=bird))     
det.juliantime<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~jcen+jcen2+Time, data=bird))     
#det.juliantime.sq<-try(colext(psiformula = bestmodel.stage3@psiformula, gammaformula = bestmodel.stage3@gamformula, epsilonformula = bestmodel.stage3@epsformula, pformula = ~Julian+tcen+tcen2, data=bird))     
results.stage4 <- model.sel('Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'Best Stage 3'= bestmodel.stage3, 'Julian' = det.julian, 'Time' = det.time, 'Julian.sq' = det.julian.sq, 'Time.sq' = det.time.sq, 'Julian+Time' = det.juliantime, 'Julian.sq+Time.sq' = det.juliantime.sq, rank=AIC)           
bestmodel.stage4<-get.models(results.stage4, subset = 1)[[1]]

fms <- fitList('Best Stage 1'= bestmodel.stage1, 'Best Stage 2'= bestmodel.stage2, 'Best Stage 3'= bestmodel.stage3, 'Julian' = det.julian, 'Time' = det.time, 'Time.sq' = det.time.sq, 'Julian+Time' = det.juliantime, 'Julian.sq+Time.sq' = det.juliantime.sq)
ms<-modSel(fms)#'Julian.sq' = det.julian.sq, 
ms.coef<-coef(ms)
ms.SE<-SE(ms)
output<-as(ms,"data.frame")
write.csv(output, file=paste0(i,".stage4modeltable.csv"))

bestmodel.stage4.boot<-nonparboot(bestmodel.stage4, B=100)

psi.unlogged<-bestmodel.stage4.boot@projected[2,,1]#rows 1-3 are for unlogged sites
psi.newcutblock<-bestmodel.stage4.boot@projected[2,,4]#rows 4-6 are for new cutblocks in 1994
psi.oldercutblock<-bestmodel.stage4.boot@projected[2,,7]#rows 7-9 are for old cutblocks in 1994 (new in 1984)
psi.estim<-cbind(year=c(1,2,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,22,23), psi.unlogged, psi.newcutblock, psi.oldercutblock, SE=bestmodel.stage4.boot@projected.mean.bsse[2,])
write.csv(psi.estim, file=paste0(i,".psi.estim.csv")) 
psi.estim<-read.csv(paste0(i,".psi.estim.csv"), header=TRUE)
psi.estim$lower.unlogged<-psi.estim$psi.unlogged-1.96*psi.estim$SE 
psi.estim$upper.unlogged<-psi.estim$psi.unlogged+1.96*psi.estim$SE 
psi.estim$lower.newcutblock<-psi.estim$psi.newcutblock-1.96*psi.estim$SE
psi.estim$upper.newcutblock<-psi.estim$psi.newcutblock+1.96*psi.estim$SE
psi.estim$lower.oldercutblock<-psi.estim$psi.oldercutblock-1.96*psi.estim$SE
psi.estim$upper.oldercutblock<-psi.estim$psi.oldercutblock+1.96*psi.estim$SE


Occu <- ggplot(data=psi.estim, aes(year, psi.oldercutblock)) +
  geom_line(aes(x=year, y=psi.unlogged), colour="green")+ 
  geom_line(aes(x=year, y=upper.unlogged), colour="green",linetype="dashed")+
  geom_line(aes(x=year, y=lower.unlogged), colour="green",linetype="dashed")+
  geom_line(aes(x=year, y=psi.oldercutblock), colour="blue")+
  geom_line(aes(x=year, y=upper.oldercutblock), colour="blue",linetype="dashed")+
  geom_line(aes(x=year, y=lower.oldercutblock), colour="blue",linetype="dashed")+
  geom_line(aes(x=year, y=psi.newcutblock), colour="red")+
  geom_line(aes(x=year, y=upper.newcutblock), colour="red",linetype="dashed")+
  geom_line(aes(x=year, y=lower.newcutblock), colour="red",linetype="dashed")+
  ggtitle(paste0(i, " Occupancy Probability Across Time"))+
  labs(x="Year", y=expression(psi))+
  geom_jitter(width = 0, height = 0.01) 

ggsave(filename=paste0(i,"_psi.pdf"), plot=Occu)

rm(list=c("null","psi.cut","psi.cut3","psi.series","psi.cutseries","psi.cut3series","bestmodel.stage1","gameps.yearfactor","gamma.yearfactor","epsilon.yearfactor","gameps.yearxcut","gamma.yearxcut","epsilon.yearxcut","gameps.yearxcut2","gamma.yearxcut2","epsilon.yearxcut2","gameps.ageclass","gamma.ageclass","epsilon.ageclass","bestmodel.stage2","ext.elnino","ext.elninoB","ext.elninoXcut","ext.elninoBXcut","bestmodel.stage3","det.julian","det.time","det.juliantime","det.julian.sq","det.time.sq","det.juliantime.sq","bestmodel.stage4"))

}




