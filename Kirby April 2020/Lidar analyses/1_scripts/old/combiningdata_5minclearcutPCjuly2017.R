#This part of the script extracts specific point count data from the BAM database.
#We are interested in 5-minute, 50-m point counts from Calling Lake (PCODE CL)
#from just nine 40-ha sites (PKEY starting with CL:CC-1, 2 or 3; CL:C-40-, or CL:S-040)

#We are using just the birds within 50 m to reduce the chances of double-counting
#the same birds at 2 different, adjacent stations

options (java.parameters = "-Xmx1024m" )
#library(XLConnect)

#1. Get point counts from cutblocks

spp<-read.csv(file="RQST_LIONEL_93_16_CL_PTCOUNT.csv", header=TRUE)   
#pulls Calling Lake point counts into R  
#204340 obs. of  12 variables
spp_CLCC1<-spp[which(substring(spp$PKEY,1,7)=="CL:CC-1"),]
spp_CLCC2<-spp[which(substring(spp$PKEY,1,7)=="CL:CC-2"),]
spp_CLCC3<-spp[which(substring(spp$PKEY,1,7)=="CL:CC-3"),]
#filters out point counts from younger cutblocks logged in 1994 at Calling Lake 
spp_CLCColder<-spp[which(substring(spp$PKEY,1,8)=="CL:S-040"),]
#filters out point counts from older cutblocks logged in 1984 at Calling Lake  
spp_CLControls<-spp[which(substring(spp$PKEY,1,8)=="CL:C-040"),]
#filters out point counts from 40-ha unharvested areas at Calling Lake 

#This will give us 9 same-sized sites (72 stations) monitored over 24 years

spp_cc<-rbind(spp_CLCC1,spp_CLCC2,spp_CLCC3,spp_CLCColder,spp_CLControls)
#39165 obs. of  12 variables

spp_cc<-spp_cc[which(spp_cc$DURATION==1),]
#filters out species detected during 5-minute point counts
#39107 obs. of  12 variables

#spp_cc$BEH<-ifelse(spp_cc$BEH==6, 1, spp_cc$BEH)   #treats detection behavior 6 (seen and heard) same as detection behavior 1 (heard)

spp_cc1<-spp_cc[which(spp_cc$BEH==1),]
spp_cc6<-spp_cc[which(spp_cc$BEH==6),]
spp_cc16<-rbind(spp_cc1,spp_cc6)
#filters out species detected (either heard or seen and heard during 5-minute point counts, but must be vocalizing)
#37566 obs. of  12 variables
write.csv(spp_cc16,file="spp_cc16.csv") #output check     #37566 obs. of  12 variables

#2. remove birds > 50 m distant
spp_cc16<-read.csv(file="spp_cc16.csv", header=TRUE)
str(spp_cc16)
spp_cc16<-spp_cc16[which(spp_cc16$DISTANCE<3),]
#100-m point count data could be used because if we want to use site as the replicate rather than station,
#and because we are interested in occupancy rather than abundance; double-counting and spatial autocorrelation
#aren't issues then. But 50-m may be necessary if we need to use stations as replicates to get sufficient sample size
#for bootstrapping psi in different years.
str(spp_cc16)
write.csv(spp_cc16,file="spp_cc16.csv") #output check    #17002 obs. of  13 variables

#3. merge "spp_cc16" with other files.
pk<-read.csv(file="RQST_LIONEL_93_16_CL_PKEY.csv", header=TRUE)
#pulls Calling Lake point counts into R  
pk_CLCC1<-pk[which(substring(pk$PKEY,1,7)=="CL:CC-1"),]
pk_CLCC2<-pk[which(substring(pk$PKEY,1,7)=="CL:CC-2"),]
pk_CLCC3<-pk[which(substring(pk$PKEY,1,7)=="CL:CC-3"),]
#filters out point counts from younger cutblocks logged in 1994 at Calling Lake 
pk_CLCColder<-pk[which(substring(pk$PKEY,1,8)=="CL:S-040"),]
#filters out point counts from older cutblocks logged in 1984 at Calling Lake  
pk_CLControls<-pk[which(substring(pk$PKEY,1,8)=="CL:C-040"),]
#filters out point counts from 40-ha unharvested areas at Calling Lake 

pkey<-rbind(pk_CLCC1, pk_CLCC2, pk_CLCC3, pk_CLCColder, pk_CLControls)
write.csv(pkey, file="pkey.csv")

spp_cc16 <- read.csv(file="spp_cc16.csv", header=TRUE)
pkey <- read.csv(file="pkey.csv", header=TRUE)

Ord1 <- order(spp_cc16$PKEY)
spp_cc16[Ord1,]

Ord2 <- order(pkey$PKEY)
pkey[Ord2,]

merge1<-merge(pkey, spp_cc16, by = c("PKEY"), all=TRUE)#all=TRUE ensures that 50-m point counts with no observations of a species are not excluded 
str(merge1)

merge1$VISIT<-paste(merge1$SS,"_",merge1$YYYY,"_",merge1$ROUND)

write.csv(merge1, file = "merge1.csv")

#4. calculate total abundance per species per site visit in each cutblock 
merge1<-read.csv("merge1.csv", header=TRUE)
merge1$ABUND<-is.atomic(merge1$ABUND)
tapply.spp<-tapply(merge1$ABUND, list(merge1$VISIT, merge1$SPECIES), sum, na.rm=TRUE)
write.csv(tapply.spp, file = "tapply.spp.csv")  
#output check. At this point we have abundance per species per station visit.
#We could use this output for mixed models or N-mixture models.
#Since we are doing multi-season occupancy models, we convert abundance to detected
#(0 or 1) and create csv file "spp_detected"


