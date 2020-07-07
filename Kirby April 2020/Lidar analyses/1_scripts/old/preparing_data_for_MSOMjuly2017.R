#The purpose of this script is to reformat count and observation covariate
#data into a form suitable for incorporating into an occupancy modelling data
#frame in R's unmarked package. Data are converted from long format with one 
#row per station visit and one column per variable to wide format with one
#spreadsheet per variable, one row per station, and one column per visit.

#Once data are recast in wide format, a separate script is necessary for calculating
#spatial weights for each station, to account for the autocorrelation in detection
#and occupancy that almost certainly exists among stations within the same sites.

#1. merge by VISIT, then assign year_round
VISIT<-merge1$VISIT
SITE<-merge1$SITE
SS<-merge1$SS
YYYY<-merge1$YYYY
YEAR<-merge1$YYYY-1993
ROUND<-merge1$ROUND
JULIAN<-merge1$JULIAN
HR<-merge1$HR
MIN<-merge1$MIN
TIME<-merge1$HR+(merge1$MIN/60)

newdf<-data.frame(VISIT,SITE,SS,YYYY,YEAR,ROUND,JULIAN,HR,MIN,TIME)
newdf<-unique(newdf)
Ord1 <- order(newdf$YEAR)
newdf[Ord1,]
write.csv(newdf, file="newdf.csv")    #output check

spp_detected<-read.csv(file="spp_detected.csv", header=TRUE)
newdf<-read.csv(file="newdf.csv", header=TRUE)

merge2<-merge(newdf, spp_detected, by = c("VISIT"), na.rm=TRUE)  #a left outer join
str(merge2)
Ord1 <- order(merge2$YEAR)
merge2[Ord1,]

merge2$yearround<-paste(merge2$YEAR,merge2$ROUND)
write.csv(merge2, "merge2.yearround.csv") #outputcheck


#2. output needs to be "recast so that counts associated with separate visits to same point count are put in separate columns

merge2.yearround<-read.csv(file="merge2.yearround.csv", header=TRUE)      

names = c("NOFL","BHCO","PISI","BLJA","COSN","CORA","OCWA","CEDW","SOSP","LCSP","BHVI","PHVI","CCSP")

for(i in names){ 
spp<-merge2.yearround[,c(i)]  #Species as variable

recasted<-tapply(spp, list(merge2.yearround$SS, merge2.yearround$yearround), max, na.rm=TRUE)
write.csv(recasted, paste0("recast.",i,".csv"))
#write.csv(recasted, "recast.CAWA.csv")
 
rearranged <-recasted[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #39,44:47 #reorders recast columns for each visit in correct chronological order
#reorders recast columns for each visit in correct chronological order
#I think there needs to be same number of intervals each year to run MSOMs in unmarked:years 3, 11, 12, 15 and 22 dropped for this reason
#year 0 dropped because there were no surveys in 1993 at some sites of interest (the cutblocks)
rearranged<-data.frame(rearranged)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.",i,".csv"))  #  
}



recast.julian<-tapply(merge2.yearround$JULIAN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.julian <-recast.julian[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.julian)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.julian.csv"))  #  


merge2.yearround$J.CEN<-scale(merge2.yearround$JULIAN, center=TRUE, scale=FALSE)
merge2.yearround$J.CEN2<-merge2.yearround$J.CEN^2

recast.jcen<-tapply(merge2.yearround$J.CEN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.jcen <-recast.jcen[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)] #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.jcen)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.jcen.csv"))  #  


recast.jcen2<-tapply(merge2.yearround$J.CEN2, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.jcen2 <-recast.jcen2[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.jcen2)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.jcen2.csv"))  #  




recast.time<-tapply(merge2.yearround$TIME, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.time <-recast.time[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.time)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.time.csv"))  #  

  
merge2.yearround$T.CEN<-scale(merge2.yearround$TIME, center=TRUE, scale=FALSE)
merge2.yearround$T.CEN2<-merge2.yearround$T.CEN^2

recast.tcen<-tapply(merge2.yearround$T.CEN, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.tcen <-recast.tcen[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.tcen)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.tcen.csv"))  #  


recast.tcen2<-tapply(merge2.yearround$T.CEN2, list(merge2.yearround$SS, merge2.yearround$yearround), mean, na.rm=TRUE)
rearranged.tcen2 <-recast.tcen2[,c(6:9,58:61,82:85,87:90,92:95,97:100,102:105,107:110,11:14,26:29,31:34,39:40,42:43,45:48,50:53,54:57,63:70,71:73,74:76)]  #reorders recast columns for each visit in correct chronological order
rearranged<-data.frame(rearranged.tcen2)
rearranged$X22.4<-"NA"
rearranged$X23.4<-"NA"
rearranged2<-rearranged[,c("X1.1","X1.2","X1.3","X1.4","X2.1","X2.2","X2.3","X2.4","X4.1","X4.2","X4.3","X4.4",
                           "X5.1","X5.2","X5.3","X5.4","X6.1","X6.2","X6.3","X6.4","X7.1","X7.2","X7.3","X7.4",
                           "X8.1","X8.2","X8.3","X8.4","X9.1","X9.2","X9.3","X9.4","X10.1","X10.2","X10.3","X10.4",
                           "X13.1","X13.2","X13.3","X13.4","X14.1","X14.2","X14.3","X14.4","X16.1","X16.2","X16.3","X16.4",
                           "X17.1","X17.2","X17.3","X17.4","X18.1","X18.2","X18.3","X18.4","X19.1","X19.2","X19.3","X19.4",
                           "X20.1","X20.2","X20.3","X20.4","X21.1","X21.2","X21.3","X21.4","X22.1","X22.2","X22.3","X22.4",
                           "X23.1","X23.2","X23.3","X23.4")]
write.csv(rearranged2, paste0("recast.tcen2.csv"))  #  



