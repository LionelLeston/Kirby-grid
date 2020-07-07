library(unmarked)
library(MuMIn)
library(jpeg)
library(ggplot2)

names<-c("ALFL", "AMRE", "AMRO", "BAWW", "BCCH", "BHCO","BHVI","BLJA","BOCH", "BRCR", "BTNW", "CAWA","CCSP","CEDW","CHSP", "CONW", "CORA","COYE", "DEJU", "GCKI", "GRAJ", "HAWO","HETH", "LCSP","LEFL", "LISP", "MAWA", "MOWA","NOFL","OCWA","OVEN","PHVI","PISI", "RBGR", "RBNU", "RCKI", "REVI","SOSP", "SWSP", "SWTH", "TEWA", "WAVI","WETA","WISN", "WIWR", "WTSP", "YBSA", "YEWA", "YRWA","RUGR")
names<-c("RBGR", "WTSP", "CAWA")
for (i in names){
psi.estim<-read.csv(paste0(i,".psi.estim.allstations.csv"), header=TRUE)

my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

psi.estim$c401<-(psi.estim$psi.unlogged.s1.c40111
                 +psi.estim$psi.unlogged.s1.c40112
                 +psi.estim$psi.unlogged.s1.c40113
                 +psi.estim$psi.unlogged.s1.c40114
                 +psi.estim$psi.unlogged.s1.c40121
                 +psi.estim$psi.unlogged.s1.c40122
                 +psi.estim$psi.unlogged.s1.c40123
                 +psi.estim$psi.unlogged.s1.c40124)/8  
psi.estim$c402<-(psi.estim$psi.unlogged.s2.c40211
                 +psi.estim$psi.unlogged.s2.c40212
                 +psi.estim$psi.unlogged.s2.c40213
                 +psi.estim$psi.unlogged.s2.c40214
                 +psi.estim$psi.unlogged.s2.c40221
                 +psi.estim$psi.unlogged.s2.c40222
                 +psi.estim$psi.unlogged.s2.c40223
                 +psi.estim$psi.unlogged.s2.c40224)/8 
psi.estim$c403<-(psi.estim$psi.unlogged.s3.c40311
                 +psi.estim$psi.unlogged.s3.c40312
                 +psi.estim$psi.unlogged.s3.c40313
                 +psi.estim$psi.unlogged.s3.c40314
                 +psi.estim$psi.unlogged.s3.c40321
                 +psi.estim$psi.unlogged.s3.c40322
                 +psi.estim$psi.unlogged.s3.c40323
                 +psi.estim$psi.unlogged.s3.c40324)/8 

psi.estim$s401<-(psi.estim$psi.oldercutblock.s1.s40111
                 +psi.estim$psi.oldercutblock.s1.s40112
                 +psi.estim$psi.oldercutblock.s1.s40121
                 +psi.estim$psi.oldercutblock.s1.s40122
                 +psi.estim$psi.oldercutblock.s1.s40131
                 +psi.estim$psi.oldercutblock.s1.s40132
                 +psi.estim$psi.oldercutblock.s1.s40141
                 +psi.estim$psi.oldercutblock.s1.s40142)/8  
psi.estim$s402<-(psi.estim$psi.oldercutblock.s2.s40211
                 +psi.estim$psi.oldercutblock.s2.s40212
                 +psi.estim$psi.oldercutblock.s2.s40221
                 +psi.estim$psi.oldercutblock.s2.s40222
                 +psi.estim$psi.oldercutblock.s2.s40231
                 +psi.estim$psi.oldercutblock.s2.s40232
                 +psi.estim$psi.oldercutblock.s2.s40241
                 +psi.estim$psi.oldercutblock.s2.s40242)/8 
psi.estim$s403<-(psi.estim$psi.oldercutblock.s3.s40311
                 +psi.estim$psi.oldercutblock.s3.s40312
                 +psi.estim$psi.oldercutblock.s3.s40321
                 +psi.estim$psi.oldercutblock.s3.s40322
                 +psi.estim$psi.oldercutblock.s3.s40331
                 +psi.estim$psi.oldercutblock.s3.s40332
                 +psi.estim$psi.oldercutblock.s3.s40341
                 +psi.estim$psi.oldercutblock.s3.s40342)/8

psi.estim$cc1<-(psi.estim$psi.newcutblock.s1.cc1_1
                 +psi.estim$psi.newcutblock.s1.cc1_2
                 +psi.estim$psi.newcutblock.s1.cc1_3
                 +psi.estim$psi.newcutblock.s1.cc1_4
                 +psi.estim$psi.newcutblock.s1.cc1_5
                 +psi.estim$psi.newcutblock.s1.cc1_6
                 +psi.estim$psi.newcutblock.s1.cc1_7
                 +psi.estim$psi.newcutblock.s1.cc1_8)/8  
psi.estim$cc2<-(psi.estim$psi.newcutblock.s2.cc2_1
                 +psi.estim$psi.newcutblock.s2.cc2_2
                 +psi.estim$psi.newcutblock.s2.cc2_3
                 +psi.estim$psi.newcutblock.s2.cc2_4
                 +psi.estim$psi.newcutblock.s2.cc2_5
                 +psi.estim$psi.newcutblock.s2.cc2_6
                 +psi.estim$psi.newcutblock.s2.cc2_7
                 +psi.estim$psi.newcutblock.s2.cc2_8)/8 
psi.estim$cc3<-(psi.estim$psi.newcutblock.s3.cc3_1
                 +psi.estim$psi.newcutblock.s3.cc3_2
                 +psi.estim$psi.newcutblock.s3.cc3_3
                 +psi.estim$psi.newcutblock.s3.cc3_4
                 +psi.estim$psi.newcutblock.s3.cc3_5
                 +psi.estim$psi.newcutblock.s3.cc3_6
                 +psi.estim$psi.newcutblock.s3.cc3_7
                 +psi.estim$psi.newcutblock.s3.cc3_8)/8

#psi.estim$ElNino<-scale(psi.estim$ElNino, scale=TRUE, center=TRUE)  
#psi.estim$SpruceBudwormYBefore<-scale(psi.estim$SpruceBudwormYBefore, scale=TRUE, center=TRUE)  
#psi.estim$TentCaterpillarYBefore<-scale(psi.estim$TentCaterpillarYBefore, scale=TRUE, center=TRUE)  
#psi.estim$newercutblocks<-(psi.estim$cc3+psi.estim$cc2+psi.estim$cc1)/3
#psi.estim$oldercutblocks<-(psi.estim$s403+psi.estim$s402+psi.estim$s401)/3
#psi.estim$controls<-(psi.estim$c403+psi.estim$c402+psi.estim$c401)/3

#controlvsoldcut<-cor(psi.estim$controls,psi.estim$oldercutblocks,use="complete.obs")
#controlvsnewcut<-cor(psi.estim$controls,psi.estim$newercutblocks,use="complete.obs")
#oldcutvsnewcut<-cor(psi.estim$oldercutblocks,psi.estim$newercutblocks,use="complete.obs")
#df<-data.frame(controlvsoldcut,controlvsnewcut,oldcutvsnewcut)
#write.csv(df, file=paste0(i,"yearlycorr.csv"))

Occu <- ggplot(data=psi.estim, aes(year, psi.oldercutblock.s1)) +
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40111), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40112), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40113), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40114), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40121), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40122), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40123), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s1.c40124), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=c401), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40111), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40112), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40121), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40122), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40131), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40132), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40141), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40142), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=s401), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_1), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_2), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_3), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_4), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_5), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_6), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_7), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_8), colour="orange", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=cc1), colour="red", position=position_jitter(w=0.02, h=0.02))+
  
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40211), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40212), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40213), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40214), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40221), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40222), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40223), colour="black")+ 
  #geom_line(aes(x=year, y=psi.unlogged.s2.c40224), colour="black")+ 
  geom_line(aes(x=year, y=c402), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40211), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40212), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40221), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40222), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40231), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40232), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40241), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40242), colour="blue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=s402), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_1), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_2), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_3), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_4), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_5), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_6), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_7), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_8), colour="red", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=cc2), colour="red", position=position_jitter(w=0.02, h=0.02))+
  
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40311), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40312), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40313), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40314), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40321), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40322), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40323), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  #geom_line(aes(x=year, y=psi.unlogged.s3.c40324), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  geom_line(aes(x=year, y=c403), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
  
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40311), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40312), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40321), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40322), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40331), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40332), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40341), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40342), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=s403), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
  
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_1), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_2), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_3), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_4), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_5), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_6), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_7), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_8), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
  geom_line(aes(x=year, y=cc3), colour="red", position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=ElNino), colour="darkgreen", size=2, position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=SpruceBudwormYBefore), colour="violet", size=2, position=position_jitter(w=0.02, h=0.02))+
  #geom_line(aes(x=year, y=TentCaterpillarYBefore), colour="orange", linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
  ggtitle(paste0(i," Occupancy Probability Across Time"))+
  labs(x="year", y=expression(psi)) +my.theme+ geom_hline(yintercept=0.5)
Occu
#ggsave(filename=paste0(i,"_psi.allstations.png"), plot=Occu)
ggsave(filename=paste0("Aug10.2018.",i,"_psi.allstations.averaged.png"), plot=Occu)
#ggsave(filename=paste0(i,"_psi.allstations.averagedINCLYearCovar.png"), plot=Occu)
}

gg_list<-list()
#Multipanel, multispecies plots
names<-c("ALFL", "AMRO", "BBWA", "BHVI", "BLJA", "CHSP", "CONW", "COYE", "GRAJ", "HETH", "LCSP", "LISP", "MOWA", "NOFL", "PHVI", "RBNU", "RUGR", "SOSP", "SWTH", "TEWA", "WAVI","WETA","COSN", "YBSA")
for (i in names){
  psi.estim<-read.csv(paste0(i,".psi.estim.allstations.csv"), header=TRUE)
  
  my.theme <- theme_classic() +
    theme(text=element_text(size=16, family="Arial"),
          axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(margin=margin(10,0,0,0)),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.line.x=element_line(linetype=1),
          axis.line.y=element_line(linetype=1))
  
  psi.estim$c401<-(psi.estim$psi.unlogged.s1.c40111
                   +psi.estim$psi.unlogged.s1.c40112
                   +psi.estim$psi.unlogged.s1.c40113
                   +psi.estim$psi.unlogged.s1.c40114
                   +psi.estim$psi.unlogged.s1.c40121
                   +psi.estim$psi.unlogged.s1.c40122
                   +psi.estim$psi.unlogged.s1.c40123
                   +psi.estim$psi.unlogged.s1.c40124)/8  
  psi.estim$c402<-(psi.estim$psi.unlogged.s2.c40211
                   +psi.estim$psi.unlogged.s2.c40212
                   +psi.estim$psi.unlogged.s2.c40213
                   +psi.estim$psi.unlogged.s2.c40214
                   +psi.estim$psi.unlogged.s2.c40221
                   +psi.estim$psi.unlogged.s2.c40222
                   +psi.estim$psi.unlogged.s2.c40223
                   +psi.estim$psi.unlogged.s2.c40224)/8 
  psi.estim$c403<-(psi.estim$psi.unlogged.s3.c40311
                   +psi.estim$psi.unlogged.s3.c40312
                   +psi.estim$psi.unlogged.s3.c40313
                   +psi.estim$psi.unlogged.s3.c40314
                   +psi.estim$psi.unlogged.s3.c40321
                   +psi.estim$psi.unlogged.s3.c40322
                   +psi.estim$psi.unlogged.s3.c40323
                   +psi.estim$psi.unlogged.s3.c40324)/8 
  
  psi.estim$s401<-(psi.estim$psi.oldercutblock.s1.s40111
                   +psi.estim$psi.oldercutblock.s1.s40112
                   +psi.estim$psi.oldercutblock.s1.s40121
                   +psi.estim$psi.oldercutblock.s1.s40122
                   +psi.estim$psi.oldercutblock.s1.s40131
                   +psi.estim$psi.oldercutblock.s1.s40132
                   +psi.estim$psi.oldercutblock.s1.s40141
                   +psi.estim$psi.oldercutblock.s1.s40142)/8  
  psi.estim$s402<-(psi.estim$psi.oldercutblock.s2.s40211
                   +psi.estim$psi.oldercutblock.s2.s40212
                   +psi.estim$psi.oldercutblock.s2.s40221
                   +psi.estim$psi.oldercutblock.s2.s40222
                   +psi.estim$psi.oldercutblock.s2.s40231
                   +psi.estim$psi.oldercutblock.s2.s40232
                   +psi.estim$psi.oldercutblock.s2.s40241
                   +psi.estim$psi.oldercutblock.s2.s40242)/8 
  psi.estim$s403<-(psi.estim$psi.oldercutblock.s3.s40311
                   +psi.estim$psi.oldercutblock.s3.s40312
                   +psi.estim$psi.oldercutblock.s3.s40321
                   +psi.estim$psi.oldercutblock.s3.s40322
                   +psi.estim$psi.oldercutblock.s3.s40331
                   +psi.estim$psi.oldercutblock.s3.s40332
                   +psi.estim$psi.oldercutblock.s3.s40341
                   +psi.estim$psi.oldercutblock.s3.s40342)/8
  
  psi.estim$cc1<-(psi.estim$psi.newcutblock.s1.cc1_1
                  +psi.estim$psi.newcutblock.s1.cc1_2
                  +psi.estim$psi.newcutblock.s1.cc1_3
                  +psi.estim$psi.newcutblock.s1.cc1_4
                  +psi.estim$psi.newcutblock.s1.cc1_5
                  +psi.estim$psi.newcutblock.s1.cc1_6
                  +psi.estim$psi.newcutblock.s1.cc1_7
                  +psi.estim$psi.newcutblock.s1.cc1_8)/8  
  psi.estim$cc2<-(psi.estim$psi.newcutblock.s2.cc2_1
                  +psi.estim$psi.newcutblock.s2.cc2_2
                  +psi.estim$psi.newcutblock.s2.cc2_3
                  +psi.estim$psi.newcutblock.s2.cc2_4
                  +psi.estim$psi.newcutblock.s2.cc2_5
                  +psi.estim$psi.newcutblock.s2.cc2_6
                  +psi.estim$psi.newcutblock.s2.cc2_7
                  +psi.estim$psi.newcutblock.s2.cc2_8)/8 
  psi.estim$cc3<-(psi.estim$psi.newcutblock.s3.cc3_1
                  +psi.estim$psi.newcutblock.s3.cc3_2
                  +psi.estim$psi.newcutblock.s3.cc3_3
                  +psi.estim$psi.newcutblock.s3.cc3_4
                  +psi.estim$psi.newcutblock.s3.cc3_5
                  +psi.estim$psi.newcutblock.s3.cc3_6
                  +psi.estim$psi.newcutblock.s3.cc3_7
                  +psi.estim$psi.newcutblock.s3.cc3_8)/8
  
  psi.estim$ElNino<-scale(psi.estim$ElNino, scale=TRUE, center=TRUE)  
  #psi.estim$SpruceBudwormYBefore<-scale(psi.estim$SpruceBudwormYBefore, scale=TRUE, center=TRUE)  
  #psi.estim$TentCaterpillarYBefore<-scale(psi.estim$TentCaterpillarYBefore, scale=TRUE, center=TRUE)  
  #psi.estim$newercutblocks<-(psi.estim$cc3+psi.estim$cc2+psi.estim$cc1)/3
  #psi.estim$oldercutblocks<-(psi.estim$s403+psi.estim$s402+psi.estim$s401)/3
  #psi.estim$controls<-(psi.estim$c403+psi.estim$c402+psi.estim$c401)/3
  
  #controlvsoldcut<-cor(psi.estim$controls,psi.estim$oldercutblocks,use="complete.obs")
  #controlvsnewcut<-cor(psi.estim$controls,psi.estim$newercutblocks,use="complete.obs")
  #oldcutvsnewcut<-cor(psi.estim$oldercutblocks,psi.estim$newercutblocks,use="complete.obs")
  #df<-data.frame(controlvsoldcut,controlvsnewcut,oldcutvsnewcut)
  #write.csv(df, file=paste0(i,"yearlycorr.csv"))
  
  gg_list[[i]] <- ggplot(data=psi.estim, aes(year, psi.oldercutblock.s1)) +
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40111), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40112), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40113), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40114), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40121), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40122), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40123), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s1.c40124), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    geom_line(aes(x=year, y=c401), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40111), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40112), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40121), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40122), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40131), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40132), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40141), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s1.s40142), colour="lightblue", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=s401), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_1), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_2), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_3), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_4), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_5), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_6), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_7), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s1.cc1_8), colour="orange", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=cc1), colour="red", position=position_jitter(w=0.02, h=0.02))+
    
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40211), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40212), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40213), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40214), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40221), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40222), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40223), colour="black")+ 
    #geom_line(aes(x=year, y=psi.unlogged.s2.c40224), colour="black")+ 
    geom_line(aes(x=year, y=c402), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40211), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40212), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40221), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40222), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40231), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40232), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40241), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s2.s40242), colour="blue", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=s402), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_1), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_2), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_3), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_4), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_5), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_6), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_7), colour="red", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s2.cc2_8), colour="red", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=cc2), colour="red", position=position_jitter(w=0.02, h=0.02))+
    
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40311), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40312), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40313), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40314), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40321), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40322), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40323), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    #geom_line(aes(x=year, y=psi.unlogged.s3.c40324), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    geom_line(aes(x=year, y=c403), colour="black", position=position_jitter(w=0.02, h=0.02))+ 
    
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40311), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40312), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40321), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40322), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40331), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40332), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40341), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.oldercutblock.s3.s40342), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=s403), colour="darkblue", position=position_jitter(w=0.02, h=0.02))+
    
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_1), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_2), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_3), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_4), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_5), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_6), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_7), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=psi.newcutblock.s3.cc3_8), colour="darkred", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=cc3), colour="red", position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=ElNino), colour="darkgreen", size=2, position=position_jitter(w=0.02, h=0.02))+
    geom_line(aes(x=year, y=SpruceBudwormYBefore), colour="violet", size=2, position=position_jitter(w=0.02, h=0.02))+
    #geom_line(aes(x=year, y=TentCaterpillarYBefore), colour="orange", linetype="dashed", position=position_jitter(w=0.02, h=0.02))+
    ggtitle(paste0(i))+
    labs(x="year", y=expression(psi)) +my.theme+ geom_hline(yintercept=0.5)
}
library(grid)
library(gridExtra)
grid.arrange(gg_list$AMRO, gg_list$CHSP, gg_list$COSN, gg_list$LCSP, gg_list$LISP, gg_list$MOWA, gg_list$NOFL, gg_list$SOSP, gg_list$TEWA, nrow=3, ncol=3)

grid.arrange(gg_list$ALFL, gg_list$BBWA, gg_list$BHVI, gg_list$CONW, gg_list$COYE, gg_list$HETH, gg_list$PHVI, gg_list$SWTH, gg_list$WAVI, nrow=3, ncol=3)

names<-c("BLJA", "GRAJ", "RBNU", "RUGR", "WETA","YBSA")
