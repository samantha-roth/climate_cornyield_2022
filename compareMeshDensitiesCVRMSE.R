#Compare cutoffs CVRMSE
rm(list=ls())
##############################################################################
#new version
################################################################################

cutoff<- .7

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.7<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.7[k]<- CVRMSE
}
summary(CVRMSEvec0.7)

################################################################################

cutoff<- .6

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.6<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.6[k]<- CVRMSE
}
summary(CVRMSEvec0.6)

################################################################################

cutoff<- .5

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.5<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.5[k]<- CVRMSE
}
summary(CVRMSEvec0.5)

################################################################################

cutoff<- .4

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.4<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.4[k]<- CVRMSE
}

summary(CVRMSEvec0.4)

################################################################################

cutoff<- .3

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.3<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.3[k]<- CVRMSE
}

################################################################################

cutoff<- .2

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.2<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.2[k]<- CVRMSE
}


################################################################################

cutoff<- .1

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

CVRMSEvec0.1<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  #load(paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.1[k]<- CVRMSE
}

################################################################################
#plot(dimSeq,CVRMSEvec0.1,main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="red")
#points(dimSeq,CVRMSEvec0.2,col="purple")
#points(dimSeq,CVRMSEvec0.3,col="blue")

#up to rank(M)=45
#plot(dimSeq[1:9],CVRMSEvec0.1[1:9],main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="red")
#points(dimSeq[1:9],CVRMSEvec0.2[1:9],col="purple")
#points(dimSeq[1:9],CVRMSEvec0.3[1:9],col="blue")
#points(dimSeq[1:9],CVRMSEvec0.4[1:9],col="orange")

##up to rank(M)=35
#plot(dimSeq[1:7],CVRMSEvec0.3[1:7],main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="blue")
#points(dimSeq[1:7],CVRMSEvec0.2[1:7],col="purple")
#points(dimSeq[1:7],CVRMSEvec0.1[1:7],col="red")
#points(dimSeq[1:7],CVRMSEvec0.4[1:7],col="orange")

################################################################################
#save plot
library(ggplot2)
library(RColorBrewer)

last= 7

allCVRMSEs<- data.frame(
  "CVRMSE"= c(CVRMSEvec0.1[1:last],CVRMSEvec0.2[1:last],CVRMSEvec0.3[1:last],CVRMSEvec0.4[1:last],CVRMSEvec0.5[1:last],CVRMSEvec0.6[1:last],CVRMSEvec0.7[1:last]),
  "rank"= rep(dimSeq[1:last],7),
  "cutoff"= c(rep(0.1,length(dimSeq[1:last])),rep(0.2,length(dimSeq[1:last])),rep(0.3,length(dimSeq[1:last])),rep(0.4,length(dimSeq[1:last])),rep(0.5,length(dimSeq[1:last])),rep(0.6,length(dimSeq[1:last])),rep(0.7,length(dimSeq[1:last])))
  )

print(allCVRMSEs[which.min(allCVRMSEs$CVRMSE),])

ggplot(allCVRMSEs,aes(rank,CVRMSE, color=cutoff)) +
  geom_point(alpha=0.5, size=2) +
  #scale_color_brewer(palette="Purples")+ 
  scale_color_gradient(low="black", high="red")+
  theme_bw() +
  ggtitle("CVRMSE by rank(M)")+ labs(y="CVRMSE", x="Rank(M)")+
  
  #ggplot(nCh.df, aes(x=value, fill=source)) +
  #  geom_density(alpha=.25)+
  geom_hline(yintercept=min(allCVRMSEs$CVRMSE), linetype="dashed", color = "black") +
  geom_text(x=7, y=14, label="lowest value",color="black",size=8)+
  #  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSEbyRankAndCutoff.jpeg",sep=""),
     width = 800, height = 600)

ggplot(allCVRMSEs,aes(rank,CVRMSE, color=cutoff)) +
  geom_point(alpha=0.5, size=2) +
  #scale_color_brewer(palette="Purples")+ 
  scale_color_gradient(low="black", high="red")+
  theme_bw() +
  ggtitle("CVRMSE by rank(M)")+ labs(y="CVRMSE", x="Rank(M)")+
  
#ggplot(nCh.df, aes(x=value, fill=source)) +
#  geom_density(alpha=.25)+
  geom_hline(yintercept=min(allCVRMSEs$CVRMSE), linetype="dashed", color = "black") +
  geom_text(x=7, y=14, label="lowest value",color="black",size=8)+
#  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

dev.off()

################################################################################
#old version

cutoff<- .1

dimSeq<- c(seq(5,45,by=5),seq(50,200,by=25))

CVRMSEvec0.1<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.1[k]<- CVRMSE
}

plot(dimSeq,CVRMSEvec0.1,main=paste("Cutoff= ",cutoff,": rank(M) vs CVRMSE",sep=""))

summary(CVRMSEvec0.1)

##############################################################################

cutoff<- .2

dimSeq<- c(seq(5,45,by=5),seq(50,200,by=25))

CVRMSEvec0.2<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.2[k]<- CVRMSE
}

plot(dimSeq,CVRMSEvec0.2,main=paste("Cutoff= ",cutoff,": rank(M) vs CVRMSE",sep=""))

summary(CVRMSEvec0.2)


##############################################################################

cutoff<- .3

dimSeq<- c(seq(5,45,by=5),seq(50,200,by=25))

CVRMSEvec0.3<- rep(NA,length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  print(CVRMSE)
  CVRMSEvec0.3[k]<- CVRMSE
}

plot(dimSeq,CVRMSEvec0.3,main=paste("Cutoff= ",cutoff,": rank(M) vs CVRMSE",sep=""))

summary(CVRMSEvec0.3)

################################################################################
plot(dimSeq,CVRMSEvec0.1,main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="red")
points(dimSeq,CVRMSEvec0.2,col="purple")
points(dimSeq,CVRMSEvec0.3,col="blue")

#up to rank(M)=50
plot(dimSeq[1:10],CVRMSEvec0.1[1:10],main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="red")
points(dimSeq[1:10],CVRMSEvec0.2[1:10],col="purple")
points(dimSeq[1:10],CVRMSEvec0.3[1:10],col="blue")

#up to rank(M)=35
plot(dimSeq[1:7],CVRMSEvec0.1[1:7],main=paste("All cutoffs: rank(M) vs CVRMSE",sep=""),col="red")
points(dimSeq[1:7],CVRMSEvec0.2[1:7],col="purple")
points(dimSeq[1:7],CVRMSEvec0.3[1:7],col="blue")


#selected:
#CVRMSE   rank cutoff
#13.49953   25    0.5
