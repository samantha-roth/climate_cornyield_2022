
rm(list=ls())

##############################################################################
cutoff<-.1
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))
#dimSeq<- c(seq(5,45,by=5),seq(50,300,by=25))

AICvec.1<- rep(NA, length(dimSeq))
BICvec.1<- rep(NA, length(dimSeq))
RMSEvec.1<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.1[k]<- AIC; BICvec.1[k]<- BIC; RMSEvec.1[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.1)]
pBaseBIC<-dimSeq[which.min(BICvec.1)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.1)]

plot(dimSeq,AICvec.1,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.1,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.1,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

#BIC selected 40

##############################################################################
cutoff=.2
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.2<- rep(NA, length(dimSeq))
BICvec.2<- rep(NA, length(dimSeq))
RMSEvec.2<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.2[k]<- AIC; BICvec.2[k]<- BIC; RMSEvec.2[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.2)]
pBaseBIC<-dimSeq[which.min(BICvec.2)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.2)]

plot(dimSeq,AICvec.2,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.2,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.2,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

#BIC selected 30

##############################################################################
cutoff=.3
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.3<- rep(NA, length(dimSeq))
BICvec.3<- rep(NA, length(dimSeq))
RMSEvec.3<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.3[k]<- AIC; BICvec.3[k]<- BIC; RMSEvec.3[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.3)]
pBaseBIC<-dimSeq[which.min(BICvec.3)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.3)]

plot(dimSeq,AICvec.3,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.3,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.3,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

#BIC selected 40

################################################################################
cutoff=.4
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.4<- rep(NA, length(dimSeq))
BICvec.4<- rep(NA, length(dimSeq))
RMSEvec.4<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.4[k]<- AIC; BICvec.4[k]<- BIC; RMSEvec.4[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.4)]
pBaseBIC<-dimSeq[which.min(BICvec.4)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.4)]

plot(dimSeq,AICvec.4,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.4,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.4,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

#BIC picks 35

################################################################################
cutoff=.5
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.5<- rep(NA, length(dimSeq))
BICvec.5<- rep(NA, length(dimSeq))
RMSEvec.5<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.5[k]<- AIC; BICvec.5[k]<- BIC; RMSEvec.5[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.5)]
pBaseBIC<-dimSeq[which.min(BICvec.5)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.5)]

plot(dimSeq,AICvec.5,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.5,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.5,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

################################################################################
cutoff=.6
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.6<- rep(NA, length(dimSeq))
BICvec.6<- rep(NA, length(dimSeq))
RMSEvec.6<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.6[k]<- AIC; BICvec.6[k]<- BIC; RMSEvec.6[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.6)]
pBaseBIC<-dimSeq[which.min(BICvec.6)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.6)]

plot(dimSeq,AICvec.6,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.6,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.6,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")


################################################################################
cutoff=.7
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec.7<- rep(NA, length(dimSeq))
BICvec.7<- rep(NA, length(dimSeq))
RMSEvec.7<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec.7[k]<- AIC; BICvec.7[k]<- BIC; RMSEvec.7[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec.7)]
pBaseBIC<-dimSeq[which.min(BICvec.7)]
pBaseRMSE<-dimSeq[which.min(RMSEvec.7)]

plot(dimSeq,AICvec.7,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec.7,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec.7,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")

################################################################################
#save plot
library(ggplot2)
library(RColorBrewer)
#library(terra)
#library(rgdal)

allBICs<- data.frame(
  "BIC"= c(BICvec.1[1:12],BICvec.2[1:12],BICvec.3[1:12],BICvec.4[1:12],BICvec.5[1:12],BICvec.6[1:12],BICvec.7[1:12]),
  "rank"= rep(dimSeq[1:12],7),
  "cutoff"= c(rep(0.1,length(dimSeq[1:12])),rep(0.2,length(dimSeq[1:12])),rep(0.3,length(dimSeq[1:12])),rep(0.4,length(dimSeq[1:12])),rep(0.5,length(dimSeq[1:12])),rep(0.6,length(dimSeq[1:12])),rep(0.7,length(dimSeq[1:12])))
)


jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/BICbyRankAndCutoff.jpeg",sep=""),
     width = 800,height = 600)

ggplot(allBICs,aes(rank,BIC, color=cutoff)) +
  geom_point(alpha=0.5, size=2) +
  scale_color_gradient(low="black", high="red")+
  theme_bw() +
  ggtitle("BIC by rank(M)")+ labs(y="BIC", x="Rank(M)")+
  geom_hline(yintercept=min(allBICs$BIC), linetype="dashed", color = "black") +
  geom_text(x=7, y=14, label="lowest value",color="black",size=8)+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

dev.off()


print(allBICs[which.min(allBICs$BIC),])
#> print(allBICs[which.min(allBICs$BIC),])
#BIC       rank cutoff
#405734.4   30    0.6

#second smallest:
#BIC       rank cutoff
#405798.3   30    0.5
