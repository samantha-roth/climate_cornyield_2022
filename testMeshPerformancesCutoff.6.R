#test performance with AIC, BIC

rm(list=ls())

library(foreach)
library(doParallel)

#load data
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

cs.obs.train<- c(0,cs.trsizes)
cs.obs.test<- c(0,cs.sizes)

#small cutoff
cutoff=.6

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_AMatCV_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",cutoff,".RData",sep=""))


mBaseList<- list()
for(i in 1:(nyrs-1)){
  mBaseList[[i]]<- AMatList[[i]]%*%MoransOperatorEig$vectors
}

dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))
#dimSeq<- seq(50,300,by=25)
#dimSeq<- seq(50,225,by=25)
#dimSeq<- seq(250,300,by=25)

#AIC<- rep(NA, length(dimSeq))
#BIC<- rep(NA, length(dimSeq))
#RMSE<- rep(NA, length(dimSeq))

#betaMatList<-list() # Contains Beta Parameters
#getRid<- RemoveZeroCols(XMat)
#getRid2<- RemoveZeroCols(XMatCV)
#getRidAll<- unique(c(getRid,getRid2))
#XMat<- XMat[,-getRid] #XMat<- XMat[,-getRidAll]
#XMatCV<- XMatCV[,-getRid] #XMatCV<- XMatCV[,-getRidAll]


#for(j in 1:ncol(XMat)){betaMatList[[j]]<-matrix(NA,nrow=length(dimSeq),ncol=3)}

#setup parallel backend to use many processors
cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)


foreach(k = 1:length(dimSeq))%dopar%{
  #for(k in 1:length(dimSeq)){
  if(dimSeq[k]%%50==0){print(dimSeq[k])}
  keepM<-1:dimSeq[k]
  
  time1 <- Sys.time()
  #mBase<- mBaseFull[,keepM]
  #mBaseCV<- mBaseCVFull[,keepM]
  
  #XMat<- as.data.frame(XMat)
  
  #mBaseAllYrs<- matrix(0, nrow= nrow(XMat), ncol= (nyrs-1)*ncol(mBase))
  mBaseAllYrs<- matrix(0, nrow= nrow(XMat), ncol= (nyrs-1)*dimSeq[k])
  for(i in 1:(nyrs-1)){
    mB<- mBaseList[[i]]
    mBaseAllYrs[(cs.obs.train[i]+1):cs.obs.train[i+1],((i-1)*dimSeq[k]+1):(i*dimSeq[k])]<- mB[,keepM]
  }
  
  #mbXM<- cbind(as.data.frame(as.matrix(mBaseFull[,keepM])),XMat[,-1]) #get rid of column of ones
  #mbXM<- as.data.frame(cbind(mBaseAllYrs,XMat[,-1])) #get rid of column of ones
  XMB<- cbind(XMat[,-1],mBaseAllYrs)
  #mbXM<- XMmbFull[,keep]
  #lm1<-glm(obsModLinear~.,data=mbXM,family = "gaussian")
  lm1<-lm(obsModLinear~.,data=as.data.frame(XMB))
  AIC<- AIC(lm1)
  BIC<- BIC(lm1)
  RMSE<- sqrt(mean(lm1$residuals^2))
  #coeffs<-lm1$coefficients
  #estMean<-coeffs[1:ncol(XMat)]
  #lowCI<-estMean-1.975*sqrt(diag(vcov(lm1)))[1:ncol(XMat)]
  #highCI<-estMean+1.975*sqrt(diag(vcov(lm1)))[1:ncol(XMat)]
  
  #for(l in 1:length(betaMatList)){betaMatList[[l]][k,]<-rbind(estMean,lowCI,highCI)[,l]}
  #XMatCV<- as.data.frame(XMatCV)
  #for(col in st1:st54) XMatCV[,col]<- as.factor(XMatCV[,col])
  
  #mbXMCV<- cbind(mBaseCV,XMatCV[,-1])
  #mbXMCV<- XMCVmbFull[,keep]
  #predCV<- predict(lm1,mbXMCV)
  #predCV2<- cbind(1,as.matrix(mbXMCV))%*%coeffs
  #CVMSE[k]<-mean((predCV-obsCVLinear)^2)
  time <- Sys.time() - time1
  time
  
  save(AIC,BIC,RMSE,time,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
}


# Select Rank of Moran's Basis Functions
# We choose the rank that yields the lowest CVMSPE based on the above
#pBaseAIC<-dimSeq[which.min(AIC)]
#pBaseBIC<-dimSeq[which.min(BIC)]
#pBaseRMSE<-dimSeq[which.min(RMSE)]

#save(AIC,BIC,RMSE,time,pBaseAIC,pBaseBIC,pBaseRMSE,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/pickP_AICBIC_cutoff",cutoff,".RData",sep=""))



stopCluster(cl)

################################################################################
cutoff=.6
#dimSeq<- c(seq(5,45,by=5))
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

AICvec<- rep(NA, length(dimSeq))
BICvec<- rep(NA, length(dimSeq))
RMSEvec<- rep(NA, length(dimSeq))

for(k in 1:length(dimSeq)){
  load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AICBICRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  AICvec[k]<- AIC; BICvec[k]<- BIC; RMSEvec[k]<- RMSE
  print(paste("AIC: ",AIC,sep="")); print(paste("BIC: ",BIC,sep=""))
}

pBaseAIC<-dimSeq[which.min(AICvec)]
pBaseBIC<-dimSeq[which.min(BICvec)]
pBaseRMSE<-dimSeq[which.min(RMSEvec)]

plot(dimSeq,AICvec,main=paste("Cutoff=",cutoff,": rank(M) vs AIC",sep=""),xlab="rank(M)",ylab="AIC")
plot(dimSeq,BICvec,main=paste("Cutoff=",cutoff,": rank(M) vs BIC",sep=""),xlab="rank(M)",ylab="BIC")
plot(dimSeq,RMSEvec,main=paste("Cutoff=",cutoff,": rank(M) vs RMSE",sep=""),xlab="rank(M)",ylab="RMSE")
