#test performance with CVRMSE
rm(list=ls())

library(foreach)
library(doParallel)
library(fields)

#load the test inds
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/allTestIndsAllYears_nOccur.RData")

#load data
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

rm(CVgridLocation); rm(XMatCV); rm(cs.sizes); rm(test.yrs); rm(distMatCVList)
rm(CVfips); rm(cvInd); rm(obsCVLinear); rm(test_indx); rm(test.sizes); rm(test_data)
rm(ifyouaintfirst); rm(yourelast)

source(file= "/storage/work/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")

nyrs= 35

# Make Mesh using INLA

XMatCV<- XMat[allTestIndsAllYears,]
XMat<-XMat[-allTestIndsAllYears,]

obsCVLinear<- obsModLinear[allTestIndsAllYears]
obsModLinear<- obsModLinear[-allTestIndsAllYears]

allTestSizes<- 0
for(t in 1:length(train.sizes)){
  allTestSizes<- c(allTestSizes,round(train.sizes[t]/10))
}
cumTestSizes<- cumsum(allTestSizes)
totalTestSize<-sum(allTestSizes)

train.sizes<- train.sizes- allTestSizes[-1]
test.sizes<- allTestSizes[-1]

#put a zero in front of the cumulative sum vectors
#to make for loops easier to deal with
cs.obs.train<- c(0,cumsum(train.sizes))
cs.obs.test<- c(0,cumsum(test.sizes))

CVfips<- fips[allTestIndsAllYears]
CVgridLocation<- gridLocation[allTestIndsAllYears,]

trainfips<- fips[-allTestIndsAllYears]
traingridLocation<- gridLocation[-allTestIndsAllYears,]

#identify all unique locations
uniquefips<- unique(fips)
uniquelonlat<- matrix(NA,ncol=2, nrow= length(uniquefips))
for(i in 1:length(uniquefips)){
  inds<- which(fips==uniquefips[i])
  uniquelonlat[i,1]<- gridLocation[inds[1],1] #longitude
  uniquelonlat[i,2]<- gridLocation[inds[1],2] #latitude
}

uniqueCVfips<- unique(CVfips)
uniqueCVlonlat<- matrix(NA,ncol=2, nrow= length(uniqueCVfips))
for(i in 1:length(uniqueCVfips)){
  inds<- which(CVfips==uniqueCVfips[i])
  uniqueCVlonlat[i,1]<- CVgridLocation[inds[1],1] #longitude
  uniqueCVlonlat[i,2]<- CVgridLocation[inds[1],2] #latitude
}


CVdist<- rdist(CVgridLocation)
#View(CVdist)
CVdistvec<-as.vector(CVdist)
CVdistvec<- CVdistvec[which(CVdistvec>0)]
smalldist<- min(CVdistvec)

#biggester cutoff
cutoff=.5

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/AMat_AMatCV_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/MOE_cutoff",cutoff,".RData",sep=""))


mBaseList<- list()
for(i in 1:nyrs){
  mBaseList[[i]]<- AMatList[[i]]%*%MoransOperatorEig$vectors
}

mBaseCVList<- list()
for(i in 1:nyrs){
  mBaseCVList[[i]]<- AMatCVList[[i]]%*%MoransOperatorEig$vectors
}

#dimSeq<- seq(50,300,by=25)
dimSeq<- c(seq(5,45,by=5),seq(50,100,by=25))

#AIC<- rep(NA, length(dimSeq))
#BIC<- rep(NA, length(dimSeq))
CVRMSEvec<- rep(NA, length(dimSeq))

XMatCoeffsList<-list() # Contains Beta Parameters

#for(j in 1:ncol(XMat)){betaMatList[[j]]<-matrix(NA,nrow=length(dimSeq),ncol=3)}

#setup parallel backend to use many processors
cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)

time1 <- Sys.time()
foreach(k = 1:length(dimSeq))%dopar%{
  #for(k in 1:length(dimSeq)){
  if(dimSeq[k]%%50==0){print(dimSeq[k])}
  keepM<-1:dimSeq[k]
  
  mBaseAllYrs<- matrix(0, nrow= nrow(XMat), ncol= nyrs*dimSeq[k])
  for(i in 1:nyrs){
    mB<- mBaseList[[i]]
    mBaseAllYrs[(cs.obs.train[i]+1):cs.obs.train[i+1],((i-1)*dimSeq[k]+1):(i*dimSeq[k])]<- mB[,keepM]
  }
  
  mBaseCVAllYrs<- matrix(0, nrow= nrow(XMatCV), ncol= nyrs*dimSeq[k])
  for(i in 1:nyrs){
    mBCV<- mBaseCVList[[i]]
    mBaseCVAllYrs[(cs.obs.test[i]+1):cs.obs.test[i+1],((i-1)*dimSeq[k]+1):(i*dimSeq[k])]<- mBCV[,keepM]
  }
  
  XMB<- cbind(XMat[,-1],mBaseAllYrs)
  XMBCV<- cbind(XMatCV[,-1],mBaseCVAllYrs)
  lm1<-lm(obsModLinear~.,data=as.data.frame(XMB))
  #  AIC[k]<- AIC(lm1)
  #  BIC[k]<- BIC(lm1)
  #  RMSE[k]<- sqrt(mean(lm1$residuals^2))
  #coeffs<-lm1$coefficients
  XMatCoeffs<- lm1$coefficients
  
  #for(l in 1:length(betaMatList)){betaMatList[[l]][k,]<-rbind(estMean,lowCI,highCI)[,l]}
  #XMatCV<- as.data.frame(XMatCV)
  #for(col in st1:st54) XMatCV[,col]<- as.factor(XMatCV[,col])
  
  predCV<- predict(lm1, as.data.frame(XMBCV))
  #predCV2<- cbind(1,as.matrix(mbXMCV))%*%coeffs
  CVRMSE<-sqrt(mean((predCV-obsCVLinear)^2))
  
  save(CVRMSE,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
  save(XMatCoeffs,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/XMatCoeffs_rank",dimSeq[k],"_cutoff",cutoff,".RData",sep=""))
}

time <- Sys.time() - time1
time
save(time,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/CVRMSE_time_cutoff",cutoff,".RData",sep=""))

# Select Rank of Moran's Basis Functions
# We choose the rank that yields the lowest CVMSPE based on the above
#pBaseAIC<-dimSeq[which.min(AIC)]
#pBaseBIC<-dimSeq[which.min(BIC)]
#pBaseCVRMSE<-dimSeq[which.min(RMSE)]

#save(AIC,BIC,RMSE,time,pBaseAIC,pBaseBIC,pBaseRMSE,file=paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/pickP_AICBIC_cutoff",cutoff,".RData",sep=""))


stopCluster(cl)
