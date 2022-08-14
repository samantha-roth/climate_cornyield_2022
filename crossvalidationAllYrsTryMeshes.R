#load the cross validation indices from the first 35 years
rm(list=ls())

if(dir.exists("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears")==F){
  dir.create("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears")}

library(fields) ; library(mvtnorm) ; library(INLA)

load("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/allTestIndsAllYears_nOccur.RData")


#load data
load("C:/Climate_CornYield-me/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

rm(CVgridLocation); rm(XMatCV); rm(cs.sizes); rm(test.yrs); rm(distMatCVList)
rm(CVfips); rm(cvInd); rm(obsCVLinear); rm(test_indx); rm(test.sizes); rm(test_data)
rm(ifyouaintfirst); rm(yourelast)

source(file= "C:/Climate_CornYield-me/PICAR/source/sharedFunctions.R")

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

#make cutoff go from .005 to .025

#try_cutoff<- c(seq(from= .005, to= .025, by= .005),.05, .1, .2, .3)

try_cutoff<- c(.05,.1,.2,.3,.4)
#try_min.angle<- c(5,10,15,21)

#20 total combinations

plotMeshYears<- c(1,18,36)
pt<- proc.time()
for(i in 1:length(try_cutoff)){
  #i=length(try_cutoff)-1
  #for(j in 1:length(try_min.angle)){
  #j= length(try_min.angle)
  
  mesh<- inla.mesh.2d(uniquelonlat, #replaced gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],]
                      max.edge=c(1),
                      #min.angle= try_min.angle[j], #smaller minimum triangle angle
                      cutoff = try_cutoff[i], #smaller minimum distance between points
                      offset=c(0.1, 0.1))
  
  DMat<-diag(apply(mesh$graph$vv,1,sum))
  WeightMat<-mesh$graph$vv
  PrecMat<-DMat-WeightMat  
  Nnew<-nrow(WeightMat)
  OrthSpace<-diag(Nnew)-(rep(1,Nnew)%*%t(rep(1,Nnew)))/Nnew
  MoransOperator<-OrthSpace%*%(WeightMat%*%OrthSpace)# Moran's Operator
  # Moran's Basis functions
  MoransOperatorEig<-eigen(MoransOperator) # Eigenvectors of the Moran's Operator
  
  #save(MoransOperatorEig,file=paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",i,"_angle",j,".RData",sep=""))
  save(MoransOperatorEig,file=paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/MOE_cutoff",try_cutoff[i],".RData",sep=""))
  ################################################################################
  ###WHEN LAST YEAR OF DATA HELD OUT####
  ###AND P NOT SELECTED YET
  AMatList<- list()
  AMatCVList<- list()
  
  for(t in 1:nyrs){
    print(t)
      #display mesh and points from each year
      # Figure for Mesh and observation locations 
      #filename<- paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/plot_cfmpw_mesh2_yr",1980+t,".jpeg",sep="")
      # Projector Matrix 
      AMat <- inla.spde.make.A(mesh, loc=traingridLocation[(cs.obs.train[t]+1):cs.obs.train[t+1],])  # model-fitting locations
      AMatList[[t]]<- as.matrix(AMat)
      #wp<-ncol(AMat)
      if(t %in% plotMeshYears){
        #filename<- paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/plot_mesh_cutoff",try_cutoff[i],"_angle",j,"_yr",1980+t,".png",sep="")
        filename<- paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/plot_mesh_cutoff_train_",try_cutoff[i],"_yr",1980+t,".png",sep="")
        jpeg(filename, width = 1000, height = 700)
        par(mfrow=c(1,1),mar=c(2,2,2,2))
        plot(mesh,main="")
        points(x=mesh$loc[,1], y=mesh$loc[,2],col="black",pch=16,cex=0.4)
        points(x=traingridLocation[(cs.obs.train[t]+1):cs.obs.train[t+1],1], 
               y=traingridLocation[(cs.obs.train[t]+1):cs.obs.train[t+1],2],col="blue",pch=16,cex=.8)
        mtext(paste(1980+t," Train Mesh",sep=""),cex=2)
        legend("topright", legend=c("Model Fitting" , "Mesh Vertices"),
               col=c("blue","black"), pch=c(16,16),cex = 0.5)
        dev.off()
      }
      #display mesh and points from each year
      # Figure for Mesh and observation locations 
      #filename<- paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/plot_cfmpw_mesh2_yr",1980+i,".jpeg",sep="")
      AMatCV <- inla.spde.make.A(mesh, loc=CVgridLocation[(cs.obs.test[t]+1):cs.obs.test[t+1],]) # validation locations
      AMatCVList[[t]]<- as.matrix(AMatCV)
      #wpCV<-ncol(AMatCV)
      if(t %in% plotMeshYears){
        #filename<- paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/plot_mesh_cutoff",try_cutoff[i],"_angle",j,"_yr",1980+t,".png",sep="")
        filename<- paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/plot_mesh_cutoff_test_",try_cutoff[i],"_yr",1980+t,".png",sep="")
        jpeg(filename, width = 1000, height = 700)
        par(mfrow=c(1,1),mar=c(2,2,2,2))
        plot(mesh,main="")
        points(x=mesh$loc[,1], y=mesh$loc[,2],col="black",pch=16,cex=0.4)
        points(x=CVgridLocation[(cs.obs.test[t]+1):cs.obs.test[t+1],1], 
               y=CVgridLocation[(cs.obs.test[t]+1):cs.obs.test[t+1],2],col="red",pch=16,cex=.8)
        mtext(paste(1980+t," Test Mesh",sep=""),cex=2)
        legend("topright", legend=c("Validation" , "Mesh Vertices"),
               col=c("red","black"), pch=c(16,16),cex = 0.5)
        dev.off()
      }
  }
  
  # Save Data
  #save(AMatCVList,AMatList,file=paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_AMatCV_cutoff",i,"_angle",j,".RData",sep=""))
  save(AMatCVList,AMatList,file=paste("C:/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/CVdiffyears/AMat_AMatCV_cutoff",try_cutoff[i],".RData",sep=""))
  #}
}

ptFinal<- proc.time()- pt; ptFinal<- ptFinal[3]

