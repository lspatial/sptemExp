

parTemporalBImp=function(allPre_,siteids_,isite_,pol_season_trends_){
  # allPre_=tarPDf; siteids_=siteids; isite_=6;pol_season_trends_=pol_season_trends
  asite=siteids_[isite_]
  asiteMe=allPre_[row.names(allPre_)==as.character(asite),]
  ret=asiteMe
  if(all(is.na(asiteMe))){
    return(ret)
  }
  ndays=ncol(asiteMe)
  days=as.integer(gsub("d","",colnames(asiteMe)))
  trainSet=NA
  for(k in c(1:ndays)){
    aday=paste("d",days[k],sep="")
    if(!is.na(asiteMe[,aday])){
      atrainPnt=data.frame(b0=1,b1=pol_season_trends_$pv1[days[k]],b2=pol_season_trends_$pv2[days[k]],con=log(asiteMe[,aday]))
      if(inherits(trainSet,"logical")){
        trainSet=atrainPnt
      }else{
        trainSet=rbind(trainSet,atrainPnt)
      }
    }
  }
  res=conOpt(pol_season_trends_,trainSet)
  if(inherits(res,"logical")){
    print(paste("Error for ",asite,sep=""))
    return(ret)
  }
  cfs=res
  # cfs=lmodel$coefficients
  conSim = function(x_,pol_season_trends_, cofs_){
    ret=exp(cofs_[1]+cofs_[2]*pol_season_trends_$pv1[x_]+cofs_[3]*pol_season_trends_$pv2[x_])
    names(ret)=as.character(x_)
    return(ret)
  }
  adsim=unlist(lapply(days,conSim,pol_season_trends_=pol_season_trends_,cofs_=cfs))
  for(k in c(1:length(days))){ # k=1
    acol=paste("d",days[k],sep="")
    ret[1,acol]=adsim[k]
  }
  return(ret)
}


inter2conOpt=function(tarPDf,pol_season_trends,ncore=1){
  times=ceiling(nrow(tarPDf)/ncore)
  siteids=as.integer(unique(row.names(tarPDf)))
  for(k in c(1:times)){ #  k=6
    print(paste(k," of ",times,sep=""))
    ifrom=(k-1)*ncore+1
    ito=k*ncore
    if(k==times){
      ito=nrow(tarPDf)
    }
    isites=c(ifrom:ito)
    pcores=ito-ifrom+1
    cl=parallel::makeCluster(pcores)
    doParallel::registerDoParallel(cl)
    ptm=proc.time()
    b_res=foreach::foreach(isites=isites, .combine='rbind', .errorhandling="pass",
                  .packages=c('mgcv','plyr')) %dopar% {
                    parTemporalBImp(tarPDf,siteids,isites,pol_season_trends_=pol_season_trends)}
    parallel::stopCluster(cl)
    proc.time()-ptm
    if(k==1){
      allRes=b_res
    }else{
      allRes=rbind(allRes,b_res)
    }
    rm(list=c("b_res"))
    gc()
  }
  return(allRes)
}

