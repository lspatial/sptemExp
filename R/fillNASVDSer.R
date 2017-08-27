
#' @title fillNASVDSer
#'
#' @description This function is to fill the missed values using SVD.
#'
#' @param dset: The target data frame of time series with many missing values
#'
#' @param idF: Field name of id
#'
#' @param dateF: Field name of date
#'
#' @param valF: Field name of value
#'
#' @param k: The number of principle components to be use
#'
#' @return data frame with the missing values filled
#'
#' @export fillNASVDSer

fillNASVDSer=function(dset,idF="siteid",dateF="date",valF="obs",k=1){
  # dset=shdSeries2014;dStr="siteid";dateF="date";valF="obs";k=1
  sites=unique(dset[,idF])
  dates=unique(dset[,dateF])
  dates=dates[order(dates)]
  for(i in c(1:length(sites))){ # i=1
    asite=sites[i]
    asiteDf=data.frame(date=dates)
    asiteData=dset[which(dset[,idF]==asite),]
    index=match(asiteDf$date,asiteData$date)
    asiteDf[,asite]=asiteData[index,valF]
    if(i==1){
      allSiteDf=data.frame(tmp=asiteDf[,asite])
    }else{
      allSiteDf=cbind(allSiteDf,tmp=asiteDf[,asite])
    }
    colnames(allSiteDf)[ncol(allSiteDf)]=asite
  }

  tmp=bcv::impute.svd(allSiteDf, k)
  allSiteDf_filled=as.data.frame(tmp[["x"]])
  colnames(allSiteDf_filled)=colnames(allSiteDf)
  retDf=dset
  for(i in c(1:length(sites))){ # i=1
    asite=sites[i]
    index=match(retDf[which(retDf[,idF]==asite),dateF],dates)
    retDf[which(retDf[,idF]==asite),valF]=allSiteDf_filled[index,asite]
  }
  return(retDf)
}
