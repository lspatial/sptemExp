
#' @title GetARegionBK
#'
#' @description This function is to caltulate block-kriged mean .
#'
#' @param rNames: A vector of regionas names
#'
#' @param rF: Starting index of the regions names whose means to be estimated
#'
#' @param rT: Ending index of the regions names whose means to be estimated
#'
#' @param rlayer: Regional layer (SpatialPolygonDataFrame)
#'
#' @param paras: Parameyters of variogram
#'
#' @param spnts: SpatialPointDataFrame whose points used to make the etimation
#'
#' @param regF: Region field name
#'
#' @param obsF: Observed value to be block-kriged
#'
#' @return A data frame, data format: (region name, block-kriged mean, block-kriged standard deviation)
#'
#' @export GetARegionBK
#'
GetARegionBK=function(rNames,rF,rT,rlayer,paras,spnts,regF,obsF="pre_mf_log"){
  #  rNames=regions;rF=1;rT=3;rlayer;paras=paras;spnts=spointspre;regF;obsF="pre_mf_log"
  for(i in c(rF:rT)){ # i=rF
    aregion=rNames[i]
    rlayera=rlayer[which(rlayer@data[,regF]==aregion),]
    inside.county=which(!is.na(sp::over(spnts, as(rlayera, "SpatialPolygons"))))
    if(length(inside.county)==0){
      ret=data.frame(cnname=aregion,bkmean=NA,bkstd=NA)
    }else{
      spntssub=spnts[inside.county, ]
      index=c()
      for(k in c(1:nrow(spntssub))){
        if((k %% 2)==0) index=c(index,k)
      }
      samplesdata=spntssub[index,]
      tardata=spntssub[-index,]
      samplesdata@data$x=sp::coordinates(samplesdata)[1]
      samplesdata@data$y=sp::coordinates(samplesdata)[2]
      tardata@data$x=sp::coordinates(tardata)[1]
      tardata@data$y=sp::coordinates(tardata)[2]
      samplesdata=samplesdata@data
      tardata=tardata@data
      samplesdata=samplesdata[which(!is.na(samplesdata[,obsF])),]
      tardata=rbind(tardata,samplesdata[which(is.na(samplesdata[,obsF])),])

      aregRes=bkriging(samplesdata, tardata, obsF, paras)
      ret=data.frame(cnname=aregion,bkmean=exp(aregRes[1]),bkstd=exp(aregRes[2]))
    }
    if(i==rF){
      retAll=ret
    }else{
      retAll=rbind(retAll,ret)
    }
  }
  return(retAll)
}
