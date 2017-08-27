

#' @title parATimePredict
#'
#' @description Parallel prediction function
#'
#' @param mdPath : Path of the models files
#'
#' @param  newPnts: New SpatialPointDataFramet to be predicted
#'
#' @param  cols:column names of covariats used in predictions.
#'
#' @param  bnd: BND format of polygons for spatial effect modeling
#'
#' @param  c: CPU core number
#'
#' @param  outPath: Output path to save the results.
#'
#' @param  idF: id field name .
#'
#' @param  ridF: Field name of the regional id
#'
#' @return NULL.  All the predicted values saved int the appointed path.
#'
#' @export parATimePredict
#'
parATimePredict=function(mdPath,newPnts,cols=NA,bnd,c=1,outPath="/tmp",idF="siteid",ridF="rid"){
  # mdPath="/home/lpola/pm25model/t_1_models"; tid=dno;newPnts=pntlayer;
  #  c=5;outPath="/tmp";idF="siteid"; ridF="rid"

  mfiles=list.files(mdPath)
  mfiles=grep("^m_.*.rds$",mfiles,value=TRUE)
  mfiles=mfiles[order(mfiles)]
  ids=as.integer(gsub("m_|.rds","",mfiles))

  pntlayerdt=newPnts@data

  if(!inherits(cols,"logical") || length(cols)>0){
    for(acol in cols){
      pntlayerdt=pntlayerdt[!is.na(pntlayerdt[,acol]),]
    }
  }

  mlen=length(mfiles)
  timesEach=floor(mlen/c)
  mFroms=c()
  mTos=c()
  pc=c
  if(timesEach==0){
    mFroms=c(1:mlen)
    mTos=c(1:mlen)
    pc=mlen
  }else{
    for(i in c(1:c)){
      mFrom=(i-1)*timesEach+1
      mTo=i*timesEach
      if(i==c){
        mTo=mlen
      }
      mFroms=c(mFroms,mFrom)
      mTos=c(mTos,mTo)
    }
  }
  cl=parallel::makeCluster(pc)
  doParallel::registerDoParallel(cl)
  ptm=proc.time()
  res=foreach::foreach(mFroms=mFroms,mTos=mTos, .combine='c') %dopar%
    { perMdPrediction(mdPath,mfiles,ids,mFroms,mTos,bnd,pntlayerdt,outPath,idF,ridF)}
  parallel::stopCluster(cl)
  proc.time()-ptm
  rm(list=c("res","pntlayerdt"))
  gc()
}





