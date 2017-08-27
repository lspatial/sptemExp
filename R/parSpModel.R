#' @title parSpModel
#'
#' @description Parallel batch training models.
#'
#' @param tSet: Trained dataset
#'
#' @param bnd: A BND format spatial polygons, see the package of BayesX
#'
#' @param fS: A vector of forumlas strings
#'
#' @param tidF: Temporal field name
#'
#' @param tids: A vector of temporal ids
#'
#' @param c: Number of the CPU corse used in training
#'
#' @param nM: Number of the models to be trained
#'
#' @param mPath: Path to save the models
#'
#' @param idF: Field name of location id
#'
#' @param dateF: Field name of date
#'
#' @param obsF: Fielf name of observed values
#'
#' @return None. The models trained saved in the appointed path.
#'
#'
#' @export parSpModel
#'
#'
parSpModel=function(tSet,bnd,fS,tidF="tid",tids, c=1,nM=30,mPath,idF="siteid",dateF="date",obsF="pm25"){

  slibs=c("R2BayesX","BayesX","BayesXsrc","miscTools","Metrics","lubridate","rgdal","plyr","rgeos")

  tlen=length(tids)
  timesEach=floor(tlen/c)
  tFroms=c()
  tTos=c()
  if(timesEach==0){
    tFroms=c(1:tlen)
    tTos=c(1:tlen)
  }else{
    for(i in c(1:c)){
      tFrom=(i-1)*timesEach+1
      tTo=i*timesEach
      if(i==c){
        tTo=tlen
      }
      tFroms=c(tFroms,tFrom)
      tTos=c(tTos,tTo)
    }
  }
  pc=c
  cl=parallel::makeCluster(pc)
  doParallel::registerDoParallel(pc)
  ptm=proc.time()
  res=foreach::foreach(tFroms=tFroms,tTos=tTos, .combine='c') %dopar% {
                abatchModel(tSet,bnd,fS,tFroms,tTos,tidF,tids,mPath,idF="siteid",dateF="date",obsF="pm25",nM)}
  parallel::stopCluster(cl) # .packages=slibs
  proc.time()-ptm
  rm(list=c("res"))
  gc()
}

