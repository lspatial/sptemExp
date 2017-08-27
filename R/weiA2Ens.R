#' @title weiA2Ens
#'
#' @description This function is to caltulate rmse.
#'
#' @param pPath : Path for the predictions files
#'
#' @param  mFile: File path of the models performance metrics
#'
#' @param  metrF: Performance metric field name
#'
#' @param  preF: Field name of predictions
#'
#' @param  idF: Field name of id
#'
#' @param  dateF: Field name of date
#'
#' @return Data frame of weightted results
#'
#' @export weiA2Ens

weiA2Ens=function(pPath,mFile,metrF="rmse",preF="pre",idF="gid",dateF=NA){
  mmetrics=read.csv(mFile,row.names = NULL, stringsAsFactors = FALSE)
  if(!("mid" %in% colnames(mmetrics))){
    mmetrics$mid=mmetrics$imodel
  }
  mmetrics$rmse=1/mmetrics$rmse
  tmpPath=paste("/tmp/t_",as.numeric(Sys.time()),"_models_re.csv",sep="")
  write.csv(mmetrics[,c("mid","r2","rmse")],file=tmpPath,row.names=FALSE)
  if(inherits(dateF,"logical"))
     dateF=idF
  res=weightedstat(pPath,modelpath=tmpPath,metric=metrF,preF,idF,dateF)
  return(res)
}

