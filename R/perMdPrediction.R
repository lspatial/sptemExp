
#' @title perMdPrediction
#'
#' @description This function is to caltulate rmse.
#'
#' @param mPath : Basic path of the models
#'
#' @param  mFiles: Files names of the models
#'
#' @param  mids:A vector of the models index
#'
#' @param  mF: Starting index of the models to be used to make the prediction
#'
#' @param  mT: Ending index of the models to be used to make the prediction
#'
#' @param  bnd: BND format of polygons for spatial effect modeling
#'
#' @param  dset: New data frame to  be predicted
#'
#' @param  outPath: The path to save the files of the predicted results.
#'
#' @param  idF: Field name of id
#'
#' @param  ridF: Field name of the regional id
#'
#' @return NULL.  All the predicted values saved int the appointed path.
#'
#' @export perMdPrediction
#'

perMdPrediction=function(mPath,mFiles,mids,mF,mT,bnd,dset,outPath,idF,ridF){
  assign("bnd",bnd)
  for(i in c(mF:mT)){
    model=readRDS(paste(mPath,"/",mFiles[i],sep=""))
    dset$pre=exp(predict(model,dset))
    cols=c(idF,ridF,"pre")
    outFl=paste(outPath,"/m_",mids[i],".csv",sep="")
    write.csv(dset[,cols],file=outFl,row.names=FALSE)
    rm(list=c("index","outFl","model"))
    gc()
  }
  return(0)
}
