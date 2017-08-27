#' @title conOpt
#'
#' @description This function is to extract temporal basis functions.
#'
#' @param ptrends: Temporal basis functions to capture the seasonal trends
#'
#' @param tSet: Train dataset of point estimates used to estimate the coefficients for ptrends
#'
#' @param preF: Field of predicted value
#'
#' @param paras: Parameters of variogram
#'
#' @param maxC: Maximum concentration
#'
#' @return A vector of beta parameters
#'
#' @export conOpt
#'
conOpt=function(ptrends,tSet,preF="con",paras=c(2.5,-5.5,-0.6,-0.1,-0.25,0.25),maxC=750){
  ## ptrends=pm25_season_trends;tSet_=tSet; preF="con"
  lc_A=as.matrix(tSet[,c("b0","b1","b2")])
  lc_B=as.vector(tSet[,preF])
  lc_G=matrix(nrow=6,ncol=3,byrow=TRUE,data=c(1,0,0,  -1,0,0,   0,1,0,    0,-1,0,  0,0,1,  0,0,-1))
  lc_H=paras
  # lc_G=NA;lc_H=NA
  for(j in c(1:nrow(ptrends))){
    arow=c(-1,-ptrends[j,"pv1"],-ptrends[j,"pv2"])
    if(inherits(lc_H,"logical")){
      lc_G=arow
      lc_H=-log(maxC)
    }else{
      lc_G=rbind(lc_G,arow)
      lc_H=c(lc_H,-log(maxC))
    }
  }
  tid1=1
  tid2=nrow(ptrends)
  arow=c(0,ptrends[tid1,"pv1"]-ptrends[tid1,"pv2"],
         ptrends[tid2,"pv1"]-ptrends[tid2,"pv2"])
  if(!is.na(arow[2]) && !is.na(arow[3])){
    lc_G=rbind(lc_G,arow)
    lc_H=c(lc_H,0)
  }
  try_res=try((lm_cons=limSolve::lsei(A =lc_A, B =lc_B, G =lc_G, H=lc_H,type=2)),silent=TRUE)
  if(class(try_res)=="try-error"){
    return(NA)
  }
  beta_para=data.frame(b0=lm_cons$X["b0"],b1=lm_cons$X["b1"],  b2=lm_cons$X["b2"])
  return(beta_para)
}



