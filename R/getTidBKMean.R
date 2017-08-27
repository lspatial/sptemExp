
#' @title getTidBKMean
#'
#' @description This function is to caltulate rmse.
#'
#' @param tvals: Data frame with the format: row-a location; column;time point ("d1" indicates day 1)
#'
#' @param spt: SpatialPointDataFrame, used to receive the val from tvals to make block kriging means
#'
#' @param rlayer: Regional layer of SpatialPolygonsDataFrame
#'
#' @param atid: a scalar value for time id
#'
#' @param regF: Region field name
#'
#' @param tarF: Target variable field name
#'
#' @param n: number of CPUs.
#'
#' @return Data frame of the kriged means, data format simialr to reionalayer with added kriged values
#'
#' @export getTidBKMean
#'
#'
getTidBKMean=function(spt,rlayer,regF="NAME_3",tarF="pre_mf",n=1){
  #  spt=spointspre;rlayer=countylayer;atid=NULL;regF="NAME_3";tarF="d91";n=1
  logTar=paste(tarF,"_log",sep="")
  spt@data[,logTar]=log(spt@data[,tarF])
  spt_sam=spt[!is.na(spt@data[,tarF]),]
  cmdStr=paste("automap::autofitVariogram(",logTar,"~1,input_data=spt_sam,model = c('Exp'),",sep="")
  cmdStr=paste(cmdStr,"miscFitOptions = list(min.np.bin = 1),fix.values=c(NA,50000,NA))",sep="")
  variogram =eval(parse(text=cmdStr))
  raster::plot(variogram)
  vars=variogram$var_model
  sill=vars[vars$model=="Exp","psill"]
  range=vars[vars$model=="Exp","range"]
  nugget=vars[vars$model=="Nug","psill"]
  paras=c(range,sill,nugget)
  regions=rlayer@data[,regF]
  rlen=length(regions)
  timesEach=floor(rlen/n)
  rFroms=c()
  rTos=c()
  pcores=n
  if(timesEach==0){
    rFroms=c(1:rlen)
    rTos=c(1:rlen)
    pcores=rlen
  }else{
    for(i in c(1:n)){
      rFrom=(i-1)*timesEach+1
      rTo=i*timesEach
      if(i==n){
        rTo=rlen
      }
      rFroms=c(rFroms,rFrom)
      rTos=c(rTos,rTo)
    }
  }
  print(paste("pcores:",pcores,sep=""))
  cl=parallel::makeCluster(pcores)
  doParallel::registerDoParallel(cl)
  ptm=proc.time()
  all_bkr_est=foreach::foreach(rFroms=rFroms,rTos=rTos, .combine='rbind', .errorhandling="pass",
                    .packages=c('rgdal','plyr')) %dopar% {
                      GetARegionBK(regions,rFroms,rTos,rlayer,paras,spt,regF,obsF=logTar)}
  parallel::stopCluster(cl)
  proc.time()-ptm

  all_bkr_est$bkmean_filled=all_bkr_est$bkmean
  rlayer_out=rlayer
  index=match(rlayer_out@data[,regF],all_bkr_est$cnname)
  rlayer_out$bkmean=all_bkr_est[index,"bkmean"]
  rlayer_out$bkstd=all_bkr_est[index,"bkstd"]
  rlayer_out$bkm_fill=all_bkr_est[index,"bkmean_filled"]
  rm(list=c("all_bkr_est"))
  gc()
  return(rlayer_out)
}


