
#' @title fillNASVD
#'
#' @description This function is to fill the missed values using SVD.
#'
#' @param dset: The target data frame with many missing values
#'
#' @param cols: A vector of column names to be filled
#'
#' @param idF: id field name
#'
#' @param dateD: date field name
#'
#' @return data frame with the missing values filled
#'
#' @export fillNASVD
#'
fillNASVD=function(dset,cols,idF="siteid",dateF="date"){
 # dset=all_train_dset;cols =c("pm25", "ndvi_chk","aod","wnd_avg","monthAv" )
  retDset=dset
  sites=unique(retDset[,"siteid"])
  for(acol in cols){ # acol="ndvi"
    index=which(is.na(retDset[,acol]))
    acol_vals=retDset[,acol]
    dates=unique(retDset$date)
    dates=dates[order(dates)]
    all_col_v=data.frame(date=dates,stringsAsFactors = F)
    for(k in c(1:length(sites))){ # k=1
      asite_v=retDset[which(retDset$siteid==sites[k]),c("date",acol)]
      asite_v=asite_v[order(asite_v$date),]
      cindex=match(all_col_v$date,asite_v$date)
      all_col_v$tmp=asite_v[cindex,acol]
      colnames(all_col_v)[ncol(all_col_v)]=sites[k]
    }
    tmp=bcv::impute.svd(all_col_v[,c(2:ncol(all_col_v))], k = 1,maxiter=1000)
    tmp=as.data.frame(tmp[["x"]])
    for(k in c(1:length(sites))){ # k=1
      retDset_tmp=dset[which(retDset$siteid==sites[k]),]
      index=match(retDset_tmp$date,all_col_v$date)
      retDset[which(retDset$siteid==sites[k]),acol]=tmp[index,k]
    }
  }
  return(retDset)
}
