
#' @title getPolyMMean
#'
#' @description This function is to extract the regional monthly means
#'
#' @param polys: SpatialPolygonDataFrame whose means to be calculated
#'
#' @param samp: Sample points distributed in the polygons
#'
#' @param tse: Data frame of time series
#'
#' @param idF: Field name of id
#'
#' @param ridF: Field name of region(polygons) id
#'
#' @param obsF: Field name of observed values
#'
#' @param dateF: Field name of date
#'
#' @return data frame with the format (rid, year, month, mean)
#'
#' @examples  rmse(obs=c(1,2,3,4),pre=c(1,2,3,4)+rnorm(4))
#'
#' @export getPolyMMean
#'
getPolyMMean=function(polys,samp,tse,idF="siteid",ridF="rid",obsF="obs",dateF="date"){
   # polys=res$polys;samp=samp; idF="siteid"; tse=filled_series;ridF="rid";obsF="obs";dateF="date"
  samp@data[,ridF]=getRidbytpoly(polys,samp)
  index=match(tse[,idF],samp@data[,idF])
  tse[,ridF]=samp@data[index,ridF]
  tse$month=as.integer(format(tse$date,"%m"))
  tse$year=as.integer(format(tse$date,"%Y"))
  strCmd=paste('plyr::ddply(tse,c("',ridF,'","year","month"),plyr::summarise,avg=mean(obs,na.rm=TRUE))',sep='')
  result=eval(parse(text=strCmd))
  return(result)
}
