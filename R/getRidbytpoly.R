

getRidbytpoly=function(tpolys,pntlayer,isnearest=TRUE){
  # tpolys=res$tpolys ;pntlayer=samplepnt

  idinpoly=sp::over(pntlayer,as(tpolys,"SpatialPolygons"))
  rids=idinpoly
  index=which(is.na(rids))
  if(!isnearest || length(index)==0)
    return(rids)

  for(i in c(1:length(index))){ # i=1
    aindex=index[i]
    rids[aindex]=tpolys@data[which.min(rgeos::gDistance(pntlayer[aindex,], as(tpolys,"SpatialPolygons"), byid=TRUE)),"ID"]
  }
  return(rids)
}
