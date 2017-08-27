

extractVTIF=function(tarshp,tifRaster){
  rstprj=sp::proj4string(tifRaster)
  tarshpprj=sp::spTransform(tarshp,rstprj)
  res=raster::extract(tifRaster,tarshpprj)
  return(res)
}

