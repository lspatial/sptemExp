
points2Raster=function(spoints,tarVar,dx=2000,dy=2000){
  ret=raster::raster(spoints,resolution=c(dx,dy))
  cells=raster::cellFromXY(ret,sp::coordinates(spoints))
  ret[cells]=spoints@data[,tarVar]
  return(ret)
}
