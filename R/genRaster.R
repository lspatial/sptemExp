
genRaster=function(sideSdf,dx=2000,dy=2000,idStr="gid"){
  #  prj_=tarprj; sideSdf_=prnside;dx_=2000; dy_=2000 ;ext_=ext

  ext=raster::extent(sideSdf)
  prj=sp::proj4string(sideSdf)
  newRst=raster::raster(crs=prj, ext=ext, resolution=c(dx,dy), vals=-9999999)
  newRst1=raster::mask(newRst, sideSdf)
  pnt=raster::rasterToPoints(newRst1, spatial=TRUE)
  pnt@data[,idStr]=c(1:nrow(pnt@data))
  ret=list(PntObj=pnt,Rst=newRst1)
  return(ret)
}
