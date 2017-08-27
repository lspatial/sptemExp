
extractVNC4=function(tarshp,ncin,bandVar,prj=NA){
  lat  =ncdf4::ncvar_get(ncin, "lat", verbose = FALSE)
  lon  =ncdf4::ncvar_get(ncin, "lon", verbose = FALSE)
  ExtentLat = range(lat)
  ExtentLon = range(lon)
  tmp.array= ncdf4::ncvar_get(ncin,bandVar)
  fillvalue= ncdf4::ncatt_get(ncin, bandVar, "_FillValue")
  tmp.array[tmp.array == fillvalue$value]= NA
  ncdf4::nc_close(ncin)
  trast = raster::raster(tmp.array)
  raster::extent(trast) = c(ExtentLon,ExtentLat)
  tarprj='+proj=longlat +datum=WGS84'
  raster::projection(trast)=tarprj
  tarshpprj=sp::spTransform(tarshp,tarprj)
  res=raster::extract(trast,tarshpprj)
  ret=list(img=trast,val=res)
  return(ret)
}
