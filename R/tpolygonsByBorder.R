
voronoipolygons2=function(x,poly) {
  if (.hasSlot(x, 'coords')) {
    crds =unique(x@coords)
  } else{
    crds = unique(x)
  }
  bb = sp::bbox(poly)
  rw = as.numeric(t(bb))
  z = deldir::deldir(crds[,1], crds[,2],rw=rw)
  w = deldir::tile.list(z)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {# i=1
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = sp::Polygons(list(sp::Polygon(pcrds)), ID=as.character(i))
  }
  SP = sp::SpatialPolygons(polys)

  voronoi = sp::SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                         y=crds[,2], row.names=sapply(slot(SP, 'polygons'),
                                                                                      function(x) slot(x, 'ID'))))
  return(voronoi)
}


tpolygonsByBorder=function(x,sidepoly){
  #require(sp)

  # x=mypointlayer2; border= extBnd
  prjinf=sp::proj4string(x)
  sidepolyp=sp::spTransform(sidepoly,prjinf)
  extBnd=as(raster::extent(sidepolyp), 'SpatialPolygons')
  sp::proj4string(extBnd)=prjinf
  pzn.coords=voronoipolygons2(x,extBnd)
  sp::proj4string(pzn.coords)=prjinf
  gg = rgeos::gIntersection(extBnd,pzn.coords,byid=TRUE)
  gg=gg-(gg-sidepoly)
  crds=sp::coordinates(gg)
  thiessenpolysclip=sp::SpatialPolygonsDataFrame(gg, data=data.frame(x=crds[,1],
        y=crds[,2], row.names=sapply(slot(gg, 'polygons'),function(x) slot(x, 'ID'))))
  thiessenpolysclip@data$src="points"
  thiessenpolysclip@data$ID=c(1:nrow(thiessenpolysclip@data))
  dataSet=attr(thiessenpolysclip, "data")

  bndDt=R2BayesX::sp2bnd(thiessenpolysclip,regionNames=as.character(dataSet$ID))
  raster::plot(thiessenpolysclip)
  reseult=list(bnd=bndDt,tpolys=thiessenpolysclip)
  return(reseult)
}
