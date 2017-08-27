\name{voronoipolygons2}
\alias{voronoipolygons2}
\title{Generation of Thiesseon Polygons By Points}
\description{Generation of Thiessen polygons by spatial points}
\usage{
voronoipolygons2(x, poly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Spatial point object, data format:\code{SpatialPointsDataFrame} }
  \item{poly}{The border polygons object to limit the Thiessen polygons. Data format:\code{SpatialPolygonsDataFrame}}
}
\value{The spatial polygongs objects. Data format: \code{SpatialPolygonsDataFrame}}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{

data("samplepnt","prnside")
x=samplepnt
sidepoly=prnside
prjinf=sp::proj4string(x)
sidepoly_p=sp::spTransform(sidepoly,prjinf)
extBnd=as(raster::extent(sidepoly_p), 'SpatialPolygons')
sp::proj4string(extBnd)=prjinf
pzn.coords=voronoipolygons2(x,extBnd)
sp::proj4string(pzn.coords)=prjinf


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
