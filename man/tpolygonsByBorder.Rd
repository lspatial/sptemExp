\name{tpolygonsByBorder}
\alias{tpolygonsByBorder}
\title{tpolygonsByBorder for Generation of Thiessen polygons}
\description{
Generate Thiessen polygons according to the point spatialframes and border.
}
\usage{
tpolygonsByBorder(x,sidepoly)
}
\arguments{
  \item{x}{SpatialPointsDataFrame \code{"SpatialPointsDataFrame"}.}
  \item{sidepoly}{SpatialPolygonsDataFrame, e.g. \code{SpatialPolygonsDataFrame}.}
}
\value{
A list object:
  \item{tpolys}{Thiessen polygons, data format: SpatialPolygonsDataFrame }
  \item{bnd}{BND object used in the model in the BayesX.}
}
\author{ Lianfa Li \email{lspatial@gmail.com}}
\examples{

data("samplepnt","prnside")
x=samplepnt
sidepoly=prnside
tpoly=tpolygonsByBorder(x,sidepoly)$tpolys
raster::plot(tpoly)

}
