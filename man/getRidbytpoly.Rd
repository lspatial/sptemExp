\name{getRidbytpoly}
\alias{getRidbytpoly}
\title{getRidbytpoly for Assignment of  Thiessen polygon id to point object}
\description{
Assign the polygon id to the data points.
}
\usage{
getRidbytpoly(tpolys,pntlayer,isnearest)
}
\arguments{
  \item{tpolys}{Thiessen polygons, data format: SpatialPolygonsDataFrame.}
  \item{pntlayer}{Points for assignment for polygons. SpatialPointsDataFrame.}
  \item{isnearest}{whether to use nearest method to assign polygon id for no overlay with polygons, default: TRUE}
}
\value{
   polygon id
}
\author{
   Lianfa Li  \email{lspatial@gmail.com}
}
\examples{

  data("samplepnt","prnside")
  # Point
  x=samplepnt
  # Border
  sidepoly=prnside
  # Get the Thiessen polygons
  res=tpolygonsByBorder(x,sidepoly)
  # Assign the regional id
  rids=getRidbytpoly(res$tpolys,x)

}
