\name{genRaster}
\alias{genRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generation of Raster Covering the Side Map}
\description{Generaye the raster to cover the study region with the preset resolution. }
\usage{
genRaster(sideSdf, dx, dy, idStr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sideSdf}{The SpatialPolygonDataFrame obejct used to constrain the grid border.}
  \item{dx}{x resolution }
  \item{dy}{y resolution }
  \item{idStr}{id name}
}

\value{
 \item{PntObj}{The SpatialPointDataFrame extracted from the generated raster. }
 \item{Rst}{The raster object covering the study region. }
}

\examples{

## Use the Shandong province as an example:
data("prnside")
ret=genRaster(prnside,dx=2000,dy=2000,idStr="gid")
raster::plot(ret$Rst)
raster::plot(ret$PntObj)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
