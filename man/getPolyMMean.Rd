\name{getPolyMMean}
\alias{getPolyMMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generation of Regional Monthly Mean Based on the Input Polygons }
\description{
  Generate the regional monthly mean of air pollutant concentrations based on the input polygons
}
\usage{
getPolyMMean(polys,samp,tse,idF="siteid",ridF="rid",obsF="obs",dateF="date")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polys}{The input region polygon map object (SpatialPolygonsDataFrame) to be used the regions for generation of the regiona monthly means  }
  \item{samp}{The sample spatial location map. Data format: \code{SpatialPointDataFrame}}
  \item{tse}{Time series for the siteid and date used for generation of monthly mean. }
  \item{idF}{location id name}
  \item{ridF}{region id name }
  \item{obsF}{observed value field name. }
  \item{dateF}{Date name}
}

\value{A data frame of data format: rid, year, month, mean}
\author{
Lianfa Li \email{lspatial@gmail.com}
}

\examples{

#Use the PM2.5 concentration as an example.

data("samplepnt","prnside","shdSeries2014")
tpolys=tpolygonsByBorder(samplepnt,prnside)$tpolys
regionmmean=getPolyMMean(tpolys, samplepnt, shdSeries2014,"siteid", "rid", "obs","date")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Regional mean}% use one of  RShowDoc("KEYWORDS")
\keyword{ air pollution }% __ONLY ONE__ keyword per line
