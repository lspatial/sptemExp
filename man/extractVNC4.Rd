\name{extractVNC4}
\alias{extractVNC4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Values for Point from NC4 Image}
\description{A program to extarct the values from the NC4 image by overlaying the subject locations. }
\usage{
extractVNC4(tarshp, ncin, bandVar, prj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tarshp}{The point objects, format: \code{SpatialPointDataFrame}}
  \item{ncin}{ the nc4 object by nc_open. }
  \item{bandVar}{The band name to be used for extration from the NC4 file}
  \item{prj}{The project information, default: NA}
}
\details{This function can be used to extract values from the NC4 images (such as satellite images)}
\value{The values extracted in the same sequence with the point object. Format: \code{vector} }
\references{
http://disc.sci.gsfc.nasa.gov/daac-bin/FTPSubset2.pl)
}
\author{
Lianfa Li, \email{lspatial@gmail.com}
}

\examples{

data("samplepnt")
nc4File=file.path(system.file(package = "sptemExp"), "extdata", "ancdata.nc4")
ncin0=ncdf4::nc_open(nc4File)
extRes=extractVNC4(samplepnt,ncin0,"TLML")
extRes


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
