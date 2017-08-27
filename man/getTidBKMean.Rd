\name{getTidBKMean}
\alias{getTidBKMean}
\title{Batch Block Kriging for Estimate of Regional Means}
\description{ A batch program to implement block kriging for estimate of regional mean for air pollutant. Support the multi-core parallel computation. }
\usage{
getTidBKMean(spt,rlayer,regF="NAME_3",tarF="pre_mf",n=1)
}
\arguments{
  \item{spt}{Spatial point layer (shape file) corresponding to the grid \code{spointspre}}
  \item{rlayer}{ Regional layer to crop the points for estimate of regional means \code{regionlayer}}
  \item{regF}{ Regiona field name \code{regionName} }
  \item{tarF}{ the target variable to be estimated \code{tarVar}}
  \item{n}{Core number of CPU for parallel support \code{ncore}}
}
\value{The spatial polygon dataframe including the field of kriged means. }
\author{ Lianfa Li \email{lspatial@gmail.com}}

\examples{

# PM2.5 example

data("spointspre","countylayer")
regionName="NAME_3"
tarF="d91" # field target name to be estimated (2014-04-01 for 91 day of 2014)
bkRes=getTidBKMean(spointspre,countylayer,regionName,tarF="d91",n=2)

}
