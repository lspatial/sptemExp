\name{inter2conOpt}
\alias{inter2conOpt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Batch Interpolation of the Missing Values for Time Series Using Constrained Optimization. }
\description{This function provides batch implementation for interpolation of the missing values for multiple locations for a raster, supporting multi-core parallel computing. }
\usage{
inter2conOpt(tarPDf, pol_season_trends, ncore)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tarPDf}{The target data frame with missing values. Each row corresponds to a location (rowname as location id) and each column corresonds to a time point. The sequence of the location and time should be in sequence in spatial and temporal dimension. This dataset comes from the raster dataset and the sequence is kept for convenience of making raster with the interpolated value.}
  \item{pol_season_trends}{ The temporal basis function using \code{getTBasisFun}}
  \item{ncore}{number of cores for parallel computing. }
}
\details{This function aims to implement the batch computing to use constrained optimization to get the concentrations for the missing values of a time series, such as PM2.5 concentration. }
\value{A data frame similar to the input data frame, tarPDf but with the missing values interpolated by constrained optimizaiotn. }

\author{Lianfa Li \email{lspatial@gmail.com}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

# Here is the sample for the first 500 locations.
# In practice, you may need more point locations and more cores.
data("allPre500","shdSeries2014")
# Get the temporal basis functions to be used in constrained optimization
season_trends=getTBasisFun(shdSeries2014,idStr="siteid",dateStr="date",
                           valStr="obs",df=10,n.basis=2,tbPath=NA)

#Constrained optimization
season_trends$tid=as.numeric(strftime(season_trends$date, format = "\%j"))
allPre_part_filled=inter2conOpt(tarPDf=allPre500[c(1:6),],pol_season_trends=season_trends,ncore=2)

}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{parallel}% __ONLY ONE__ keyword per line
