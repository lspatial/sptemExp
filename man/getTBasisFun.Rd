\name{getTBasisFun}
\alias{getTBasisFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generation of Temporal Basis Function}
\description{Generation of temporal basis function }
\usage{
getTBasisFun(serDf, idStr, dateStr, valStr, df = 25, n.basis = 2, tbPath = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{serDf}{Time series dataframe, format: (siteid,date,observed value)}
  \item{idStr}{Location id name}
  \item{dateStr}{Date id name}
  \item{valStr}{The target variable's name}
  \item{df}{Degree of freedom}
  \item{n.basis}{Number of temporal basis function }
  \item{tbPath}{The path to save the plots of each temporal basis component. Default: NA, no plots generated}
}

\value{A dataframe of temporal basis function: (date, pvi (the ith temporal basis function output for a date ))}
\references{
Finkenstadt, B., Held, L., Isham, V., 2007. Statistical Methods for Spatio-Temporal Systems. Chapman & Hall/CRC, New York.
}
\examples{
#Use PM2.5 as example:

data("shdSeries2014")
result=getTBasisFun(shdSeries2014,"siteid","date","obs",df=10,n.basis=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatiotemporal modeling }% use one of  RShowDoc("KEYWORDS")
\keyword{ temporal basis function }% __ONLY ONE__ keyword per line
