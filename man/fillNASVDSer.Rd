\name{fillNASVDSer}
\alias{fillNASVDSer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SVD to Interpolate the Missing Values in the Time Series Data }
\description{
Function to Use SVD to Interpolate the Missing Values in the Time Series Data
}
\usage{
fillNASVDSer(dset, idF, dateF, valF, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{The data frame for time series. Data format: siteid, date, obs \code{dataframe}. }
  \item{idF}{The unique location id like siteid. }
  \item{dateF}{The time column name. }
  \item{valF}{The target variable column name. }
  \item{k}{ the priciple component, default 1}
}
\details{
This function can be used to fill the missing values in time series for many locations.
}
\value{The data frame similar to the input dset's structure but with filled values.}
\examples{

#Using the 2014 PM2.5 time series as an example
data("shdSeries2014")
n=nrow(shdSeries2014)
p=0.1 # Set the proportion of missing values
np=as.integer(n*p)
index=sample(n,np)
shdSeries2014missed=shdSeries2014
shdSeries2014missed[index,"obs"]=NA
shdSeries2014filled=fillNASVDSer(shdSeries2014missed,"siteid","date","obs",k=1)

#Exmine the accuracy:
cor(shdSeries2014filled[index,"obs"],shdSeries2014[index,"obs"])
rmse(shdSeries2014filled[index,"obs"],shdSeries2014[index,"obs"])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
