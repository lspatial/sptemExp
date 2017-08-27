\name{rmse}
\alias{rmse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RMSE function }
\description{A function to calculate rmse.}
\usage{
rmse(obs, pre)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{Observed values}
  \item{pre}{Predicted values}
}
\value{A  scalar value, RMSE}
\examples{

  obs=runif(400,1,100)
  pre=obs+rnorm(400,5,10)
  rmse(obs,pre)

}

\keyword{ ~rmse }% __ONLY ONE__ keyword per line
