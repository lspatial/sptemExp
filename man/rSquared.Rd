\name{rSquared}
\alias{rSquared}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coefficient of Determination }
\description{A function to calculate the rSquared.}
\usage{
rSquared(obs, res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A vector of the observed values.}
  \item{res}{A vector of residuals}
}
\value{rsquared value}
\examples{

  obs=runif(400,1,100)
  pre=obs+rnorm(400,5,10)
  res=obs-pre
  rSquared(obs,res)
}
\keyword{ ~r2 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~rquared }% __ONLY ONE__ keyword per line
