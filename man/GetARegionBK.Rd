\name{GetARegionBK}
\alias{GetARegionBK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a regional Kriging}
\description{
   Estimate a regional mean for the regions.
}
\usage{
GetARegionBK(rNames,rF,rT,rlayer,paras,spnts,regF,obsF="pre_mf_log")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rNames}{region data}
  \item{rF}{id of the start region}
  \item{rT}{id of the end}
  \item{rlayer}{regional layer }
  \item{paras}{ parameters of variogram}
  \item{spnts}{spatial points for preiction}
  \item{regF}{region field name}
  \item{obsF}{observed field name }
}

\value{Regiona means by block kriging. }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
