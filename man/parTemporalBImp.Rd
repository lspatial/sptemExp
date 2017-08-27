\name{parTemporalBImp}
\alias{parTemporalBImp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Fill Missing Values by Constraint Optimization}
\description{A function to use constraint pptimization to predict the missing values. }
\usage{
parTemporalBImp(allPre_, siteids_, isite_, pol_season_trends_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{allPre_}{Prediction dataset including many mssing values}
  \item{siteids_}{Ponitoring station id}
  \item{isite_}{Target site field name}
  \item{pol_season_trends_}{Temporal basis function. }
}
\value{Parameters for temporal basis functions. }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
