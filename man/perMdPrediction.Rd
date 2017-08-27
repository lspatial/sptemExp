\name{perMdPrediction}
\alias{perMdPrediction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Batch Prediction Using the Trained Models }
\description{Batch Prediction Using the Trained Models }
\usage{
    perMdPrediction(mPath,mFiles,mids,mF,mT,bnd,dset,outPath,idF,ridF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mPath}{Model path}
  \item{mFiles}{Model file path}
  \item{mids}{Set of mids}
  \item{mF}{From field name}
  \item{mT}{To field name}
  \item{bnd}{BND object used in spatial effect modeling}
  \item{dset}{newDataset to be predicted}
  \item{outPath}{Ourput path to save the predictions. }
  \item{idF}{id field name}
  \item{ridF}{regional id field name}
}
\value{No straightforward output. All output saved in the appointed path. }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
