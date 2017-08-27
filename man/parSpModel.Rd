\name{parSpModel}
\alias{parSpModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generation of Spatiotemporal Models by Bootstrap Aggregating}
\description{
  Generation of multiple models using bootstrap aggregating, supporting multi-cores based parallel computing.
}
\usage{
parSpModel(tSet,bnd,fS,tidF="tid",tids, c=1,
     nM=30,mPath,idF="siteid",dateF="date",obsF="pm25")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tSet}{Dataframe of the training dataset, including the measurements of the target variable and covariates.}
  \item{bnd}{BND object used in saptial effect modeling (BayesX)}
  \item{fS}{Formula string, like that used in BayesX}
  \item{tidF}{time id (ensemble models for each time point)}
  \item{tids}{all the time ids for which multiple models will be trained. }
  \item{c}{CPU core number}
  \item{nM}{Number of ensemble models for each time point.}
  \item{mPath}{Path where the models will be saved. }
  \item{idF}{Unique location name}
  \item{dateF}{Time id}
  \item{obsF}{Target variable name}
}
\details{Batch training of the models using the multi-cores based parallel computing }
\value{The model will be saved into the assigned path. }
\references{
Breiman, L., 1996. Bagging Predictors. Machine Learning 24, 123-140.
Lianfa Li et al, 2017, Constrained Mixed-Effect Models with Ensemble Learning for Prediction of Nitrogen Oxides Concentrations at High Spatiotemporal Resolution, ES & T, DOI: 10.1021/acs.est.7b01864

}
\author{Lianfa Li \email{lspatial@gmail.com} }

\examples{
# Example the PM2.5 data for Shandong

dPath=tempdir()
mPath=paste(dPath,"/models",sep="")
unlink(mPath,recursive = TRUE)
dir.create(mPath)

data("trainsample","bnd")

aform=paste0('logpm25 ~sx(rid,bs ="mrf",map =bnd)+sx(monthAv,bs="rw2")')
aform=paste0(aform,'+sx(ndvi,bs="rw2")+sx(aod,bs="rw2")+sx(wnd_avg,bs="rw2")')

formulaStrs=c(aform)

trainsample$tid=as.numeric(strftime(trainsample$date, format = "\%j"))
trainsample$logpm25=log(trainsample$pm25)
tids=c(91)

parSpModel(trainsample,bnd,formulaStrs,tidF="tid",
   tids,c=2,nM=3,mPath,idF="siteid",dateF="date",obsF="pm25")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bootstrap aggregate }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ensemble learning }% __ONLY ONE__ keyword per line
