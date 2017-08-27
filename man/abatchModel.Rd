\name{abatchModel}
\alias{abatchModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Batch Modeing Training Inner Functions}
\description{
This function is for a batch training models. The users can call parSpModel rather than this for training of multiple models.
}
\usage{
abatchModel(td,bnd,fS,iF,iT,tidF,tids,mPath,idF="siteid",dateF="date",obsF="obs",nM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{td}{Training dataset}
  \item{bnd}{Map object used in spatial effect model. For specific format, refer to BayesX}
  \item{fS}{Formular string}
  \item{iF}{Staring time id}
  \item{iT}{Ending time id}
  \item{tidF}{Time field name}
  \item{tids}{Time vector}
  \item{mPath}{The path for the models trained to be saved}
  \item{idF}{location id name}
  \item{dateF}{Date or time field name}
  \item{obsF}{observed value field name}
  \item{nM}{number of models to be trained}
}
\details{
This is an inner function to be called by parSpModel.
}
\value{The trained models will be saved on the appointed path. No direct output for this function.}

\examples{

#An example of PM2.5 data from Shandong


dPath=tempdir()
modelPath=paste(dPath,"/models",sep="")
unlink(modelPath,recursive = TRUE)
dir.create(modelPath)

data("trainsample","bnd")
aform=paste0('logpm25 ~sx(rid,bs ="mrf",map =bnd)+sx(monthAv,bs="rw2")')
aform=paste0(aform,'+sx(ndvi,bs="rw2")+sx(aod,bs="rw2")+sx(wnd_avg,bs="rw2")')

formulaStrs=c(aform)

trainsample$tid=as.numeric(strftime(trainsample$date,format= "\%j"))
trainsample$logpm25=log(trainsample$pm25)
tids=c(91)
abatchModel(trainsample,bnd,formulaStrs,1,1,"tid",tids,modelPath,"siteid","date","pm25",3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
