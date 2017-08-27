\name{weiA2Ens}
\alias{weiA2Ens}
\title{Ensemble Weighted Prediction of Mixed Models}
\usage{
weiA2Ens(pPath,mFile,metrF="rmse",preF="pre",idF="gid",dateF=NA)
}
\description{
  Weighted average and standard deviation of multiple models.  The weights can be the model's performance metrics such as R2 or RMSE.
}
\arguments{
  \item{pPath}{Path for the prediction files from multiple models with the unified format and field names. File format: CSV with head.}
  \item{mFile}{File path for the corresponding multiple models's performance. CSV format:mid, r2, rmse. }
  \item{metrF}{target metric such as rmse or r2 to weigh the model's output}
  \item{preF}{prediction field name}
  \item{idF}{unique identifier string}
  \item{dateF}{date string if any}
}
\value{
   id and corresponding mean and standard deviation. Format: \code{dataframe}
}
\author{
  Lianfa Li: \email{lspatial@gmail.com}
}
\examples{
#First generate the prediction dataset and metrics.
# In application, you can use parSpModel to train models and
# get the models's performance metrics, and use the parATimePredict function to make the prediction

# Simulared data

dPath=tempdir()
pPath=paste(dPath,"/preds",sep="")
unlink(pPath, recursive=TRUE, force=TRUE)
dir.create(pPath)

nr=2000;nmod=80
for(i in c(1:nmod)){ # i =1
  dset=data.frame(gid=c(1:nr),rid=sample(c(1:30),size=nr,replace=TRUE),stringsAsFactors = FALSE)
  dset$pre=dset$gid\%\%nmod+rnorm(nr,mean=5,sd=9)+runif(nr,0,1)
  dset$gid=paste("c",dset$gid,sep="")
  afile=paste(pPath,"/m",i,".csv",sep="")
  write.csv(dset,file=afile,row.names = FALSE)
}

modelsMetrics=data.frame(mid=c(1:nmod),r2=runif(nmod,0.6,0.9),rmse=runif(nmod,20,60))
mfile=paste(dPath,"/model_metrics.csv",sep="")
write.csv(modelsMetrics,file=mfile,row.names = FALSE)
result=weiA2Ens(pPath,mfile,metrF="rmse","pre","gid","gid")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ensemble learning }% use one of  RShowDoc("KEYWORDS")
\keyword{ machine learning }% __ONLY ONE__ keyword per line


