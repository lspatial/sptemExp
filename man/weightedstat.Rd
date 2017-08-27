\name{weightedstat}
\alias{weightedstat}
\title{Weighted Average for Multiple Models}
\usage{
  weightedstat(path,modelpath,metric,preStr, idStr,dateStr)
}
\arguments{
  \item{path}{path of the prediction dataset files.}
  \item{modelpath}{model metric file }
  \item{metric}{performance weight }
  \item{preStr}{prediction value's name}
  \item{idStr}{id name}
  \item{dateStr}{date name}
}
\description{
  Returns an \R \code{dataframe} containing the character vector
  \code{c("foo", "bar")}  \code{c(0, 1)}.
}

\author{ Lianfa Li \email{lspatial@gmail.com}}

\examples{

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
modelsMetrics$rmse2=1/modelsMetrics$rmse
mfile=paste(dPath,"/model_metrics.csv",sep="")
write.csv(modelsMetrics,file=mfile,row.names = FALSE)
res=weightedstat(pPath,modelpath=mfile,metric="rmse2",preStr="pre",idStr="gid",dateStr="gid")



}
