\name{noweiAvg}
\alias{noweiAvg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Averages over the Ensemble Predictions of Mixed Models (No weighted)
}
\description{
 Average and standard deviation of multiple models by ensemble learning.
}
\usage{
noweiAvg(path, preStr = "preno2", idStr = "id", dateStr = "s_date")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{Path for the prediction files from multiple models with the unified format and field names. File format: CSV with head:(gid,rid,pre)}
  \item{preStr}{prediction field names}
  \item{idStr}{unique identifier string}
  \item{dateStr}{date string. You can set it as the same as idStr}
}
\value{ id and corresponding mean and standard deviation. Format: \code{dataframe} }
\author{
Lianfa Li: \email{lspatial@gmail.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

# Generate the prediction dataset, but you can use parATimePredict function
# to make the prediction in application

dPath=tempdir()
pPath=paste(dPath,"/preds",sep="")
unlink(pPath, recursive=TRUE, force=TRUE)
dir.create(pPath)


nr=2000
for(i in c(1:80)){ # i =1
  dset=data.frame(gid=c(1:nr),rid=sample(c(1:30),size=nr,replace=TRUE))
  dset$pre=dset$gid\%\%80+rnorm(nr,mean=5,sd=9)+runif(nr,0,1)
  afile=paste(pPath,"/m_",i,".csv",sep="")
  write.csv(dset,file=afile,row.names = FALSE)
}
result=noweiAvg(pPath, preStr="pre",idStr="gid",dateStr="gid")


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ensemble learning }% use one of  RShowDoc("KEYWORDS")
\keyword{ machine learning }% __ONLY ONE__ keyword per line
