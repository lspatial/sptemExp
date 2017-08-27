\name{fillNASVD}
\alias{fillNASVD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Use SVD to Impute the Missing Values for Training Dataset}
\description{ Singular value decomposition (SVG) is used to impute the missing values for the training dataset. For each monitoring location, the time series of multivariate data is leveraged to impute the missing values using SVD.}
\usage{
fillNASVD(dset, cols, idF, dateF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{The dataframe having many missing values. Data format: \code{dataframe}}
  \item{cols}{A character vector to contain the column names (including the columns with missing values) used to impute the missing valeus}
  \item{idF}{Unique location identification}
  \item{dateF}{Date column name if any}
}

\value{A dataframe base on the input dset, but with filled values. }

\examples{

# Use the covariates for PM2.5 data as a example:

data("trainsample")
cols=c("ndvi","aod","wnd_avg","monthAv")
n=nrow(trainsample)
p=0.05
pn=as.integer(p*n)
trainsample2missed=trainsample
for(col in cols){
  index=sample(n,pn)
  trainsample2missed[index,col]=NA
}
trainsample2filled=fillNASVD(trainsample2missed,cols,"siteid","date")

#Examine the accuracy:
for(col in cols){
  index=which(is.na(trainsample2missed[,col]))
  obs=trainsample[index,col]
  missed=trainsample2missed[index,]
  sindex=match(interaction(missed$siteid,missed$date),
               interaction(trainsample2filled$siteid,trainsample2filled$date))
  pre=trainsample2filled[sindex,col]
  print(paste(col," missing value correlation: ",round(cor(obs,pre),2)))
  print(paste(col," missing value cv rmse: ",round(rmse(obs,pre),2)))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fillNASVD }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~missing }% __ONLY ONE__ keyword per line
