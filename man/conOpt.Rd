\name{conOpt}
\alias{conOpt}
\title{Function of Constrained Optimization}
\description{
  Constrained optimization to construct the long-term series of air pollutants .
}
\usage{
   conOpt(ptrends,tSet,preF="con",paras=c(2.5,-5.5,-0.6,-0.1,-0.25,0.25),maxC)
}

\arguments{
  \item{ptrends}{seasonal trends such as temporal basis functions. }
  \item{tSet}{Train dataset (observed or estimated values) to get the solution.}
  \item{preF}{Predicted field name. }
  \item{paras}{A vector, constraints for the coefficients of temporal basis functions, respectively correponding to b0, b1 and b2. Different pollutants have different constraint parameters. }
  \item{maxC}{Maximum values for conentration of air pollutants. }
}
\value{
  a vector of the coefficients for temporal basis functions.
}
\references{
Lianfa Li et al, 2017, Constrained Mixed-Effect Models with Ensemble Learning for Prediction of Nitrogen Oxides Concentrations at High Spatiotemporal Resolution, ES & T, DOI: 10.1021/acs.est.7b01864
}
\author{
   Lianfa Li  \email{lspatial@gmail.com}
}
\examples{
#PM2.5 exmaple:

data("allPre500","shdSeries2014")
#Get the temporal basis functions
season_trends=getTBasisFun(shdSeries2014,idStr="siteid",dateStr="date",
                           valStr="obs",df=10,n.basis=2,tbPath=NA)

asiteMe=allPre500[1,]
ndays=ncol(allPre500)
trainSet=NA
days=as.integer(gsub("d","",colnames(allPre500)))
for(k in c(1:ndays)){
  aday=paste("d",days[k],sep="")
  if(!is.na(asiteMe[,aday])){
    atrainPnt=data.frame(b0=1,b1=season_trends$pv1[days[k]],
          b2=season_trends$pv2[days[k]],con=log(asiteMe[,aday]))
    if(inherits(trainSet,"logical")){
      trainSet=atrainPnt
    }else{
      trainSet=rbind(trainSet,atrainPnt)
    }
  }
}
#Set the PM2.5 constriants:
paras=c(2.5,-5.5,-0.6,-0.1,-0.25,0.25)
maxCon=750
res=conOpt(season_trends,trainSet,preF="con",paras,maxCon)


}

\keyword{models} \keyword{air pollution} \keyword{regression}%-- one or more ..
\concept{Exposure models}
