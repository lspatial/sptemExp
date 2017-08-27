\name{colorGrinf}
\alias{colorGrinf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generation of Customed Gradient Colors}
\description{
  This function is to generate the color gradient with the customed levels
}
\usage{
colorGrinf(x, levels=NA, colors=c("green","yellow","red"), colsteps=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector value}
  \item{levels}{levels of gradient colors}
  \item{colors}{Color ranges}
  \item{colsteps}{Levels of color gradient. }
}
\value{
  \item{levels}{Level of values for legend use}
  \item{cols}{Color ranges for legeng use }
  \item{index}{Color values for map. }
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

#Example

x=sample(c(1:1000),size=100)
x=x[order(x)]
ret=colorGrinf(x)

# A block kriging example :

data("spointspre","countylayer")
tarF="d91" # target variable to be kriged
regionName="NAME_3"
bkRes=sptemExp::getTidBKMean(spointspre,countylayer,regionName,tarF,2)

bkRes=bkRes[!is.na(bkRes$bkm_fill),]
levels=c(30,60,100,150,250)
cr=sptemExp::colorGrinf(bkRes$bkm_fill,levels,colors=c("darkgreen","yellow","darkred"))
par(mar=c(1,1,1,1))
title=expression("Regional Block Kriged PM"[2.5]*" Concentration Estimated")
raster::plot(bkRes,col =cr$cols[cr$index],main=title)
legend("bottomright", fill =cr$cols, legend = cr$levels,col =cr$cols, cex=1,bty="n",bg="n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
