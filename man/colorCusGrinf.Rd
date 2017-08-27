\name{colorCusGrinf}
\alias{colorCusGrinf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Customed Color Generation by the Number of the Levels }
\description{A function for generation of colors by the numebr of levels for use in the map making. }
\usage{
colorCusGrinf(brkpts, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{brkpts}{a vector to contain the breakpoints }
  \item{cols}{Selection of colors for different timelines. }
}
\value{A colors of gradient levels}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{

data("spointspre","countylayer")
praster=sptemExp::points2Raster(spointspre,"d91")
dtStr=as.character(as.Date(91,origin=as.Date("2014-01-1")))
title=expression("PM"[2.5]*" Concentration Estimated")
par(mar=c(4,4,1,1))
breakpoints = c(0,50,100,200,350,600)
colors=colorCusGrinf(breakpoints,c("darkgreen","yellow","darkred"))
raster::plot(praster,breaks=breakpoints,col=colors,
    main=title,xlab=paste("Shandong Province, China (",dtStr,")",sep=""))

}
\keyword{ ~colorCusGrinf }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~raster }% __ONLY ONE__ keyword per line
