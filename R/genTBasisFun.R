

getTBasisFun=function(serDf,idStr,dateStr,valStr,df=25,n.basis=2,tbPath=NA){
  #serDf=shdSeries2014;idStr="siteid";dateStr="date";valStr="obs";df=10;n.basis=2;tbPath="/tmp"

  basisFunc=SpatioTemporal::calcSmoothTrends(obs=log(serDf[,valStr]), date =serDf[,dateStr],
                             ID = serDf[,idStr],df=df,n.basis=n.basis,cv=TRUE)
  dates_ser=unique(serDf[,dateStr])
  dates_ser=dates_ser[order(dates_ser)]

  cmdStr="data.frame(date=dates_ser"
  for(i in c(1:n.basis)){
    cmdStr=paste(cmdStr,", pv",i,"=basisFunc$trend$V",i,sep="")
  }
  cmdStr=paste(cmdStr,")",sep="")
  season_trends=eval(parse(text=cmdStr))
#  season_trends$tid=season_trends$date-as.Date("01/01/2014",format="%m/%d/%Y")+1
  if(inherits(tbPath,"logical"))
    return(season_trends)
  for(i in c(1:n.basis)){
    fl=paste0(tbPath,"/se",i,".png",sep="")
    png(filename=fl,width=900,height=400)
    par(mar=c(4,4.3,1,1))
    tarV=paste0("V",i)
    range(basisFunc$trend[,tarV])
    plot(dates_ser,basisFunc$trend[,tarV],type="l",col="blue",ylim=c(-2.5,2.5),xlab="Year",
         ylab=paste0(i,"st temporal component"))
    dev.off()
  }
  return(season_trends)
}


