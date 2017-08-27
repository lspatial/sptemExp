

colorGrinf = function(x, levels=NA, colors=c("green","yellow","red"), colsteps=10) {
  if(inherits(levels,"logical")){
     levels=seq(min(x,is.na=TRUE),max(x,is.na=TRUE), length.out=colsteps)
  }else{
     colsteps=length(levels)
  }
  levStr=c()
  for(i in c(1:(length(levels)-1))){
    levStr=c(levStr,paste(round(levels[i],0),"-",round(levels[i+1],0),sep=""))
  }
  levStr=c(levStr,paste(round(levels[length(levels)],0),"-",sep=""))
  drg=findInterval(x,levels)
  colors_gr=colorRampPalette(colors) (length(levels))
  result=list(levels=levStr,cols=colors_gr,index=drg)
  return(result)
}
