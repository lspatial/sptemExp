
colorCusGrinf = function(brkpts,cols=c("green","yellow","red")) {
  return(colorRampPalette(cols) (length(brkpts)-1))
}
