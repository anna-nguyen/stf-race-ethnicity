

# format point estimate and ci
pt.est.ci.f=function(est, lb, ub,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),est*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),lb*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),ub*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}
