CalSimil <- function (bxy,cxy)
{
  
  #compare eficiencies
  be <- CalEff(bxy$X,bxy$Y)[[3]]
  ce <- CalEff(cxy$X,cxy$Y)[[3]]
  de <- be-ce

  #interpolate data when time-series are of unequal length
  cl<-max(c(nrow(bxy),nrow(cxy)))
  if (nrow(cxy) < nrow(bxy)) {
    nxi <- approx(cxy$X, method="linear", n=cl)
    nyi <- approx(cxy$Y, method="linear", n=cl)
    cxy <- data.table(X=nxi$y,Y=nyi$y)
  } else if (nrow(cxy) > nrow(bxy)) {
    cxi <- approx(bxy$X, method="linear", n=cl)
    cyi <- approx(bxy$Y, method="linear", n=cl)
    bxy <- data.table(X=cxi$y,Y=cyi$y)
  }
  
  #get average nearest neighbour distance
  nn <- round(CalNN(bxy,cxy),digits=1)
  
  #convert to angles
  ch <- cart2pol(diff(bxy$X) , diff(bxy$Y), degrees = T)$theta
  nh <- cart2pol(diff(cxy$X) , diff(cxy$Y), degrees = T)$theta
  
  #calculate prediction error
  prd_err <- CalAngPErr(bxy,cxy,ch,nh)
  ape <- prd_err[1]

  #merge dataset
  mh <- c(ch,nh)
  
  #perform bin ranking
  rh <- RankHist(mh)
  #split data
  rch <- rh[1:(cl-1)]
  rnh <- rh[cl:length(rh)]
  rnh <- rnh[!is.na(rnh)]
  
  #perform mutual info calculation
  nmi <- round(CalNMI(rch,rnh),digits=3)
  
  #assemble results
  return(c(be,ce,de,nn,ape,nmi))
}