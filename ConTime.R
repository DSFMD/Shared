# DSFMD - Dynamic Systems Framework for Movement Data
# function to convert seconds to HH:MM:SS format (got fed up with posixct :)
# Code by Ingo Schiffner 2018

ConTime <- function (t)
{
  H <- t %/% 3600
  t <- t  - H*3600
  M <- t %/% 60
  S <- t - M*60
  H <- as.character(H)
  M <- as.character(M)
  S <- as.character(S)
  if (nchar(H)==1){
    H<-paste('0',H,sep="")
  }
  if (nchar(M)==1){
    M<-paste('0',M,sep="")
  }
  if (nchar(S)==1){
    S<-paste('0',S,sep="")
  }
  return(paste(H,M,S,sep=":"))
}