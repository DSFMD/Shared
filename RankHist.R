#histogram based ranking
RankHist <- function (x)
{
  bin <- floor(log(length(x))/ log(2))
  dpb <- floor(length(x)/bin)
  bin <- bin + 1
  b_cnt <- 0 #bin counter
  r_cnt <- 1 #rank counter
  xr<-x
  xr<-NA

  for (i in 1:length(x))
  {
    max_pos <- which(x==max(x,na.rm = T))
    min_pos <- which(x==min(x,na.rm = T))
    b_cnt <- b_cnt + 1
    # check current occupancy increase rank if necessary allow over occupancy in middle section
    if (b_cnt > dpb & r_cnt < bin)
    {
      b_cnt <- 0
      r_cnt <- r_cnt + 1
    }
    if (length(max_pos)>0 | length(min_pos)>0)
    {
      #set ranks and values as used
      if (length(max_pos)>0)
      {
        xr[max_pos] <- bin - r_cnt
        x[max_pos] <- NA
      }
      if (length(min_pos)>0) 
      {
        xr[min_pos] = r_cnt
        x[min_pos] = NA
      } 
    } else {
      #end rank assignment
      i<-length(x)
    }
  }
  return(xr)
}