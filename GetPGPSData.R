# DSFMD - Dynamic Systems Framework for Movement Data
# wrapper function to read GPS files
# Code by Ingo Schiffner 2018

GetPGPSData <- function (fullfile)
{
  TimeZone <- 'UTC'
  t_dat <- as.data.table(read.table(fullfile, header=T, sep=",", quote='"', dec=".", na.strings="", colClasses="character", fill=TRUE, strip.white=TRUE, blank.lines.skip=TRUE)) 
  t_dat<-t_dat[,1:6]
  colnames(t_dat)<-c('DATE','TIME','LAT','LON','ALT','SPEED')
  t_dat$LAT<-as.numeric(t_dat$LAT)
  t_dat$LON<-as.numeric(t_dat$LON)
  t_dat$ALT<-as.numeric(t_dat$ALT)
  t_dat$SPEED<-round(as.numeric(t_dat$SPEED)/1000, digits=1) 
  t_dat <- subset(t_dat,t_dat$SPEED>0)
  
  #merge date and time
  t_dat$TIME <- as.numeric(as.POSIXct(paste(t_dat$DATE,t_dat$TIME),"%Y/%m/%d %H:%M:%S", tz='UTC'))
  t_dat$DATE<-NULL
  
  #convert to utm
  sdf <- data.frame(t_dat$LAT,t_dat$LON)
  colnames(sdf) <- c("lat","lon")
  coordinates(sdf)<-~lon+lat
  proj4string(sdf)<-CRS(wgs84)
  sdf<-spTransform(sdf, utm)
  t_dat$LAT<-sdf@coords[,1]
  t_dat$LON<-sdf@coords[,2]
  colnames(t_dat)[2:3]<-c('X','Y')
  return(t_dat)
}