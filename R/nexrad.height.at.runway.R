nexrad.height.at.runway <- function(csv, plane.range.mi,nexrad, PHI = 0.5, IR = 1.21,tower.height.m){
  #tower height resource
  #http://apollo.lsc.vsc.edu/classes/remote/lecture_notes/radar/88d/88D_locations.html
  #facilities and runway info
  #https://www.faa.gov/airports/airport_safety/airportdata_5010/
    
  #Require
  require(geosphere)
  #Constants
  Re <- 6371000 #Radius of Earth in m 
  theta <- 0.925
  #Define some variables.
  working.directory<-file.path(path.expand("~"),"NEXRAD-Airports")
  plane.range.m<- plane.range.mi * 1609.34
  #Load the Runway data
  setwd(file.path(working.directory,"Data","AirportsCSV"))
  rnw.data<-read.csv(csv,header = T)
  #Load the nexrad locations
  setwd(file.path(working.directory,"Data"))
  nex.data<-read.csv("nexrad_loc.csv",header = T)
  sub.nex.data <- subset(nex.data, NEXRAD.ID == nexrad)
  nexrad.location <- cbind(sub.nex.data$lon[1],sub.nex.data$Lat..dec.deg.[1])
  #Calculate things
  rnw.data$Airplane.Range.m <- plane.range.m
  rnw.data$Airplane.Alt.m <- (tan(3*pi/180)) * rnw.data$Airplane.Range.m
  temp <- as.data.frame(destPointRhumb(p = cbind(rnw.data$Approach.Lon,rnw.data$Approach.Lat),b = 360-rnw.data$Bearing.of.Approach,d = rnw.data$Airplane.Range.m))
  rnw.data$Dist.to.Nexrad.m <- distGeo(p2 = cbind(temp$lon,temp$lat), p1 = nexrad.location)
  rnw.data$Slant.Range.m <- sqrt((rnw.data$Dist.to.Nexrad.m^2 + rnw.data$Airplane.Alt.m^2))
  rnw.data$Nexrad.Center.m <- (rnw.data$Slant.Range.m*sin(PHI*pi/180)) + (rnw.data$Slant.Range.m^2/(2*IR*Re)) + tower.height.m
  rnw.data$Nexrad.Beamwidth.m <- rnw.data$Airplane.Range.m * (theta*pi/180)
  rnw.data$beam.perpendicular.height.m <- rnw.data$Nexrad.Beamwidth.m * cos(PHI * pi/180)
  rnw.data$Nexrad.Top.m <- rnw.data$Nexrad.Center.m + (0.5 * rnw.data$beam.perpendicular.height.m) 
  rnw.data$Nexrad.Bottom.m <- rnw.data$Nexrad.Center.m - (0.5 * rnw.data$beam.perpendicular.height.m) 
  rnw.data$In.Beam<-ifelse(rnw.data$Airplane.Alt.m <= rnw.data$Nexrad.Top.m & rnw.data$Airplane.Alt.m >= rnw.data$Nexrad.Bottom.m, "YES","NO")
  #Save
  filename<-paste0("Result_",csv)
  write.csv(x = rnw.data,file = file.path(working.directory,"Results",filename),row.names = F)
}