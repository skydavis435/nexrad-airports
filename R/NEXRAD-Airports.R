#Geography, NEXRADS, and Airports

  #Load required packages
    require(geosphere)
    require(fields)
  
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"NEXRAD-Airports")
  
  #Load up the data from CSV
    airports<-read.csv(file.path(working.directory,"Data","NfdcFacilities_ALL.csv"),header = T)
    nexrads<-read.csv(file.path(working.directory,"Data","nexrad_loc.csv"),header = T)
    
  #Create matricies of names, lon, and lat
    a.matrix<-matrix(data = c(airports$ARP.Long.DecDeg,airports$ARP.Lat.DecDeg),ncol = 2,dimnames = list(airports$LocationID,c("lon","lat"))) #Airport Matrix
    n.matrix<-matrix(data = c(nexrads$long..dec.deg.,nexrads$Lat..dec.deg.),ncol = 2,dimnames = list(nexrads$NEXRAD.ID,c("lon","lat"))) #NEXRAD Matrix
    
  #Calculate all the distances, in kilometers 
    distance<-rdist.earth(x1 = a.matrix,x2 = n.matrix,miles = F)
    
  #From the above... find the closes airport nexrad pair plus the distance between  
    result <- t(sapply(seq(nrow(distance)), function(i) {
      j <- which.min(distance[i,])
      c(paste(rownames(distance)[i], colnames(distance)[j], sep='/'), distance[i,j])
    }))
    
  #Find X closest nexrad to airport (if needed)
    sort(distance[1,],decreasing = FALSE)[1]