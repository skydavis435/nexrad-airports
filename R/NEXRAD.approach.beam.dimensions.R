NEXRAD.approach.beam.dimensions <- function(n = 1, aircraft.altitude.ft = c(500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000)){
  #Require,
    require(geosphere)
    require(ggmap)
    require(ggthemes)
    require(RColorBrewer)
  
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"NEXRAD-Airports")
  
  #################### <MODIFY ME> ##################### 
    Re <- 6371000 #Radius of Earth in m 
    theta <- 0.925 #Normalized Nexrad Beamwidth Degrees
    PHI <- 0.5 #Nexrad Elevation Angle
    IR <- 1.21 #Refractive Index
  #################### </MODIFY ME> ##################### 
  
  #Load up all the data
    distances<-read.csv(file = file.path(working.directory,"Data","all.distances.csv"),header = T)
    facilities<-read.csv(file = file.path(working.directory,"Data","NfdcFacilities.csv"),header = T,stringsAsFactors = F)
    runways<-read.csv(file = file.path(working.directory,"Data","NfdcRunways.csv"),header = T,stringsAsFactors = F)
    nexrads<-read.csv(file = file.path(working.directory,"Data","nexrad_loc.csv"),header=T)
  #Process facilities/runways
    facilities$LocationID<-gsub(pattern = "'",replacement = "",x = facilities$LocationID)
  #Fix Names  
    runways$RunwayID<-gsub(pattern = "'",replacement = "",x = runways$RunwayID)
    runways$BaseEndID<-gsub(pattern = "'",replacement = "",x = runways$BaseEndID)
    runways$ReciprocalEndID<-gsub(pattern = "'",replacement = "",x = runways$ReciprocalEndID)
  #Units (Decimal degrees and meters)
    #Base End
      BaseEndPhysicalLatitude.deg <- as.numeric(substr(x = runways$BaseEndPhysicalLatitude,start = 1,stop = 2))
      BaseEndPhysicalLatitude.min<- as.numeric(substr(x = runways$BaseEndPhysicalLatitude,start = 4,stop = 5))
      BaseEndPhysicalLatitude.sec<- as.numeric(substr(x = runways$BaseEndPhysicalLatitude,start = 7,stop = 13))
      runways$BaseEndPhysicalLatitude.decimal<-(BaseEndPhysicalLatitude.sec/60 + BaseEndPhysicalLatitude.min)/60 + BaseEndPhysicalLatitude.deg
      BaseEndPhysicalLongitude.deg <- as.numeric(substr(x = runways$BaseEndPhysicalLongitude,start = 1,stop = 3))
      BaseEndPhysicalLongitude.min<- as.numeric(substr(x = runways$BaseEndPhysicalLongitude,start = 5,stop = 6))
      BaseEndPhysicalLongitude.sec<- as.numeric(substr(x = runways$BaseEndPhysicalLongitude,start = 8,stop = 14))
      runways$BaseEndPhysicalLongitude.decimal<--1*((BaseEndPhysicalLongitude.sec/60 + BaseEndPhysicalLongitude.min)/60 + BaseEndPhysicalLongitude.deg)
      runways$BaseEndPhysicalElevation.m<-runways$BaseEndPhysicalElevation*0.3048
    #Reciprocal End
      ReciprocalEndPhysicalLatitude.deg <- as.numeric(substr(x = runways$ReciprocalEndPhysicalLatitude,start = 1,stop = 2))
      ReciprocalEndPhysicalLatitude.min<- as.numeric(substr(x = runways$ReciprocalEndPhysicalLatitude,start = 4,stop = 5))
      ReciprocalEndPhysicalLatitude.sec<- as.numeric(substr(x = runways$ReciprocalEndPhysicalLatitude,start = 7,stop = 13))
      runways$ReciprocalEndPhysicalLatitude.decimal<-(ReciprocalEndPhysicalLatitude.sec/60 + ReciprocalEndPhysicalLatitude.min)/60 + ReciprocalEndPhysicalLatitude.deg
      ReciprocalEndPhysicalLongitude.deg <- as.numeric(substr(x = runways$ReciprocalEndPhysicalLongitude,start = 1,stop = 3))
      ReciprocalEndPhysicalLongitude.min<- as.numeric(substr(x = runways$ReciprocalEndPhysicalLongitude,start = 5,stop = 6))
      ReciprocalEndPhysicalLongitude.sec<- as.numeric(substr(x = runways$ReciprocalEndPhysicalLongitude,start = 8,stop = 14))
      runways$ReciprocalEndPhysicalLongitude.decimal<--1*((ReciprocalEndPhysicalLongitude.sec/60 + ReciprocalEndPhysicalLongitude.min)/60 + ReciprocalEndPhysicalLongitude.deg)
      runways$ReciprocalEndPhysicalElevation.m<-runways$ReciprocalEndPhysicalElevation*0.3048
    #Remove runways with insufficient information
      runways<-subset(runways, is.na(BaseEndPhysicalLongitude.decimal) == F & is.na(BaseEndPhysicalLatitude.decimal) == F & is.na(ReciprocalEndPhysicalLongitude.decimal) == F & is.na(ReciprocalEndPhysicalLatitude.decimal) == F  )
  #Loop through every airport
    #Show some progress
      pb<- txtProgressBar(min=0,max=nrow(distances),style=3)
      setTxtProgressBar(pb,0)  
    for(i in 1: nrow(distances)){
      what.the.hell<- i
        assign("what.the.hell",what.the.hell,.GlobalEnv)
      #Find facility
        sub.facility<-subset(facilities, LocationID == distances$X[i])
      #Find runways for facility
        sub.runways<-subset(runways, SiteNumber == sub.facility$SiteNumber[1])
        if(nrow(sub.runways)>=1)
      {#Check for alignment
        sub.runways$BaseEndTrueAlignment<-ifelse(is.na(sub.runways$BaseEndTrueAlignment)==T,bearing(p1 = cbind(sub.runways$BaseEndPhysicalLongitude.decimal,sub.runways$BaseEndPhysicalLatitude.decimal),p2 = cbind(sub.runways$ReciprocalEndPhysicalLongitude.decimal,sub.runways$ReciprocalEndPhysicalLatitude.decimal)),sub.runways$BaseEndTrueAlignment)
        sub.runways$ReciprocalEndTrueAlignment <- ifelse(is.na(sub.runways$ReciprocalEndTrueAlignment)==T,bearing(p2 = cbind(sub.runways$BaseEndPhysicalLongitude.decimal,sub.runways$BaseEndPhysicalLatitude.decimal),p1 = cbind(sub.runways$ReciprocalEndPhysicalLongitude.decimal,sub.runways$ReciprocalEndPhysicalLatitude.decimal)),sub.runways$ReciprocalEndTrueAlignment)
      #Generate a runway map for reference
        airport.code<-sub.facility$LocationID[1]
        title<-paste(airport.code,"Runways")
        sub.base<- data.frame(sub.runways$BaseEndPhysicalLongitude.decimal,sub.runways$BaseEndPhysicalLatitude.decimal)
        colnames(sub.base)<-c("Lon","Lat")
        sub.reciprocal<- data.frame(sub.runways$ReciprocalEndPhysicalLongitude.decimal,sub.runways$ReciprocalEndPhysicalLatitude.decimal)
        colnames(sub.reciprocal)<-c("Lon","Lat")
        bounds<- rbind(sub.base,sub.reciprocal)
        map<-get_map(location = make_bbox(lon = bounds$Lon,lat = bounds$Lat,f = 1),zoom = 13,maptype = "satellite",color = "bw")
      ##Get map attirbutes (corners)  
        bb<-attr(map,"bb")
        assign("bb",bb,.GlobalEnv)
        ##Use map attributes to create a scale bar
        sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                           lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                           lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                          lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
        sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
        ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
        rnw.map<-ggmap(map) + geom_segment(data = sub.runways,aes(x=BaseEndPhysicalLongitude.decimal,BaseEndPhysicalLatitude.decimal,xend=ReciprocalEndPhysicalLongitude.decimal,yend=ReciprocalEndPhysicalLatitude.decimal),color="black",size=2.2)+ geom_segment(data = sub.runways,aes(x=BaseEndPhysicalLongitude.decimal,BaseEndPhysicalLatitude.decimal,xend=ReciprocalEndPhysicalLongitude.decimal,yend=ReciprocalEndPhysicalLatitude.decimal,color=RunwayID),size=2) + scale_color_brewer(palette = "RdYlBu") + ggtitle(title) 
        rnw.map.2<- rnw.map + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")  +theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
        rnw.map.final<-rnw.map.2 + theme_map()
        map.filename<-paste0(airport.code,"_Runway_Map.png")
        dir.create(path = file.path(working.directory,"Results","Beam_Calculations",airport.code),showWarnings = F)
        ggsave(filename = file.path(working.directory,"Results","Beam_Calculations",airport.code,map.filename),plot = rnw.map.final, width= 10,height=10.2,pointsize=12,units="in" )
      #Find nth closest NEXRAD
        nex.names<-colnames(distances)
        nex.names<-nex.names[2:length(nex.names)]
        dist.from.airport<-as.numeric(distances[i,])
        dist.from.airport<-dist.from.airport[2:length(dist.from.airport)]
        dist.df<-data.frame(nex.names,dist.from.airport)
        colnames(dist.df)<-c("NEXRAD","Distance.km")
        ordered.df<-dist.df[order(dist.df$Distance.km),]
        nth.closest<-as.character(ordered.df$NEXRAD[n])
        sub.nexrads<- subset(nexrads, NEXRAD.ID == nth.closest)
      #Trim and combine Dataframes
        base<-subset(sub.runways, is.na(BaseEndGlidePathAngle) == F)
        base.df<-data.frame(base$RunwayID,base$BaseEndTrueAlignment,base$BaseEndGlidePathAngle,base$BaseEndPhysicalLongitude.decimal,base$BaseEndPhysicalLatitude.decimal,base$BaseEndPhysicalElevation.m)
        #base.df$End<-"Runway.Base"
        colnames(base.df)<-c("ID","RunwayTrueAlignment","Glide.Path.Angle","Lon","Lat","Elev.m")
        
        reciprocal<-subset(sub.runways, is.na(ReciprocalEndGlidePathAngle) == F)
        reciprocal.df<-data.frame(reciprocal$RunwayID,reciprocal$ReciprocalEndTrueAlignment,reciprocal$ReciprocalEndGlidePathAngle,reciprocal$ReciprocalEndPhysicalLongitude.decimal,reciprocal$ReciprocalEndPhysicalLatitude.decimal,reciprocal$ReciprocalEndPhysicalElevation.m)
        #reciprocal.df$End<-"Runway.Reciprocal"
        colnames(reciprocal.df)<-c("ID","RunwayTrueAlignment","Glide.Path.Angle","Lon","Lat","Elev.m")
        combined<-rbind(base.df,reciprocal.df)
      #Calculate NEXRAD Beam Characteristics
        combined$NEXRAD <- sub.nexrads$NEXRAD.ID
      #Loop through the aircraft altitude steps
        for(j in 1:length(aircraft.altitude.ft)){
          plane.alt.ft<-aircraft.altitude.ft[j]
          plane.alt.m<-plane.alt.ft * 0.3048
          dims<-combined
          dims$Aircraft.Alt.m <- plane.alt.m
          dims$Aircraft.Alt.msl <- dims$Aircraft.Alt.m + dims$Elev.m
          dims$Aircraft.Dist.from.Approach.m <- plane.alt.m/tan((dims$Glide.Path.Angle * (pi/180)))
          aircraft.position<- destPoint(p = cbind(dims$Lon,dims$Lat),b = dims$RunwayTrueAlignment - 180,d = dims$Aircraft.Dist.from.Approach.m)
          dims$Aircraft.Lon <- aircraft.position[,1]
          dims$Aircraft.Lat <- aircraft.position[,2]
          dims$Aircraft.Dist.to.NEXRAD.m <- distGeo(p1 = cbind(dims$Aircraft.Lon,dims$Aircraft.Lat),p2 = cbind(sub.nexrads$long..dec.deg.,sub.nexrads$Lat..dec.deg.))
          dims$Aircraft.Height.Relative.to.NEXRAD.m <- dims$Aircraft.Alt.msl - sub.nexrads$Elev..m..with.tower.height #Be mindful of Sea level
          dims$Slant.Range.m <- sqrt((dims$Aircraft.Dist.to.NEXRAD.m^2 + dims$Aircraft.Height.Relative.to.NEXRAD.m^2)) 
          dims$NEXRAD.Center.m <- (abs(dims$Slant.Range.m)*sin(PHI*pi/180)) + (abs(dims$Slant.Range.m)^2/(2*IR*Re))
          dims$NEXRAD.Center.msl <- dims$NEXRAD.Center.m + sub.nexrads$Elev..m..with.tower.height
          dims$NEXRAD.Beamwidth.m <- dims$Aircraft.Dist.to.NEXRAD.m * (theta*pi/180)
          dims$NEXRAD.Beam.Perpendicular.Height.m <- dims$NEXRAD.Beamwidth.m * cos(PHI * pi/180)
          dims$NEXRAD.Top.m <- dims$NEXRAD.Center.m + (0.5 * dims$NEXRAD.Beam.Perpendicular.Height.m)
          dims$NEXRAD.Top.msl <- dims$NEXRAD.Top + sub.nexrads$Elev..m..with.tower.height
          dims$NEXRAD.Bottom.m <- dims$NEXRAD.Center.m - (0.5 * dims$NEXRAD.Beam.Perpendicular.Height.m)
          dims$NEXRAD.Bottom.msl <- dims$NEXRAD.Bottom.m + sub.nexrads$Elev..m..with.tower.height
          dims$In.Beam <- ifelse(dims$Aircraft.Alt.msl <= dims$NEXRAD.Top.msl & dims$Aircraft.Alt.msl >= dims$NEXRAD.Bottom.msl & dims$Aircraft.Dist.to.NEXRAD.m <= 230000 , "YES", "NO")
          dims$In.Beam <- factor(dims$In.Beam,levels = c("YES","NO"))
          #Map it
          #if(nrow(dims)==1){
          #  map.2<-get_map(location = airport.code ,maptype = "satellite",color = "bw",zoom=13) 
          #}else{
          #  map.2<-get_map(location = make_bbox(lon = dims$Aircraft.Lon,lat = dims$Aircraft.Lat,f = 1) ,maptype = "satellite",color = "bw")
          #}
           
          #  title.2 <- paste(airport.code,"Runway Approaches")
          #  subtitle.2 <- paste("Aircraft at", plane.alt.ft,"ft above runway approach end")
          #  map.2.filename<-paste0(paste0(airport.code,"_",sub.nexrads$NEXRAD.ID,"_",plane.alt.ft,"ft_Alt_Approaches_Map.png"))
          #  ##Get map attirbutes (corners)  
          #  bb<-attr(map.2,"bb")
          #  assign("bb",bb,.GlobalEnv)
          #  ##Use map attributes to create a scale bar
          #  sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
          #                     lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
          #                     lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
          #                     lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
          #  sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
          #  ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
          #  approach.map<-ggmap(map.2,extent = "normal",maprange = F) 
          #  approach.map.1<-approach.map + geom_segment(data = dims,aes(x=Aircraft.Lon,y=Aircraft.Lat,xend=Lon,yend=Lat),color="blue") + geom_point(data = dims,aes(x=Aircraft.Lon,y=Aircraft.Lat,color = In.Beam)) + scale_color_brewer(palette = "Dark2")  + ggtitle(label = title.2,subtitle = subtitle.2)
          #  approach.map.2 <- approach.map.1 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
          #  approach.map.final<-approach.map.2 + theme_map()
          #  ggsave(filename = file.path(working.directory,"Results","Beam_Calculations",airport.code,map.2.filename),plot = approach.map.final, width= 10,height=10.2,pointsize=12,units="in" )
            
          #Save it
            csv.filename<-paste0(airport.code,"_",sub.nexrads$NEXRAD.ID,"_",plane.alt.ft,"ft_Alt_Approaches.csv")
            write.csv(x = dims,file = file.path(working.directory,"Results","Beam_Calculations",airport.code,csv.filename),row.names = F)
            
        }
        #Update the progress bar
          setTxtProgressBar(pb,i)
        
        }  else{print("moving right along")
          #Update the progress bar
          setTxtProgressBar(pb,i)
          }
    }
  #Save the parameters for this run
      parameters<-data.frame(Re,theta,PHI,IR)
      colnames(parameters) <- c("Radius of Earth (m)","Theta","PHI","IR")
      write.csv(x = parameters,file = file.path(working.directory,"Results","Beam_Calculations","parameters.csv"),row.names = F)  
  #Finish
    print("All done.")
    
    }
