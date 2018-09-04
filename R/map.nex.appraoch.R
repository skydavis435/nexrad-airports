map.nex.approach <- function(){
  #Require
    require(data.table)
    require(bit64)
    require(ggmap)
    require(ggthemes)
    require(RColorBrewer)
    require(geosphere)
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"NEXRAD-Airports")
  #Load some reference info
    airports<-read.csv(file.path(working.directory,"Data","Airports_all.csv"),header = T)
    nexrads<-read.csv(file.path(working.directory,"Data","nexrad_loc.csv"),header=T)
  #What will we be doing today
    setwd(file.path(working.directory,"Results","Beam_Calculations"))
    dirs<-list.dirs()
  #Show some progress
    pb<- txtProgressBar(min=0,max=length(dirs),style=3)
    setTxtProgressBar(pb,0)    
  #Loop through directories
    for(i in 194:length(dirs)){
      what.the.hell<-i
      assign("what.the.hell",what.the.hell,.GlobalEnv)
      setwd(file.path(working.directory,"Results","Beam_Calculations",dirs[i]))
    #Load up the data    
      filelist<-list.files(pattern="\\.csv$")
      datalist<-lapply(filelist,fread)
      data<-rbindlist(datalist)
    #Process a bit
      airport.code<-gsub(pattern = "./",replacement = "",x = dirs[i])
      nexrad.code<-data$NEXRAD[1]
      data$In.Beam <- factor(data$In.Beam,levels = c("YES","NO"))
      
    #Add back in plane height in feet
      data$Aircraft.Alt.ft<-data$Aircraft.Alt.m * 3.28084
      data$Aircraft.Alt.ft<-factor(round(data$Aircraft.Alt.ft,digits = 0))
    #Save an overall spreadsheet
      csv.filename<- paste0(airport.code,"_",nexrad.code,"_All_Approaches.csv")
      write.csv(x = data,file = file.path(working.directory,"Results","Beam_Calculations",dirs[i],csv.filename))
    #Find distance from airport to nexrad
      sub.airport<-subset(airports, LocationID == airport.code)
      sub.nexrad<-subset(nexrads, NEXRAD.ID == nexrad.code)
      dist.to.nexrad.m<-distGeo(p1 = cbind(sub.airport$ARP.Long.DecDeg,sub.airport$ARP.Lat.DecDeg),p2 = cbind(sub.nexrad$long..dec.deg.,sub.nexrad$Lat..dec.deg.))
      dist.to.nexrad.km<- round(dist.to.nexrad.m * 0.001,digits = 2)
    #Bounds
      pt1<-data.frame(data$Aircraft.Lon,data$Aircraft.Lat)
      colnames(pt1)<-c("Lon","Lat")
      pt2<-data.frame(sub.airport$ARP.Long.DecDeg,sub.airport$ARP.Lat.DecDeg)
      colnames(pt2)<-c("Lon","Lat")
      bounds<-rbind(pt1,pt2)
    #Grab a map to fit the data    
      map.2<-get_map(location = make_bbox(lon = bounds$Lon,lat = bounds$Lat,f = 1) ,maptype = "satellite",color = "bw")
    #Get Creative with the title  
      title.2 <- paste(airport.code,"Runway Approaches")
      subtitle.2 <- paste("Approach beam dimensions calculated from",nexrad.code, "which is",dist.to.nexrad.km,"km from airport")
      map.2.filename<-paste0(paste0(airport.code,"_",nexrad.code,"_","All_Approaches_Map.png"))
      ##Get map attirbutes (corners)  
        bb<-attr(map.2,"bb")
        assign("bb",bb,.GlobalEnv)
      ##Use map attributes to create a scale bar
        sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                           lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                           lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                           lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
        sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
        ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
      #Map the thing, finally  
        approach.map<-ggmap(map.2,extent = "normal",maprange = F) 
        approach.map.1<-approach.map + geom_segment(data = data,aes(x=Aircraft.Lon,y=Aircraft.Lat,xend=Lon,yend=Lat),color="blue") + geom_point(data = data,aes(x=Aircraft.Lon,y=Aircraft.Lat,color=In.Beam,shape = Aircraft.Alt.ft)) + scale_color_brewer(palette = "Dark2")  + ggtitle(label = title.2,subtitle = subtitle.2)
        approach.map.2 <- approach.map.1 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
        approach.map.final<-approach.map.2 + scale_shape_manual(values = 0:11)
        ggsave(filename = file.path(working.directory,"Results","Beam_Calculations",dirs[i],map.2.filename),plot = approach.map.final, width= 15,height=15.2,pointsize=12,units="in" )
      #Update the progress bar
        setTxtProgressBar(pb,i)
    }
print("Work's done. Let's go fly a kite!")
  }