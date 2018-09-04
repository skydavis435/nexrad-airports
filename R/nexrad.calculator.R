nexrad.calculator <- function(){
  #http://training.weather.gov/wdtd/tools/misc/beamwidth/index.htm
  #https://www.roc.noaa.gov/WSR88D/Engineering/NEXRADTechInfo.aspx
  #Calculates beam characteristics at range above radar level
  
  #Constants
    Re <- 6371 #Radius of Earth in km
  #Variables
    R <- 100 #Slant Range observed on radar (km)
    PHI <- 0.5 #Radar elevation angle (degrees)
    IR <- 1.21 #Refractive Index
    theta <- 0.925 #angular beamwidth
    r <- cos(PHI*pi/180) * R #distance of the range gate from the antenna (km)
  #Calculate
    beam.center <- (R*sin(PHI*pi/180)) + (R^2/(2*IR*Re))
    s <- r * (theta*pi/180)
    beam.perpendicular.height <- s * cos(PHI * pi/180)
    beam.top <- beam.center + (0.5 * beam.perpendicular.height) 
    beam.bottom <- beam.center - (0.5 * beam.perpendicular.height)
}