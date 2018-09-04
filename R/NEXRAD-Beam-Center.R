nexrad.beam.center <- function(slant.range,angle){
  #http://training.weather.gov/wdtd/tools/misc/beamwidth/index.htm
  #Calculates beam characteristics at range above radar level
  
  #Constants
    R <- 6371 #Radius of Earth
    refractive.index <- 1.21 #Refractive index
  #Calculate
    center.height <- (slant.range * sin(angle * pi/180)) + slant.range^2 / (2*refractive.index * R)
    top.height <- 0.94 * cos(.5 *pi/180)
}