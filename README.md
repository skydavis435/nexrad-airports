# nexrad-airports
A series of R scripts to calculate the nearest NEXRAD station to an airport and whether or not the NEXRAD will have coverage at various distances from every airport runway's approach and departure path. 

This project utilizes existing data from the FAA on airport facilities, which inclues the lon lat elevation values for every runway in the united states. Approach departure paths are calculated for various distances from each individual runway and information is saved into a CSV file. A map is also produced, showing the approach/departure range and whether or not the NEXRAD beam will intersect with that elevation. 

In this way, it can be determined which airports in the country have coverage of approach departure paths by the NEXRAD sensors. 
