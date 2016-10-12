

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  
  library(sp)
  library(maps)
  library(maptools)
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- maps::map('state', fill=TRUE, col="transparent", plot=FALSE)
  ID <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_spat <- maptools::map2SpatialPolygons(states, IDs=ID,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSPAT <- sp::SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  index <- sp::over(pointsSPAT, states_spat)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_spat@polygons, function(x) x@ID)
  stateNames[index]
}





