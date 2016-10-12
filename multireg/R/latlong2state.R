#' Converting latitude and longitude to state names
#' 
#' The function converts latitude and longitude to US state names. Especially used in function \code{visualize_airport_delay} 
#' @param pointsDF A data frame which column 1 contains the longitude in degrees (negative in US). 
#'        column 2 contains the latitude.
#' @return a character vector with the US states names.
#' @import sp
#' @import maps
#' @import maptools
#' @examples
#' testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
#' latlong2state(testPoints)
#' @export

latlong2state <- function(pointsDF) {
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  ID <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_spat <- map2SpatialPolygons(states, IDs=ID,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSPAT <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  index <- over(pointsSPAT, states_spat)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_spat@polygons, function(x) x@ID)
  stateNames[index]
}





