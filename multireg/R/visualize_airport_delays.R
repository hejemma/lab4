#' Visualizing using a choropleth map the mean total flight delays. 
#' 
#' The function visualizes the mean total flight delays from different aiports in the US. By using the data \code{airports} and \code{flights} from the package \code{nycflights13}, the mean total time delays were calculated.
#' @import MASS
#' @return A ggplot object visualizes into a choropleth map.
#' @export


visualize_airports_delay<- function(){
#library(dplyr)
#library(ggplot2)
#library(nycflights13)  

# subsetting the important variables for the analysis #
airports_sub<- dplyr::select(airports, faa, name, lat, lon)
flights_sub<- dplyr::select(flights, dest, dep_delay, arr_delay)

# Total time of delays #
new_flights<- dplyr::mutate(flights_sub, total_delay = dep_delay + arr_delay)

# Merging the two data frames #
airFlight<- dplyr::inner_join(airports_sub, new_flights, by=c("faa"="dest"))

# Compute total mean delay of flights by airport #
mean_delay<- airFlight %>% dplyr::group_by(faa) %>% dplyr::summarise(total_mean = mean(total_delay, na.rm=T))

# Creating new data set for plotting. Total mean delay for every airport #
airFlight_delay<- dplyr::inner_join(airports_sub, mean_delay, by="faa")

# To get states name we use function latlong2state # 
airFlight_delay$States<- latlong2state(airFlight_delay[,c("lon", "lat")])

airFlight_delay <- airFlight_delay %>% dplyr::select(States, total_mean, lon, lat) 

# Plotting the data #
world<- ggplot2::map_data("world")

USA<- dplyr::filter(world, region == c("USA", "Canada"))

map_base<- ggplot() + coord_cartesian(xlim=c(-160,-45), ylim=c(min(USA$lat),65), expand=F)+
  geom_polygon(data = USA, mapping = aes(x = long, y = lat, group=group),color = "gray", fill = "white") 
  
plot<- map_base + geom_point(data = airFlight_delay, aes(x= lon, y=lat, color=total_mean, size=total_mean)) +
  ggtitle("Mean total flight delays for different airports")

plot

}
