#' Visualizing using a choropleth map the mean total flight delays. 
#' 
#' The function visualizes the mean total flight delays from different aiports in the US. By using the data \code{airports} and \code{flights} from the package \code{\link{nycflights13}}, the mean total time delays were calculated.
#' 
#' @return A ggplot object visualizes into a choropleth map.
#'
#' @export


visualize_airports_delay<- function(){
  
library(dplyr)
library(ggplot2)
library(nycflights13)  

# subsetting the important variables for the analysis #
airports_sub<- select(airports, faa, name, lat, lon)
flights_sub<- select(flights, dest, dep_delay, arr_delay)

# Total time of delays #
new_flights<- mutate(flights_sub, total_delay = dep_delay + arr_delay)

# Filtering NAs#

# Merging the two data frames #
airFlight<- inner_join(airports_sub, new_flights, by=c("faa"="dest"))

# Compute total mean delay of flights by airport #
mean_delay<- airFlight %>% group_by(faa) %>% summarise(total_mean = mean(total_delay, na.rm=T))

# Creating new data set for plotting. Total mean delay for every airport #
airFlight_delay<- inner_join(airports_sub, mean_delay, by="faa")
#airFlight_delay<- merge(airports_sub, mean_delay, by.x="faa" )

# To get states name we use function latlong2state # 
airFlight_delay$States<- latlong2state(airFlight_delay[,c("lon", "lat")])

airFlight_delay <- airFlight_delay %>% select(States, total_mean, lon, lat) 

# Plotting the data #
states<- map_data("state")

air_mapdata<- semi_join(airFlight_delay, states, by=c("States"="region")) 

map_base<- ggplot() + 
  geom_polygon(data = states, mapping = aes(x = long, y = lat, group=group),color = "gray", fill = "white")

plot<- map_base + geom_point(data = air_mapdata, aes(x= lon, y=lat, color=total_mean)) +
  ggtitle("Mean total flight delays for different airports")

noAxes<- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

plot + noAxes

}
