#--------Analysis JEV outbreak in India---------#



#loading the required packages

library(sf)      
library(terra)   
library(raster)
library(tidyverse)
library(tmap)
library(spdep)
library(spatstat)
library(maptools)
library(dismo)
library(viridis)


#loading shapefile data for states and districts
states = read_sf("India_State/India_State.shp")
districts = read_sf("India_Districts/India_Districts.shp")


#loading data for the outbreaks
jev_df = read_csv("jev.occ.idsp.csv")

#checking for unusual value in latitude and longitude
summary(jev_df$latitude)
summary(jev_df$longitude)

#converting dataframe to spatial object and setting coordinate as well as a coordinate reference system
jev_sf = jev_df %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs("EPSG:4326") 

## QUESTION 1 ##

plot(states[0], col = "lightgrey", border = "white")
plot(jev_sf[0], add = T, col = "red")

plot(districts[0], col = "lightgrey", border = "white")
plot(jev_sf[0], add = T, col = "red")



#create new column to count the number of outbreak in each district using dplyr
districts = mutate(districts, JEV_count = lengths(st_intersects(districts, jev_sf))) 

tm_shape(districts) +
  tm_polygons(col = "JEV_count", title = "JEV outbreaks     ", 
              border.col = "black", style = "jenks", n = 5, legend.reverse = T, # set the number of color to 5
              palette = c("#ffffcc", "#a1dab4","#41b6c4", "#2c7fb8", "#253494")) + # define the five color to use
  tm_grid(ticks = T, lines = F) +
  tm_scale_bar(breaks = c(0, 200, 400), text.size = 1, position = c("left", "bottom")) + #add scale bar
  tm_compass(type = "rose", size = 5, position = c("left", "top")) #add compass




districts.nb = poly2nb(districts) #extract topology information from spatial polygon object
districts.lw = nb2listw(districts.nb) # convert topology information to a weighted object to perform moran test

m_t = moran.test(districts$JEV_count, districts.lw) # Moran's index
m_t



#transform jev spatial polygon object to a point process object
jev.ppp = as.ppp(
  X = st_coordinates(jev_sf), 
  W = as.owin(st_bbox(states))
)

kde = density.ppp(jev.ppp) #compute kernel density estimate
kde.ras = rast(kde) #convert to raster data

#cleaning of raster data to allow data visualisation
kde.ras = crop(kde.ras, vect(states))
kde.ras = mask(kde.ras, vect(states))

#plot kernel density with color palette from the viridis package
plot(kde.ras, 
     col = viridis(n = 20, begin = 0.2, direction = -1, option = "A")) +
plot(states[0], border = "black", add = T)




#computes 99 simulation envelopes of Ripley's K function using point process object of JEV outbreak
env_kest_jev = envelope(jev.ppp, Kest) 
plot(env_kest_jev, main = NULL, xlab = "Distance (km)", ylab = "K-value")



#obtention of raster data representing the distance in km from a river
river_dist = rast("river.dist.km.tif")

#cleaning of raster data to allow data visualisation
river_dist = crop(river_dist, vect(states))
river_dist = mask(river_dist,vect(states))
river_dist = aggregate(river_dist, fact = 5, fun = mean, na.rm = TRUE)

#define color function to plot the river raster data
colfunc = colorRampPalette(c("skyblue", "darkorange"))
plot(river_dist, breaks = seq (0,400, by = 50), col = colfunc(100)) # plot river distance
plot(states[0], border = "white", add = T) #add states of India
plot(jev.ppp, col = "black", pch = 1, add = T) # add JEV outbreaks
