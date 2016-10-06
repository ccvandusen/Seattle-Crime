#
#
# Creating various heatmaps of seattle reported incident locations
#
#
#disclaimer:
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=tartu&zoom=12&size=%20640x640&scale=%202&maptype=satellite&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=tartu&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms





#finding the min and max long and lat to size the map correctly
min(Crime_Data[,13], na.rm=TRUE)
max(Crime_Data[,13], na.rm=TRUE)
min(Crime_Data[,14], na.rm=TRUE)
max(Crime_Data[,14], na.rm=TRUE)

#
# I borrowed much of this ggmap code from this helpful link: 
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html
#


# Creating a map to heat
Seattle_Map <- get_map(location = c(lon = -122.350876, lat = 47.620499), 
                       maptype = "hybrid", 
                       zoom = 11)

# Drawing a map w/ red dots for incident location
ggmap(Seattle_Map, extent = "device") + 
  geom_point(aes(x = Longitude, y = Latitude),
             colour = "red", alpha = 0.5, size = 0.4, 
             data = Giflist[[1]], 
             na.rm = TRUE)

# Drawing a map w/ countours for incident density
ggmap(Seattle_Map, extent = "device") + 
  geom_density2d(data = CrimeData2012[CrimeData2015$Event.Clearance.Group == "PROSTITUTION",], 
                 aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = CrimeData2012[CrimeData2015$Event.Clearance.Group == "PROSTITUTION",], 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +  
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.5), guide = FALSE)


# Idea to use Image Maverick and make this 
# from https://www.r-bloggers.com/animated-plots-with-r/

setwd("/Users/ChrisV/Documents/CrimePNG ")

for(i in 1:32){
  #Creating a name for each plot with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png', sep='')}
  if (i >= 10) {name = paste('00',i,'plot.png', sep='')}
 
  a <- ggmap(Seattle_Map, extent = "device") + 
    geom_point(aes(x = Longitude, y = Latitude),
               colour = "red", alpha = 1, size = 0.7, 
               data = Giflist[[i]], 
               na.rm = TRUE) +
    ggtitle(paste0("Month ", i, " Arrest Locations"))
  
  png(name)
  print(a)
  dev.off()
  
  }


for(i in 1:32){
  #Creating a name for each plot with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png', sep='')}
  if (i >= 10) {name = paste('00',i,'plot.png', sep='')}
  
  b <- ggmap(Seattle_Map, extent = "device") + 
    geom_density2d(data = Giflist[[i]], 
                   aes(x = Longitude, y = Latitude), size = 0.3) + 
    stat_density2d(data = Giflist[[i]], 
                   aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                   bins = 16, geom = "polygon") +  
    scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.5), guide = FALSE) +
    ggtitle(paste0("Month ", i, " Robbery Densities"))
  
  png(name)
  print(b)
  dev.off()
  
}







