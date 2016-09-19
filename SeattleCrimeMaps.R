library(ggplot2)
library(ggmap)
library(stringr)

# don't run this line unless you have to; big file!
Crime_Data <- read.csv("SeattleIncident.csv", stringsAsFactors = FALSE)


# This gets all Event Clearance Group/subgroup/descriptions so I can choose what Incidents
# to observe / omit and just view Incidents
unique((str_extract_all(Crime_Data$Event.Clearance.Group, "\\w+")))
unique((str_extract_all(Crime_Data$Event.Clearance.SubGroup, "\\w+")))
unique((str_extract_all(Crime_Data$Event.Clearance.Description, "\\w+")))

#Trying to find the incidents of Rape, there are none
sum(str_count(TrueCrime$Event.Clearance.Group, "Rape"))

# removing reported incidents I don't want to look at like traffic incidents, false alarms, etc
TrueCrime <- Crime_Data[!(Crime_Data$Event.Clearance.Group=="TRAFFIC RELATED CALLS") & 
                          !(Crime_Data$Event.Clearance.Group=="FALSE ALARMS") &
                          !(Crime_Data$Event.Clearance.Group=="FALSE ALACAD") &
                          !(Crime_Data$Event.Clearance.Group=="HARBOR CALLS") &
                          !(Crime_Data$Event.Clearance.Group=="NULL"),]


#Reformatting the Event.Clearance.Date column into new column Date so I can subset the data into different years
TrueCrime$Date <- as.Date(as.character(TrueCrime$Event.Clearance.Date), "%m/%d/%Y")

#Subsetting Data into years
myYearList <- split(TrueCrime, format(TrueCrime$Date, "%Y"))
CrimeData2011 <- myYearList[["2011"]]
CrimeData2012 <- myYearList[["2012"]]
CrimeData2013 <- myYearList[["2013"]]
CrimeData2014 <- myYearList[["2014"]]
CrimeData2015 <- myYearList[["2015"]]

#Testing new functions with a small subset so not to make running take too long
Subset <- read.csv("SeattleIncident.csv",nrows=300)
Subset$Event.Clearance.Date
Subset$Date <- as.Date(as.character(Subset$Event.Clearance.Date), "%m/%d/%Y")
SubsetHalf <- Subset[Subset$Date > as.Date("2010-07-17"),]
class(Subset$Date)
  Subset$Date
SubsetHalf$Date
SubsetHalf

#finding the min and max long and lat to size the map correctly
min(Crime_Data[,13], na.rm=TRUE)
max(Crime_Data[,13], na.rm=TRUE)
min(Crime_Data[,14], na.rm=TRUE)
max(Crime_Data[,14], na.rm=TRUE)

# I borrowed much of this ggmap code from this helpful link: 
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html

Homicide2011 <- CrimeData2011[CrimeData2011$Event.Clearance.Group == "HOMICIDE",]

# Creating a map to heat
Seattle_Map <- get_map(location = c(lon = -122.350876, lat = 47.620499), maptype = "hybrid", zoom = 11)

# Drawing the map
ggmap(Seattle_Map, extent = "device") + geom_point(aes(x = Longitude, y = Latitude),
                          colour = "red", alpha = 0.1, size = 0.3, data = CrimeData2012[CrimeData2012$Event.Clearance.Group == "PROSTITUTION",], na.rm = TRUE)

# Drawing a BETTER map
ggmap(Seattle_Map, extent = "device") + 
  geom_density2d(data = CrimeData2012[CrimeData2015$Event.Clearance.Group == "PROSTITUTION",], 
    aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = CrimeData2012[CrimeData2015$Event.Clearance.Group == "PROSTITUTION",], 
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.5), guide = FALSE)
#disclaimer:
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=tartu&zoom=12&size=%20640x640&scale=%202&maptype=satellite&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=tartu&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms

# Here are some mistakes I made, I'm keeping them here to learn, etc.

# old method for Subsetting Data into years, this way generates
# exactly 14198 rows filled with na values. Don't know why.
Crime_Data2010 <- TrueCrime[TrueCrime$Date < as.Date("2011-01-01"),]
Crime_Data2011 <- TrueCrime[TrueCrime$Date >= as.Date("2011-01-01") & TrueCrime$Date <as.Date("2012-01-01"),]
Crime_Data2012 <- TrueCrime[TrueCrime$Date >= as.Date("2012-01-01") & TrueCrime$Date <as.Date("2013-01-01"),]
Crime_Data2013 <- TrueCrime[TrueCrime$Date >= as.Date("2013-01-01") & TrueCrime$Date <as.Date("2014-01-01"),]
Crime_Data2014 <- TrueCrime[TrueCrime$Date >= as.Date("2014-01-01") & TrueCrime$Date <as.Date("2015-01-01"),]
Crime_Data2015 <- TrueCrime[TrueCrime$Date >= as.Date("2015-01-01") & TrueCrime$Date <as.Date("2016-01-01"),]
# this line removes all the na values from these subsets
Crime_Data2011 <- Crime_Data2011[rowSums(is.na(Crime_Data2011)) != ncol(Crime_Data2011), ]

sum(is.na(TrueCrime$Date >= as.Date("2011-01-01") & 
            TrueCrime$Date <as.Date("2012-01-01")))


#Don't run this unless you want to crash r like I did
Crime_Data[grep("Rape", Crime_Data),]
