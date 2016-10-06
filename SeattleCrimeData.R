library(stringr)
library(plyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(tidyr)

Crime_Data <- read.csv("SeattleIncident.csv", stringsAsFactors = FALSE)


# This gets all Event Clearance Group/subgroup/descriptions so I can view all the different
# categorizations of incidents the SPD has
unique((str_extract_all(TrueCrime$Event.Clearance.Group, "\\w+")))
unique((str_extract_all(TrueCrime$Event.Clearance.SubGroup, "\\w+")))
unique((str_extract_all(TrueCrime$Event.Clearance.Description, "\\w+")))


# removing reported incidents I don't want to look at like traffic incidents, false alarms, etc
TrueCrime <- Crime_Data[!(Crime_Data$Event.Clearance.Group=="TRAFFIC RELATED CALLS") & 
                          !(Crime_Data$Event.Clearance.Group=="FALSE ALARMS") &
                          !(Crime_Data$Event.Clearance.Group=="FALSE ALACAD") &
                          !(Crime_Data$Event.Clearance.Group=="HARBOR CALLS") &
                          !(Crime_Data$Event.Clearance.Group=="NULL") &
                          !(Crime_Data$Event.Clearance.SubGroup=="TRAFFIC RELATED CALLS"),]



#Reformatting the Event.Clearance.Date column into new column Date so I can subset the data into different years
TrueCrime$Date <- as.Date(as.character(TrueCrime$Event.Clearance.Date), "%m/%d/%Y")

#Subsetting Data into years
myYearList <- split(TrueCrime, format(TrueCrime$Date, "%Y"))
CrimeData2011 <- myYearList[["2011"]]
CrimeData2012 <- myYearList[["2012"]]
CrimeData2013 <- myYearList[["2013"]]
CrimeData2014 <- myYearList[["2014"]]
CrimeData2015 <- myYearList[["2015"]]
CrimeData2016 <- myYearList[["2016"]]

#
# Loop to create vectors of counts of reported incidents
#

#Now subsetting months (of all years combined)
myMonthList <- split(TrueCrime, format(TrueCrime$Date, "%m"))
ShopliftCount2 <- integer()
for(i in 1:12)
{
  # this line formats i correctly so that I can subset the groups into lists
 p <- str_pad(toString(i), 2, pad = "0")
 MonthCrime <- myMonthList[[p]]
 h <- nrow(MonthCrime[MonthCrime$Event.Clearance.Group == "SHOPLIFTING",]) 
 ShopliftCount2 <- append(ShopliftCount2, h) 
}

#
# Loop to create Matrices with specific names so we can create gifs
# of Crime Incident reported locations over time
#

MonthList14 <- split(CrimeData2014, format(CrimeData2014$Date, "%m"))
MonthList15 <- split(CrimeData2015, format(CrimeData2015$Date, "%m"))
MonthList16 <- split(CrimeData2016, format(CrimeData2016$Date, "%m"))

Giflist <- list()
for (i in 1:36){
  if (i <= 12){
    p <- str_pad(toString(i), 2, pad = "0")
    MonthCrime14 <- MonthList14[[p]]
    x <- MonthCrime14[MonthCrime14$Event.Clearance.Group == "ARREST",]
    Giflist[[i]] <- x

  }
  if (i > 12 && i <= 24){
    p <- i - 12
    p <- str_pad(toString(p), 2, pad = "0")
    MonthCrime15 <- MonthList15[[p]]
    y <- MonthCrime15[MonthCrime15$Event.Clearance.Group == "ARREST",]
    Giflist[[i]] <- y
    
  }
  if (i > 24 && i <= 36){
    p <- i - 24
    p <- str_pad(toString(p), 2, pad = "0")
    MonthCrime16 <- MonthList16[[p]]
    z <- MonthCrime16[MonthCrime16$Event.Clearance.Group == "ARREST",]
    Giflist[[i]] <- z
    
  }
}


