#Prowling = Car Prowling

#
# Creating a stacked bar chart
#

CrimeCountMatrix <- matrix(c(RobberyCount, MentalCount, BurglaryCount, ShopliftCount, AssaultCount, ProwlingCount), nrow=length(AssaultCount),
                        dimnames=list(c("January","February","March","April","May","June","July",     
                                        "August","September","October","November","December"), 
                                         c("Robbery", "Mental Count", "Burglary", "Shoplifting", "Assaults", "Prowling")))

#Honestly, the melt() function is still pretty nebulous to me
#But it gets the Matrix formatted correctly so I'm using it!
CrimeCountMatrix2 <- melt(CrimeCountMatrix)

#Final Graph of Crimes / Month
ggplot(data = CrimeCountMatrix2, aes(x = Var1, y = value, fill = Var2)) + geom_bar(stat = "identity") + scale_fill_brewer(palette = 3) + ggtitle("Monthly Crime Rate") + labs(x = "Month", y= "# of Incidents") + theme(legend.title =element_blank())

#
# Creating a graph of multiple regression lines
#

#Creating a vector of dates to relate all the vector counts
indx <- seq(as.Date('2011-01-01'), length.out=24, by='1 month')
indx2 <- seq(as.Date('2014-01-01'), length.out=32, by='1 month')
month_axis <- c(indx, indx2)

# These vectors are counts of the # of incident reports each month
# Used the same loop to create these vectors that I did for the stacked barplot
# I just created a month list of each year subset I already had, which just meant
# I had to run the loop for each year of each type of incident - about 25 times
# Tried to find a more elegant solution but this dataset kinda sucks and I suck at coding
# so I gave up
ShopliftCount2
BurglaryCount2
ProwlingCount2
RobberyCount2
ShopliftCount2

#Creating the matrix - the 2013 incidents are much, much lower than other years
# - 40k incidents in 2013 vs 165k-202k for the other years so I omitted 2013
Regression_Matrix2 <- data.frame(Dates = month_axis, Assaults = AssaultCount2, Burglary = BurglaryCount2, Prowling = ProwlingCount2, Robbery = RobberyCount2, Shoplifting = ShopliftCount2)


#Reformatting matrix to work w/ ggplot2
Regression_Matrix <- gather(Regression_Matrix2, type, value, Assaults, Burglary, Prowling, Robbery, Shoplifting)


#Making the plot
ggplot(Regression_Matrix, aes(Dates, value, color = type)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.80) + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  ggtitle("Crime Trends 2011-2016") + labs(x = "Date", y = "# Of Incidents") +
  theme(legend.title = element_blank())

