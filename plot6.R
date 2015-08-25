###############################################################################################

## Coursera Exploratory Data Analysis Final Course Project
## Christopher Skyi
## Sunday, August 23, 2015, New York City, [86-74]F, 60% chance of thunderstorms by evening

###########################################################################

# QUESTION 6: Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California. 
# Which city has seen greater changes over time in motor vehicle emissions?

###########################################################################
library(dplyr)
library(ggplot2)

# PM2.5 Emissions Data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Check for any missing data
any(is.na(NEI$Emissions)) # returns TRUE if at least one NA found

# Source Classification Code Table
SCC <- readRDS("Data/Source_Classification_Code.rds")

vehicle.scc <- as.character(SCC[grep("^Mobile -(.*) vehicle", SCC$EI.Sector, ignore.case=T),1])

# A neat trick with dplyr and filter 
gettheserows <- NEI$SCC %in% vehicle.scc

# create a dataset of mean emissions for Baltimore & Los Angeles County, sum total emission for each year
vehicle.emissions <- NEI %>% 
   filter(SCC = gettheserows) %>% 
   filter(fips == "24510" | fips == "06037") %>% 
   select(Emissions, fips, year) %>%
   group_by(fips, year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# change city codes to city names
vehicle.emissions$fips[1:4] = "LA"
vehicle.emissions$fips[5:8] = "Baltimore"

# convert to plotable categorical data to factors (and years to ordered factors)
vehicle.emissions$fips <- as.factor(vehicle.emissions$fips)
vehicle.emissions$year <- factor(vehicle.emissions$year, levels = c("1999","2002","2005", "2008"), ordered = TRUE)

# NOTE: Column names CANNOT have spaces, or the qplot below fails (no idea why)
new.var.names <- colnames(vehicle.emissions)
new.var.names[1] <- "City"
new.var.names[2] <- "Year"
new.var.names[3] <- "PM2.5"
colnames(vehicle.emissions) <- new.var.names

# make sure everything is OK
g <- qplot(Year,PM2.5, main = "Total PM2.5 Emission Source Time Trend: Baltimore vs L.A.", data = vehicle.emissions, group = City, geom=c("point", "smooth"), method="lm", formula=y~x)
g <- g + geom_line(size = 1, aes(colour = City)) + geom_point(size =5, pch = 21, fill = "salmon", alpha = .5)
g <- g + annotate("text", col = "red", x = 2.5, y = 5400, label = "L.A. Emissions have Increased Over Time")
g <- g + annotate("text", col = "red", x = 2.5, y = 2800, label = "Baltimore & L.A. PM2.5 have Roughtly Changed")
g <- g + annotate("text", col = "red", x = 2.5, y = 2600, label = "equally Over Time, but in Opposite Directions")
g <- g + annotate("text", col = "red", x = 2.5, y = 2400, label = "(see blue trend lines)")
g <- g + annotate("text", col = "red", x = 2.5, y = 1000, label = "Baltimore Emissions have Decreased Over Time")

# write barplot to png file, in the same folder as this script
png(file="plot6.png",width=480,height=480)

g

dev.off()


