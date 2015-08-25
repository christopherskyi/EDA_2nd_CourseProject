###############################################################################################

## Coursera Exploratory Data Analysis Final Course Project
## Christopher Skyi
## Sunday, August 23, 2015, New York City, [86-74]F, 60% chance of thunderstorms by evening

###########################################################################

# QUESTION 5: How have emissions from motor vehicle sources changed from 
# 1999-2008 in Baltimore City??

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

# create a dataset of mean emissions for Baltimore (fips == "24510"), sum total emission for each year
vehicle.emissions <- NEI %>% 
   filter(SCC = gettheserows) %>% 
   filter(fips == "24510") %>% 
   select(Emissions, year) %>%
   group_by(year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# write barplot to png file, in the same folder as this script
png(file="plot5.png",width=480,height=480)

barplot(vehicle.emissions$sum.pm, xlab="Year",  ylim = c(0, 400), main = "Total Baltimore  Motor Vehicle PM2.5 Trend (1999 - 2008)",
        ylab="Total PM2.5 Motor Vehicle Emission", names.arg=c("1999", "2002", "2005", "2008"))
        text(2.6,370, "Overall, Motor Vehicle Emissions Decreased from 1999 to 2008", col = "red")


dev.off()


