###############################################################################################

## Coursera Exploratory Data Analysis Final Course Project
## Christopher Skyi
## Sunday, August 23, 2015, New York City, [86-74]F, 60% chance of thunderstorms by evening

###########################################################################

# QUESTION 1: Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the total 
# PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

###########################################################################
library(dplyr)

NEI <- readRDS("Data/summarySCC_PM25.rds")

# Check for any missing data
any(is.na(NEI$Emissions)) # returns TRUE if at least one NA found

# create a dataset of sum total emission for each year
 pm.by.year <- NEI %>% 
   select(Emissions,year) %>% 
   group_by(year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# write barplot to png file, in the same folder as this script
png(file="plot1.png",width=480,height=480)

barplot(pm.by.year$sum.pm, xlab="Year",  main = "Total U.S. PM2.5 Emission Time Trend (1999 - 2008)",
        ylab="Total PM2.5 Emission", names.arg=c("1999", "2002", "2005", "2008"))
text(3.1,7200000, "Decrease in Total U.S. Emissions from 1999 to 2008", col = "red")

dev.off()

