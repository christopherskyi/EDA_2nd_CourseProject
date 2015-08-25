###############################################################################################

## Coursera Exploratory Data Analysis Final Course Project
## Christopher Skyi
## Sunday, August 23, 2015, New York City, [86-74]F, 60% chance of thunderstorms by evening

###########################################################################

# QUESTION 4: Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999-2008?

###########################################################################
library(dplyr)
library(ggplot2)

# PM2.5 Emissions Data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Check for any missing data
any(is.na(NEI$Emissions)) # returns TRUE if at least one NA found

# Source Classification Code Table
SCC <- readRDS("Data/Source_Classification_Code.rds")

coal.scc <- as.character(SCC[grep("^fuel comb -(.*)- coal$", SCC$EI.Sector, ignore.case=T),1])

# A neat trick with dplyr and filter 
gettheserows <- NEI$SCC %in% coal.scc

# create a dataset of mean emissions for Baltimore, sum total emission for each year
coal.emissions <- NEI %>% 
   filter(SCC = gettheserows) %>% 
   select(Emissions, year) %>%
   group_by(year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# write barplot to png file, in the same folder as this script
png(file="plot4.png",width=480,height=480)

barplot(coal.emissions$sum.pm, xlab="Year",  ylim = c(0, 602100), main = "Total U.S. Coal Combustion Emission Time Trend (1999 - 2008)",
        ylab="Total PM2.5 Coal Combustion Emission", names.arg=c("1999", "2002", "2005", "2008"))
        text(2.6,590000, "Overall, a Coal Combustion Emissions Decrease from 1999 to 2008", col = "red")

dev.off()


