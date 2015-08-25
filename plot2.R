###########################################################################

# QUESTION 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

###########################################################################
library(dplyr)

NEI <- readRDS("Data/summarySCC_PM25.rds")

# Check for any missing data
any(is.na(NEI$Emissions)) # returns TRUE if at least one NA found

# create a dataset of mean emissions for Baltimore (fips == "24510"), sum total emission for each year
pm.baltimore <- NEI %>% 
   filter(fips == "24510") %>% 
   group_by(year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# write barplot to png file, in the same folder as this script
png(file="plot2.png",width=480,height=480)
  barplot(pm.baltimore$sum.pm, xlab="Year",  ylim = c(0, 3500), main = "Total Baltimore PM2.5 Emission Time Trend (1999 - 2008)",
        ylab="Total PM2.5 Emission", names.arg=c("1999", "2002", "2005", "2008"))
  text(2.5,3400, "Overall, Total Emissions Decrease from 1999 to 2008", col = "red")
  
dev.off()

