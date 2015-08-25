###########################################################################

# QUESTION 3: Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, 
# a) which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# b) Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.



###########################################################################
library(dplyr)
library(ggplot2)

NEI <- readRDS("Data/summarySCC_PM25.rds")

# Check for any missing data
any(is.na(NEI$Emissions)) # returns TRUE if at least one NA found

# create a dataset of mean emissions for Baltimore, sum total emission for each year
# Baltimore City, Maryland (fips == "24510") 
pm.baltimore.type <- NEI %>% 
   filter(fips == "24510") %>% 
   select(Emissions,type, year) %>% 
   group_by(type, year) %>% 
   summarize(sum.pm = sum(Emissions)) 

# convert to plotable categorical data to factors (and years to ordered factors)
pm.baltimore.type$type <- as.factor(pm.baltimore.type$type)
pm.baltimore.type$year <- factor(pm.baltimore.type$year, levels = c("1999","2002","2005", "2008"), ordered = TRUE)

# NOTE: Column names CANNOT have spaces, or the qplot below fails (no idea why)
new.var.names <- colnames(pm.baltimore.type)
new.var.names[1] <- "EmissionSource"
new.var.names[2] <- "Year"
new.var.names[3] <- "PM2.5"
colnames(pm.baltimore.type) <- new.var.names

# make sure everything is OK
g <- qplot(Year,PM2.5, main = "Total PM2.5 Baltimore Emission Source Time Trend", data = pm.baltimore.type, group = EmissionSource, geom=c("point", "smooth"), method="lm", formula=y~x)
g <- g + geom_line(size = 1, aes(colour = EmissionSource)) + geom_point(size =5, pch = 21, fill = "salmon", alpha = .5)
g <- g + annotate("text", col = "red", x = 2.5, y = 2700, label = "All Baltimore sources have decreased over time")
g <- g + annotate("text", col = "red", x = 2.5, y = 2500, label = "except POINT (see blue trend lines)")
g

# write barplot to png file, in the same folder as this script
png(file="plot3.png",width=480,height=480)

g

dev.off()

