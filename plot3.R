# Bar plot to show the total emissions for
# Baltimore City, MD for the fours years in 
# the data. Emissions are broken down by the 
# type of emission (NON-ROAD, NONPOINT, ON-ROAD,
# and POIT). A trend line is included to 
# explicitly show whether emissions have increased
# or decreased.

library(ggplot2)

# This function assumes both data files are
# in the current working directory
generatePlot <- function()
{
    # Read in data and work with the columns needed
    neiData <- readRDS("summarySCC_PM25.rds")
    pollutionData <- neiData[neiData$fips == "24510", c("year", "Emissions", "type")]
    
    aggregateData = 
     aggregate(pollutionData["Emissions"], by=pollutionData[c("year","type")], FUN=sum)
    
    aggregateData$year <- as.factor(aggregateData$year)
    
    png("plot3.png")    
    myPlot <- qplot(year, Emissions, data=aggregateData, facets = .  ~ type, geom="bar", stat="identity")
    print(myPlot)
    dev.off()
}

