# Bar plot to show the total PM25 emissions for
# Baltimore City, MD for the fours years in 
# the data. Emissions are broken down by the 
# type of emission (NON-ROAD, NONPOINT, ON-ROAD,
# and POINT).

# The plot shows that emissions from types 
# NON-ROAD, NONPOINT, and ON-ROAD have all
# decreased from 1999-2008. Emissions from
# type POINT have had an overall slight 
# increase although they have decreased
# from all-time highs in previous years.

library(ggplot2)

# This function assumes the summary data file
# is in the current working directory.
generatePlot <- function()
{
    # Read in data and work with the columns needed
    neiData <- readRDS("summarySCC_PM25.rds")
    pollutionData <- neiData[neiData$fips == "24510", c("year", "Emissions", "type")]
    
    # Aggregate the data by year and type and change the year column to be a factor
    # to get distinct x-axis ticks.
    aggregateData = 
     aggregate(pollutionData["Emissions"], by=pollutionData[c("year","type")], FUN=sum)
    aggregateData$year <- as.factor(aggregateData$year)
    
    png("plot3.png")    
    
    myPlot <- qplot(year, Emissions, data=aggregateData, 
     facets = . ~ type, 
     geom="bar", 
     stat="identity")
    
    # Add title and labels
    myPlot <- myPlot + labs(title = expression('Total Emissions of PM' [2.5] * ' by Type in Baltimore City, MD')) +
                       labs(x = "Year", y = "Tons")
    print(myPlot)
    dev.off()
}

