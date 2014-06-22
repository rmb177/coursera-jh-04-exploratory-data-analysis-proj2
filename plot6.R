# Bar plot to show the total motor-vehicle
# source emissions for Baltimore City, MD vs.
# Los Angeles County, CA.

# Motor Vehicle sources were determined by searching
# the SCC.Level.Three field for items that contained the 
# string "Vehicles ". I assumed that the question was 
# concerned about on-road vehicles and not vehicles such
# as construction/mining equipemnt Investigation of the results 
# of this filtering showed that it provided the sources
# asked for in the problem.

# The plot shows that emissions in Los Angeles County, CA
# are much higher than in Baltimore City, MD. It also
# shows that Baltimore City, MD was able to reduce its
# emissions while Los Angeles County, CA had little
# or no reduction in its emissions.

library(ggplot2)

# This function assumes the summary data file
# is in the current working directory.
generatePlot <- function()
{
    # Read in both data files 
    neiData <- readRDS("summarySCC_PM25.rds")
    sccData <- readRDS("Source_Classification_Code.rds")

    # Pull out all motor vehicle related classifications from scc data and merge data
    vehicleClassifications <- sccData[grepl("Vehicles ", sccData[,c("SCC.Level.Three")]), ]
    cityData <- neiData[neiData$fips == "24510" | neiData$fips == "06037",]
    mergedData <- merge(cityData, vehicleClassifications)
    
    # Aggregate the data by year and type and change the year column to be a factor
    # to get distinct x-axis ticks.
    aggregateData = 
        aggregate(mergedData["Emissions"], by=mergedData[c("year","fips")], FUN=sum)
    aggregateData$year <- as.factor(aggregateData$year)
    aggregateData$fips[aggregateData$fips == "24510"] <- "Baltimore City, MD"
    aggregateData$fips[aggregateData$fips == "06037"] <- "Los Angeles County, CA"
    
    png("plot6.png", width=820, height=480)
    
    myPlot <- qplot(year, Emissions, data=aggregateData, 
                    facets = . ~ fips, 
                    geom="bar", 
                    stat="identity")
    
    # Add title and labels
    myPlot <- myPlot + labs(title = expression('Total Emissions of PM'[2.5] * ' from Motor Vehicle Related Sources Los Angeles County, CA vs. Baltimore City, MD')) +
        labs(x = "Year", y = "Tons")
    print(myPlot)
    dev.off()
}

