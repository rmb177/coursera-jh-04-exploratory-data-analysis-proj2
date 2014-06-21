# Bar plot to show the total emissions for
# the fours years in the data. 

# The plot shows that emissions have decreased
# from 1999 to 2008.


# This function assumes the summary data file
# is in the current working directory.
generatePlot <- function()
{
    # Read in data and work with the two columns needed
    neiData <- readRDS("summarySCC_PM25.rds")
    pollutionData <- neiData[, c("year", "Emissions")]
    
    # Split by year and sum up totals
    years <- split(pollutionData$Emissions, pollutionData$year)
    emissions <- lapply(years, sum)
    emissions <- unlist(emissions) / 1000
    
    png("plot1.png")
    
    myPlot <- barplot(emissions,
     col="#daf0dd",
     main=expression('Total Emissions of PM'[2.5]),
     xlab="Year",
     ylab="Thousands of Tons")
    
    dev.off()
}

