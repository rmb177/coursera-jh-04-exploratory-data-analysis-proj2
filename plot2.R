# Bar plot to show the total emissions for
# Baltimore City, MD for the fours years in 
# the data. 

# The plot shows that overall emissions
# have decreased from 1999 to 2008, although
# there was an increase in emissions from 2002
# to 2005.


# This function assumes the summary data file
# is in the current working directory.
generatePlot <- function()
{
    # Read in data and work with the columns needed
    neiData <- readRDS("summarySCC_PM25.rds")
    pollutionData <- neiData[neiData$fips == "24510", c("year", "Emissions")]
    
    # Split by year and sum up totals
    years <- split(pollutionData$Emissions, pollutionData$year)
    emissions <- lapply(years, sum)
    
    png("plot2.png")
    
    myPlot <- barplot(unlist(emissions),
                      col="#daf0dd",
                      main=expression('Total Emissions of PM'[2.5] * ' in Baltimore City, MD'),
                      xlab="Year",
                      ylab="Tons")
    dev.off()
}

