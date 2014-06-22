# Bar plot to show the total emissions for
# coal-combustion related sources.

# Coal-combustion sources were determined by searching
# the EI.Sector field for items that contained the 
# string "- Coal". Investigation of the results of
# this filtering showed that it provided the sources
# asked for in the problem.

# This function assumes both the summary data file
# and clasification file are in the current working directory.
generatePlot <- function()
{
    # Read in both data files 
    neiData <- readRDS("summarySCC_PM25.rds")
    sccData <- readRDS("Source_Classification_Code.rds")
    
    # Pull out all coal combustion related classifications from scc data and merge data
    coalClassifications <- sccData[grepl("- Coal", sccData[,c("EI.Sector")]), ]
    mergedData <- merge(neiData, coalClassifications)
    
    # Split by year and sum up totals
    years <- split(mergedData$Emissions, mergedData$year)
    emissions <- lapply(years, sum)
    emissions <- unlist(emissions) / 1000
    
    png("plot4.png")
    
    myPlot <- barplot(emissions,
     col="#daf0dd",
     main=expression('Total Emissions of PM'[2.5] * ' from Coal Combustion-Related Sources'),
     xlab="Year",
     ylab="Thousands of Tons")
    
    dev.off()
}

