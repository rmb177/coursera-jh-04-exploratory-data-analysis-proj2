# Bar plot to show the total PM25 emissions for
# motor vehicle related sources.

# Motor Vehicle sources were determined by searching
# the SCC.Level.Three field for items that contained the 
# string "Vehicles ". I assumed that the question was 
# concerned about on-road vehicles and not vehicles such
# as construction/mining equipemnt Investigation of the results 
# of this filtering showed that it provided the sources
# asked for in the problem.

# The plot shows that overall emissions from motor-vehicle related
# sources decreased from 1999 to 2008.

# This function assumes both the summary data file
# and clasification file are in the current working directory.
generatePlot <- function()
{
    # Read in both data files 
    neiData <- readRDS("summarySCC_PM25.rds")
    sccData <- readRDS("Source_Classification_Code.rds")
    
    # Pull out all motor vehicle related classifications from scc data and merge data
    vehicleClassifications <- sccData[grepl("Vehicles ", sccData[,c("SCC.Level.Three")]), ]
    baltimoreData <- neiData[neiData$fips == "24510",]
    
    mergedData <- merge(baltimoreData, vehicleClassifications)
    
    # Split by year and sum up totals
    years <- split(mergedData$Emissions, mergedData$year)
    emissions <- lapply(years, sum)
    emissions <- unlist(emissions)
    
    png("plot5.png", width=620, height=480)
    
    myPlot <- barplot(emissions,
     col="#daf0dd",
     main=expression('Total Emissions of PM'[2.5] * ' from Motor Vehicle Related Sources in Baltimore City, MD'),
     xlab="Year",
     ylab="Tons")
    
    dev.off()
}

