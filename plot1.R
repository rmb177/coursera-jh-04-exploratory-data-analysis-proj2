# Bar plot to show the total emissions for
# the fours years in the data. A trend
# line is included to explicitly show that
# emissions have decreated from 1999 to 2008.

# This function assumes the pollution data file
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
    
    df <- data.frame(
     matrix(c(emissions, as.numeric(names(years))), nrow=4))
    colnames(df) <- c("Emissions", "Year")
    #linearModel <- lm(Emissions ~ year, pollutionData)
    lines(myPlot, fitted(lm(df$Emissions ~ df$Year, data=df)), col="red")
    
    dev.off()
}

