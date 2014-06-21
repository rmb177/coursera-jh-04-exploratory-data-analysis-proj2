# Bar plot to show the total emissions for
# Baltimore City, MD for the fours years in 
# the data. A trend line is included to 
# explicitly show that emissions have 
# decreased from 1999 to 2008.

# This function assumes both data files are
# in the current working directory
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
                      main=expression('Baltimore City, MD: Total Emissions of PM'[2.5]),
                      xlab="Year",
                      ylab="Tons")
    
    df <- data.frame(
        matrix(c(as.numeric(emissions), as.numeric(names(years))), nrow=4))
    colnames(df) <- c("Emissions", "Year")
    linearModel <- lm(Emissions ~ year, pollutionData)
    lines(myPlot, fitted(lm(df$Emissions ~ df$Year, data=df)), col="red")
    
    dev.off()
}

