# This script uses the “Individual household electric power consumption Data Set”
# from the UC Irvine Machine Learning Repository.
# Data information:
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# Data set:
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# The data are loaded, subselected for a 2-day period, and formatted with datetimes.
# Multiple plots from the data are created and written to a PNG file.

# Data URL
dataUrl <- 'https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip'

loadZipData <- function(dataUrl) {
    # Get filename from URL
    urlParts <- strsplit(dataUrl, '/')[[1]]
    dataFile <- urlParts[length(urlParts)]
    # Download file
    download.file(dataUrl, dataFile, mode = 'curl')
    # Unzip file in temp directory
    tmpDir <- 'unzip_tmp'
    unzip(dataFile, exdir = tmpDir)
    # Get files in temp directory
    dataFiles <- list.files(tmpDir)
    dataFiles <- paste(tmpDir, dataFiles, sep = '/')
    # Read first file (there is only one file)
    data <- read.csv(dataFiles[[1]], sep = ';', na.strings = '?')
    # Remove temp directory
    unlink(tmpDir, recursive = TRUE)
    data
}

subselectData <- function(data) {
    # Subselect data for 2-day period
    selectDates <- c('1/2/2007', '2/2/2007')
    data <- subset(data, data$Date %in% selectDates)
}

formatData <- function(data) {
    # Create datetime variable from date and time variables
    data$DateTime <- as.POSIXlt(strptime(paste(data$Date, data$Time),
                                         "%d/%m/%Y %H:%M:%S"))
    # Remove date and time variables
    data <- subset(data, select = !(names(data) %in% c('Date', 'Time')))
}

plotGlobalActivePower <- function() {
    # Column to plot
    plotCol <- 'Global_active_power'
    # Suppress x-axis plotting
    par(xaxt = 'n')
    # Create plot without data
    plot(as.numeric(data$DateTime), data[[plotCol]],
         xlab = '',
         ylab = 'Global Active Power (kilowatts)',
         type = 'n')
    # Add lines
    lines(as.numeric(data$DateTime), data[[plotCol]])
    # Enable x-axis plotting
    par(xaxt = 's')
    # Create x-axis ticks/labels
    xTickDates <- c('1/2/2007', '2/2/2007', '3/2/2007')
    xTickDates <- as.POSIXlt(strptime(xTickDates, "%d/%m/%Y"))
    # Create x-axis
    axis(1, at = as.numeric(xTickDates),
         labels = weekdays(xTickDates, abbreviate = TRUE))
}

plotEnergySubMetering <- function() {
    # Columns to plot
    plotCols <- c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')
    # Suppress x-axis plotting
    par(xaxt = 'n')
    # Create plot without data
    plot(as.numeric(data$DateTime), data[[plotCols[1]]],
         xlab = '',
         ylab = 'Energy sub metering',
         type = 'n')
    # Add lines
    lineColors <- c('black', 'red', 'blue')
    for (i in 1:length(plotCols)) {
        lines(as.numeric(data$DateTime), data[[plotCols[i]]], col = lineColors[i])
    }
    # Add legend
    legend('topright', legend = plotCols, col = lineColors, lty = 1, bty = 'n')
    # Enable x-axis plotting
    par(xaxt = 's')
    # Create x-axis ticks/labels
    xTickDates <- c('1/2/2007', '2/2/2007', '3/2/2007')
    xTickDates <- as.POSIXlt(strptime(xTickDates, "%d/%m/%Y"))
    # Create x-axis
    axis(1, at = as.numeric(xTickDates),
         labels = weekdays(xTickDates, abbreviate = TRUE))
}

plotVoltage <- function() {
    # Column to plot
    plotCol <- 'Voltage'
    # Suppress x-axis plotting
    par(xaxt = 'n')
    # Create plot without data
    plot(as.numeric(data$DateTime), data[[plotCol]],
         xlab = 'datetime',
         ylab = plotCol,
         type = 'n')
    # Add lines
    lines(as.numeric(data$DateTime), data[[plotCol]])
    # Enable x-axis plotting
    par(xaxt = 's')
    # Create x-axis ticks/labels
    xTickDates <- c('1/2/2007', '2/2/2007', '3/2/2007')
    xTickDates <- as.POSIXlt(strptime(xTickDates, "%d/%m/%Y"))
    # Create x-axis
    axis(1, at = as.numeric(xTickDates),
         labels = weekdays(xTickDates, abbreviate = TRUE))
}

plotGlobalReactivePower <- function() {
    # Column to plot
    plotCol <- 'Global_reactive_power'
    # Suppress x-axis plotting
    par(xaxt = 'n')
    # Create plot without data
    plot(as.numeric(data$DateTime), data[[plotCol]],
         xlab = 'datetime',
         ylab = plotCol,
         type = 'n')
    # Add lines
    lines(as.numeric(data$DateTime), data[[plotCol]])
    # Enable x-axis plotting
    par(xaxt = 's')
    # Create x-axis ticks/labels
    xTickDates <- c('1/2/2007', '2/2/2007', '3/2/2007')
    xTickDates <- as.POSIXlt(strptime(xTickDates, "%d/%m/%Y"))
    # Create x-axis
    axis(1, at = as.numeric(xTickDates),
         labels = weekdays(xTickDates, abbreviate = TRUE))
}

# Load the data
data <- loadZipData(dataUrl)
# Subselect the data
data <- subselectData(data)
# Format the data
data <- formatData(data)

# Open PNG graphics device
png('plot4.png', width = 480, height = 480, units = 'px')

# Set graphics device to 2x2 array of plots
par(mfcol = c(2, 2))

# Draw plots
plotGlobalActivePower()
plotEnergySubMetering()
plotVoltage()
plotGlobalReactivePower()

# Close graphics device
dev.off()
