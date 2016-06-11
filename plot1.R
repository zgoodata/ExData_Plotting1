# This script uses the “Individual household electric power consumption Data Set”
# from the UC Irvine Machine Learning Repository.
# Data information:
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# Data set:
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# The data are loaded, subselected for a 2-day period, and formatted with datetimes.
# A simple plot from the data is created and written to a PNG file.

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

# Load the data
data <- loadZipData(dataUrl)
# Subselect the data
data <- subselectData(data)
# Format the data
data <- formatData(data)

# Open PNG graphics device
png('plot1.png', width = 480, height = 480, units = 'px')

# Set background to transparent
par(bg = NA)

# Create histogram plot
hist(data$Global_active_power,
     col = 'red',
     main = 'Global Active Power',
     xlab = 'Global Active Power (kilowatts)')

# Close graphics device
dev.off()
