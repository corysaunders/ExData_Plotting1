##Examine how household energy usage varies over a 2-day period in February, 2007

##Load required libraries
library(dplyr)
library(data.table)

plot2 <- function() {
  ##Read data from file
  fh <- fread("~/coursera/datascience/Exploratory_data_analysis/household_power_consumption.txt",
              na.strings="?", stringsAsFactors=FALSE)
  ##Subset data to include only 2007/2/1 and 2007/2/2
  hpc <- filter(fh, grep("^[1,2]/2/2007", Date))
  
  ##Convert global active power column to numeric
  hpc$Global_active_power <- as.numeric(as.character(hpc$Global_active_power))

  ##Create new column combining date and time 
  hpc$Timestamp <-paste(hpc$Date, hpc$Time)
  
  ##Create graph of Global active power data by date/time
  plot(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Global_active_power,
       type="l", xlab="", ylab="Global Active Power(kilowatts)")
  
  ##Copy plot to PNG
  dev.copy(png, file="plot2.png")
  dev.off()
}
