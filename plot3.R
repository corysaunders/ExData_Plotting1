##Examine how household energy usage varies over a 2-day period in February, 2007

##Load required libraries
library(dplyr)
library(data.table)

plot3 <- function() {
  ##Read data from file
  fh <- fread("~/coursera/datascience/Exploratory_data_analysis/household_power_consumption.txt",
              na.strings="?", stringsAsFactors=FALSE)
  ##Subset data to include only 2007/2/1 and 2007/2/2
  hpc <- filter(fh, grep("^[1,2]/2/2007", Date))
  
  ##Convert sub metering columns to numeric
  hpc$Sub_metering_1 <- as.numeric(as.character(hpc$Sub_metering_1))
  hpc$Sub_metering_2 <- as.numeric(as.character(hpc$Sub_metering_2))
  hpc$Sub_metering_3 <- as.numeric(as.character(hpc$Sub_metering_3))
  
  ##Create new column combining date and time 
  hpc$Timestamp <-paste(hpc$Date, hpc$Time)
  
  ##Create plot of Sub metering 1 data by date/time
  plot(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Sub_metering_1,
       type="l", xlab="", ylab="Energy sub metering")
  
  ##Add line graph of Sub metering 2 data by date/time in red
  lines(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Sub_metering_2,
        type="l", col="red" )
  
  ##Add line graph of Sub metering 3 data by date/time in blue
  lines(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Sub_metering_3,
        type="l", col="blue" )
  
  ##Add legend to graph
  legend("topright", lty=1, col=c("black", "red", "blue"),
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  ##Copy plot to PNG
  dev.copy(png, file="plot3.png")
  dev.off()
}
