##Examine how household energy usage varies over a 2-day period in February, 2007

##Load required libraries
library(dplyr)
library(data.table)

plot4 <- function() {
  ##Read data from file
  fh <- fread("~/coursera/datascience/Exploratory_data_analysis/household_power_consumption.txt",
              na.strings="?", stringsAsFactors=FALSE)
  ##Subset data to include only 2007/2/1 and 2007/2/2
  hpc <- filter(fh, grep("^[1,2]/2/2007", Date))
  
  ##Convert global active power column to numeric
  hpc$Global_active_power <- as.numeric(as.character(hpc$Global_active_power))
  
  ##Convert sub metering columns to numeric
  hpc$Sub_metering_1 <- as.numeric(as.character(hpc$Sub_metering_1))
  hpc$Sub_metering_2 <- as.numeric(as.character(hpc$Sub_metering_2))
  hpc$Sub_metering_3 <- as.numeric(as.character(hpc$Sub_metering_3))
  
  ##Convert global reactive power column to numeric
  hpc$Global_reactive_power <- as.numeric(as.character(hpc$Global_reactive_power))
  ##Convert voltage column to numeric
  hpc$Voltage <- as.numeric(as.character(hpc$Voltage))
  
  ##Create new column combining date and time 
  hpc$Timestamp <-paste(hpc$Date, hpc$Time)
  
  ##Create 4 graphs in a 2x2 grid, by column
  save_par <- par(mfcol = c(2,2))

  ##Create graph of Global active power data by date/time -- 1st col, 1st row
  plot(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Global_active_power,
       type="l", xlab="", ylab="Global Active Power")
  
  ##Create plot of Sub metering 1 data by date/time -- 1st col, 2nd row
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
  
  ##Create plot of Voltage by date/time -- 2nd col, 1st row
  plot(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Voltage,
       type="l", xlab="datetime", ylab="Voltage")
  
  ##Create plot of Global reactive power by date/time -- 2nd col, 2nd row
  plot(strptime(hpc$Timestamp, "%d/%m/%Y %H:%M:%S"), hpc$Global_reactive_power,
       type="l", xlab="datetime", ylab="Global_reactive_power")

  ##Reset par() to previous settings
  par(save_par)
  
  ##Copy plots to PNG
  dev.copy(png, file="plot4.png")
  dev.off()
}
