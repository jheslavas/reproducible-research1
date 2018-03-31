
## Plot 4: PNG figure: 4 Plots: Global active power per time; Energy Sub metering per time; Voltage per time; Global reactive power per time

## Data source contains: "Individual household electric power consumption Data Set” with "Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years"

## The instruction is: 
## 1. To read the database
## 2. To use data from the dates 2007-02-01 and 2007-02-02.
## 3. To take into account of missing values coded as ?.
## 4. To reconstruct the plots showed and save each one in a PNG file with a width of 480 pixels and a height of 480 pixels.
## 5. To publish at the repository the PNG file and the Script of codes used to draw and to save the file.


## Reading the file

energy <- read.table("./household_power_consumption.txt", header = TRUE, sep  = ";", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?")

## Subsetting the file

subset <- energy[energy$Date %in% c("1/2/2007","2/2/2007"),]

## Fixing Date and Time variables

DataTime <- strptime(paste(subset$Date, subset$Time, sep = " "),"%d/%m/%Y %H:%M:%S")
subset <- cbind(DataTime, subset)

## Plotting the histogram

par(mfcol = c(2,2))
par(mar = c(4,4,2,1), oma = c(0,0,1,0))

plot(subset$DataTime, subset$Global_active_power, type = "l", ylab = "Global Active Power", xlab = " ")

plot(subset$DataTime, subset$Sub_metering_1, type = "l", ylab = "Energy Sub metering", xlab = " ")
lines(subset$DataTime, subset$Sub_metering_2, col = "red")
lines(subset$DataTime, subset$Sub_metering_3, col = "blue")
legend("topright",  col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = "solid", lwd = 2, bty = "n")

plot(subset$DataTime, subset$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")

plot(subset$DataTime, subset$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")

## Copying the picture in other Device

dev.copy(png, file = "plot4.png", width = 480, height = 480)
dev.off()
