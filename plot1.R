load.data <- function(){
        setwd("/Users/North_Point/Dropbox/MOOC/Data_Science/Exploratory_Analysis/week1/New_Submission")
        data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
        data
}

clean.data <- function(){
        dat <- load.data()
        dat$date <- as.Date(dat$Date, "%d/%m/%Y")
        start_date = as.Date("2007-02-01", "%Y-%m-%d")
        end_date = as.Date("2007-02-02", "%Y-%m-%d")
        subdat <- subset(dat, date >= start_date & date <= end_date)
        subdat$time <- paste(subdat$Date, subdat$Time)
        subdat$time <- strptime(subdat$time, "%d/%m/%Y %H:%M:%S")
        subdat <- subset(subdat, select = -c(Date, Time, date))
        subdat        
}

plot.1 <- function(){
        dat <- clean.data()
        par(mfrow = c(1,1))
        with(dat, hist(Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)"))
        dev.copy(png, "plot1.png")
        dev.off()
}

plot.2 <- function(){
        dat <- clean.data()
        par(mfrow = c(1,1))
        with(dat, plot(time, Global_active_power, type = "l", ylab = "Global Active Power (killowatts)", xlab = " " ))
        dev.copy(png, "plot2.png")
        dev.off()
}

plot.3 <- function(){
        dat <- clean.data()
        par(mfrow = c(1,1))
        with(dat, plot(time, Sub_metering_1, type = "n", ylab = "Energy sub metering"))
        with(dat, lines(time, Sub_metering_1))
        with(dat, lines(time, Sub_metering_2, col = "red"))
        with(dat, lines(time, Sub_metering_3, col = "blue"))
        legend("topright", pch = "_", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        dev.copy(png, "plot3.png")
        dev.off()
}

plot.4 <- function(){
        dat <- clean.data()
        par(mfrow = c(2,2))
        with(dat, plot(time, Global_active_power, type = "l", ylab = "Global Active Power", xlab = " " ))
        with(dat, plot(time, Voltage, type = "l", ylab = "Voltage", xlab = "datetime" ))
        with(dat, plot(time, Sub_metering_1, type = "n", ylab = "Energy sub metering"))
        with(dat, lines(time, Sub_metering_1))
        with(dat, lines(time, Sub_metering_2, col = "red"))
        with(dat, lines(time, Sub_metering_3, col = "blue"))
        legend("topright", bty = "n", pch = "_", cex = 0.75, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        with(dat, plot(time, Global_reactive_power, type = "l", ylab = "Global_reactive_power", xlab = "datetime" ))
        dev.copy(png, "plot4.png")
        dev.off()
}

plot.1()


