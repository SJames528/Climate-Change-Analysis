                                        # ---------------
                                        # Set Preliminaries
                                        # ---------------

library(ggplot2)
data_loc="~/Projects/Data/Climate-Change-Analysis"




                                        # ---------------
                                        # Load in Data
                                        # ---------------


city_data <- read.csv(paste(data_loc, "/GlobalLandTemperaturesByCity.csv", sep = ""))
city_data$dt <- as.Date(city_data$dt, "%Y-%m-%d")
head(city_data)




                                        # ---------------
                                        # EDA
                                        # ---------------

## There are records for several thousand cities here, each with regular monthly average
## temperature and uncertainty readings. It will be easier to focus on one city to begin with.

minsk_data_init <- city_data[city_data[,"City"]=="Minsk",]
head(minsk_data_init)

## There are already visible entries with missing data

minsk_data_missing = minsk_data_init[is.na(minsk_data_init[,"AverageTemperature"]) | is.na(minsk_data_init[,"AverageTemperatureUncertainty"]),]
dim(minsk_data_missing)

## Luckily, however, only 73 entries contain missing temperature data. This should be
## a small enough proportion of the entries to continue our analysis without needing to
## deal with data cleaning.

minsk_data = minsk_data_init[!(is.na(minsk_data_init[,"AverageTemperature"]) | is.na(minsk_data_init[,"AverageTemperatureUncertainty"])),]
summary(minsk_data)

## We can create some plots to observe initial trends in the data

dev.new()
plot(minsk_data[, c("dt","AverageTemperature")],type="l",main="Average Temperature in Minsk")
dev.new()
plot(minsk_data[, c("dt","AverageTemperatureUncertainty")],type="l",main="Uncertainty of Average Temperature Reading in Minsk")

## The average temperature reading data follows a regular variation, as would be
## expected throughtout the year. An appropriate next step may be to isolate readings
## for each month of the year or seasonally, in order to better see trends across time.

## The uncertainty data tells us that early readings for temperature are wildly more
## uncertain than those taken in the last century or so.

seasonal_months <- c("Jan", "Apr", "Jul", "Oct")
minsk_data_snl <- list()
for (mon in seasonal_months){
    minsk_data_snl[[mon]] <- minsk_data[format(minsk_data$dt, format="%b") == mon,]
}

## Plot seasonal data

dev.new()
seasonal_plot <- ggplot()
colours = c("#000080","#d40202","#d9d400", "#008000")
for (i in 1:4){
    seasonal_plot <- seasonal_plot + geom_line(data = minsk_data_snl[[i]], aes(x = dt, y = AverageTemperature), color = colours[i], alpha = 0.7)
}
seasonal_plot + theme_minimal()

## There are no visible trends in the data, as yearly variance causes the data to be
## particularly sporadic. However, it is possible we can statistically test for a
## significant increase over time, which would indicate provable truth to global warming.

