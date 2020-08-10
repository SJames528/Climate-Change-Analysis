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

print("Missing Dates:")
minsk_data_missing$dt

## We can see here that the missing data is complete from 1753 onwards, with the exception of
## one month in 2013

minsk_data = minsk_data_init[format(minsk_data_init$dt, format="%Y")>=1753,]
minsk_data = minsk_data[!(is.na(minsk_data[,"AverageTemperature"]) | is.na(minsk_data[,"AverageTemperatureUncertainty"])),]
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

## There are no visible trends in the data, as yearly variance causes interference.
## However, it is possible we can statistically test for a significant increase over
## time, which would indicate provable evidence of global warming.

                                        # ---------------
                                        # Statistical Testing
                                        # ---------------

## I will first try a basic test, asking if the yearly difference sequence has a mean
## greater than zero, which would prove increase over time. I will be using a significance
## level of a=0.05. I will try this using each month in turn, thus testing 12 times. I will
## display my working for January, and extend this to the others.

## I want to define an appropriate test for each monthly dataset. There are two
## ways I could do this: 1) By converting the absolute temperature data into yearly differences,
## and testing if overall these have a significantly positive mean; 2) By defining a suitable
## year range (e.g. 1750-1900) as a "control" period, and testing if recent measurements
## do not fit the control distribution.

## The first option will be slightly easier to perform, so I will pursue that one initially.

## First to convert the absolute data into differences:

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
minsk_data_monthly <- list()
minsk_data_month_diff <- list()
for (mon in months){
    minsk_data_monthly[[mon]] <- minsk_data[format(minsk_data$dt, format="%b") == mon,]
}


## *!* NOW FIXED *!*

for (mon in months){
  minsk_data_month_diff[[mon]] <- data.frame(minsk_data_monthly[[mon]]$dt[-1])
  names(minsk_data_month_diff[[mon]]) = c("Date")
  minsk_data_month_diff[[mon]]["Difference"] <- minsk_data_monthly[[mon]]$AverageTemperature[-1] - minsk_data_monthly[[mon]]$AverageTemperature[-(dim(minsk_data_monthly[[mon]])[1])]
  minsk_data_month_diff[[mon]]["DifferenceUncertainty"] <- minsk_data_monthly[[mon]]$AverageTemperatureUncertainty[-1] + minsk_data_monthly[[mon]]$AverageTemperatureUncertainty[-(dim(minsk_data_monthly[[mon]])[1])]
}

## January differences plot

dev.new()
ggplot() + geom_point(data = minsk_data_month_diff[["Jan"]], aes(x = Date, y = Difference), alpha = 0.7) + ggtitle("Year-to-Year changes in January temperature in Minsk") + xlab("Year") + ylab("Temperature Difference (Celcius)")
dev.new()
ggplot() + geom_line(data = minsk_data_month_diff[["Jan"]], aes(x = Date, y = DifferenceUncertainty), alpha = 0.7)

## We need to bear uncertainty in mind, since the error in this data can be quite significant. By taking a difference between two measurements, we add together the uncertainty from both. I've plotted a section of the graph with uncertainty included:

minsk_jan_reduced=minsk_data_month_diff[["Jan"]][c(50:100),]
dev.new()
ggplot(minsk_jan_reduced) + geom_point(aes(x = Date, y = Difference))  + geom_errorbar(aes(x = Date, y = Difference, ymin=Difference-DifferenceUncertainty, ymax=Difference+DifferenceUncertainty))

## These error bounds are quite significant, particularly for the earlier readings, and will definitely cast doubt on any conclusions we make.

##
##

## Now that we have the difference sequence, I will divide by the empirical standard deviation and test the following hypotheses:

## Let X_1,...,X_n be the standardised series of differences
## H_0: X_1,...,X_n ~ i.i.d normal(0,1)
## H_1: X_1,...,X_n ~ i.i.d normal(u,1), u>0

for (mon in months){
    minsk_data_month_diff[[mon]]["StandardisedDifference"] = minsk_data_month_diff[[mon]]$Difference / sd(minsk_data_month_diff[[mon]]$Difference)
}

## We want our test statistic here to be the most powerful, which by the Neyman-Pearson Lemma is simply the mean of the difference sequence. To see a proof of this, please see Section A in the appendix. I will gather a p-value for each month of the year, to gain 12 scores in total.

for (mon in months){
    min
}
