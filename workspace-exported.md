Climate Change Analysis
-----------------------

### Set Preliminaries

    library(ggplot2)
    data_loc="~/Projects/Data/Climate-Change-Analysis"

### Load in Data

    city_data <- read.csv(paste(data_loc, "/GlobalLandTemperaturesByCity.csv", sep = ""))
    city_data$dt <- as.Date(city_data$dt, "%Y-%m-%d")
    head(city_data)

    ##           dt AverageTemperature AverageTemperatureUncertainty  City Country
    ## 1 1743-11-01              6.068                         1.737 Århus Denmark
    ## 2 1743-12-01                 NA                            NA Århus Denmark
    ## 3 1744-01-01                 NA                            NA Århus Denmark
    ## 4 1744-02-01                 NA                            NA Århus Denmark
    ## 5 1744-03-01                 NA                            NA Århus Denmark
    ## 6 1744-04-01              5.788                         3.624 Århus Denmark
    ##   Latitude Longitude
    ## 1   57.05N    10.33E
    ## 2   57.05N    10.33E
    ## 3   57.05N    10.33E
    ## 4   57.05N    10.33E
    ## 5   57.05N    10.33E
    ## 6   57.05N    10.33E

### EDA

There are records for several thousand cities here, each with regular
monthly average temperature and uncertainty readings. It will be easier
to focus on one city to begin with.

    minsk_data_init <- city_data[city_data[,"City"]=="Minsk",]
    head(minsk_data_init)

    ##                 dt AverageTemperature AverageTemperatureUncertainty  City
    ## 4839603 1743-11-01              0.229                         1.878 Minsk
    ## 4839604 1743-12-01                 NA                            NA Minsk
    ## 4839605 1744-01-01                 NA                            NA Minsk
    ## 4839606 1744-02-01                 NA                            NA Minsk
    ## 4839607 1744-03-01                 NA                            NA Minsk
    ## 4839608 1744-04-01              7.358                         2.858 Minsk
    ##         Country Latitude Longitude
    ## 4839603 Belarus   53.84N    28.64E
    ## 4839604 Belarus   53.84N    28.64E
    ## 4839605 Belarus   53.84N    28.64E
    ## 4839606 Belarus   53.84N    28.64E
    ## 4839607 Belarus   53.84N    28.64E
    ## 4839608 Belarus   53.84N    28.64E

There are already visible entries with missing data

    minsk_data_missing = minsk_data_init[is.na(minsk_data_init[,"AverageTemperature"]) | is.na(minsk_data_init[,"AverageTemperatureUncertainty"]),]
    dim(minsk_data_missing)

    ## [1] 73  7

Luckily, however, only 73 entries contain missing temperature data. This
should be a small enough proportion of the entries to continue our
analysis without needing to deal with data cleaning.

    minsk_data = minsk_data_init[!(is.na(minsk_data_init[,"AverageTemperature"]) | is.na(minsk_data_init[,"AverageTemperatureUncertainty"])),]
    summary(minsk_data)

    ##        dt             AverageTemperature AverageTemperatureUncertainty
    ##  Min.   :1743-11-01   Min.   :-17.013    Min.   : 0.084               
    ##  1st Qu.:1815-10-08   1st Qu.: -2.728    1st Qu.: 0.334               
    ##  Median :1881-09-16   Median :  5.465    Median : 0.747               
    ##  Mean   :1881-09-04   Mean   :  5.404    Mean   : 1.509               
    ##  3rd Qu.:1947-08-24   3rd Qu.: 14.363    3rd Qu.: 2.235               
    ##  Max.   :2013-08-01   Max.   : 22.767    Max.   :11.078               
    ##                                                                       
    ##        City             Country        Latitude      Longitude   
    ##  Minsk   :3166   Belarus    :3166   53.84N :3166   28.64E :3166  
    ##  A Coruña:   0   Afghanistan:   0   0.80N  :   0   0.00W  :   0  
    ##  Aachen  :   0   Albania    :   0   0.80S  :   0   0.81E  :   0  
    ##  Aalborg :   0   Algeria    :   0   10.45N :   0   0.81W  :   0  
    ##  Aba     :   0   Angola     :   0   10.45S :   0   1.15E  :   0  
    ##  Abadan  :   0   Argentina  :   0   12.05N :   0   1.18E  :   0  
    ##  (Other) :   0   (Other)    :   0   (Other):   0   (Other):   0

We can create some plots to observe initial trends in the data

    dev.new()
    plot(minsk_data[, c("dt","AverageTemperature")],type="l",main="Average Temperature in Minsk")
    dev.new()
    plot(minsk_data[, c("dt","AverageTemperatureUncertainty")],type="l",main="Uncertainty of Average Temperature Reading in Minsk")

The average temperature reading data follows a regular variation, as
would be expected throughtout the year. An appropriate next step may be
to isolate readings for each month of the year or seasonally, in order
to better see trends across time.

The uncertainty data tells us that early readings for temperature are
wildly more uncertain than those taken in the last century or so.

    seasonal_months <- c("Jan", "Apr", "Jul", "Oct")
    minsk_data_snl <- list()
    for (mon in seasonal_months){
        minsk_data_snl[[mon]] <- minsk_data[format(minsk_data$dt, format="%b") == mon,]
    }

Plot seasonal data

    dev.new()
    seasonal_plot <- ggplot()
    colours = c("#000080","#d40202","#d9d400", "#008000")
    for (i in 1:4){
        seasonal_plot <- seasonal_plot + geom_line(data = minsk_data_snl[[i]], aes(x = dt, y = AverageTemperature), color = colours[i], alpha = 0.7)
    }
    seasonal_plot + theme_minimal()

There are no visible trends in the data, as yearly variance causes
interference. However, it is possible we can statistically test for a
significant increase over time, which would indicate provable evidence
of global warming.

I will first try a basic test, asking if the yearly difference sequence
has a mean greater than zero, which would prove increase over time. I
will be using a significance level of a=0.05
