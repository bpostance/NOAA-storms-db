

# Analysis of NOAA's Storm Events Database: Preparing for Severe Weather Events

## Synopsis
This document provides an analysis of U.S. National Oceanic and Atmospheric Administration's (NOAA) [Storm Events Database](http://www.ncdc.noaa.gov/stormevents/) in order to address the question of which major weather events are most harmful with respect to population health and/or have the greatest economic consequences. It's the result of the second assignment from the Coursera's Reproducible Research online course.

## Data files

The main data file used in this analysis is a 47Mb comma-separated-value file compressed via the bzip2 algorithm that can be downloaded from the course web site.
* Main data file: [repdata data StormData.csv.bz2](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

Additionally, we've used two more files that allowed us to 1) map inconsistent event type descriptions to the limited list of 48 event types, as defined in NOAA's National Weather Service (NWS) Directive 10-1605 and 2) adjust economic damages for inflation. These files are available at this GitHub [public repository](https://github.com/pjpjean/RepData_PeerAssessment2):
* [evtypes-directive-10-1605.csv](https://raw.githubusercontent.com/pjpjean/RepData_PeerAssessment2/master/evtypes-directive-10-1605.csv): a manually built mapping from the 985 different event types in the original data file to the NWS's 48-event types list, according to (my best judgment of) Directive 10-1605 guidelines.

* [bls-cpi-1950-2013.csv](https://raw.githubusercontent.com/pjpjean/RepData_PeerAssessment2/master/bls-cpi-1950-2013.csv): Average Consumer Price Index for all calendar years in the 1950-2013 period, adjusted relatively to 1982-84 (index=100). These indexes were download from the Bureau Labor Statistics (BLS) [website](http://data.bls.gov/pdq/SurveyOutputServlet), with the following parameters: **Series ID**, CUUR0000SA0; **Year range**: 1950-2013; **One Time period**: Annual Data; **Output format**: Text, comma delimited.


## Data processing
We begin by loading the main data file into R. Because it's a bzip2 compressed file,  we have to create a bzip2 file connection and pass it to `read.csv()` instead of just the filename. We split the reading in two parts in order to select just the columns we need for our analysis.



```r
# Read only the first line to get columns names (to be able to select just a
# few of them when reading the full dataset). Notice the bzip2 file
# connection we create and close just after using it.
filecon = bzfile("repdata-data-StormData.csv.bz2", "r")
col.names = colnames(read.csv(filecon, nrows = 1))
# If we don't close this connection now, next call to read.csv() will resume
# from where it stopped, but we don't want this.
close(filecon)

# define needed columns
cols.to.read = c("BGN_DATE", "COUNTY", "STATE", "EVTYPE", "FATALITIES", "INJURIES", 
    "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

# Read file from the beginning (new connection), keeping just the needed
# columns.
filecon = bzfile("repdata-data-StormData.csv.bz2", "r")
storm = read.csv(filecon, colClasses = c("NULL", NA)[(col.names %in% cols.to.read) + 
    1])
close(filecon)
```


Let's take a look at our data.

```r
str(storm)
```

```
## 'data.frame':	902297 obs. of  10 variables:
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```


#### Cleaning and tranforming the data
This data definitely needs some tidying. The more important things we'd want to change are:
* `BGN_DATE` as a factor: we think that a **date** type would be more appropriate, because it'll make it easier to aggregate and summarize on specific periods of time.


```r
storm$BGN_DATE = strptime(storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")
```


* `EVTYPE` as a 985-level factor: it seems that there is a lot of redudancy (or mistakes) in these event type descriptions. According to [NOAA's website](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype), from 1996 to present, only 48 event types are defined by NWS Directive 10-1605. We'd want to translate or map this gigantic list of event types into NWS's latest list.


```r
evtype <- read.csv("evtypes-directive-10-1605.csv", stringsAsFactors = FALSE)
storm$EVTYPE48 = factor(evtype$EVTYPE48[match(levels(storm$EVTYPE)[storm$EVTYPE], 
    evtype$EVTYPE)])
levels(storm$EVTYPE48)
```

```
##  [1] "Astronomical Low Tide"    "Avalanche"               
##  [3] "Blizzard"                 "Coastal Flood"           
##  [5] "Cold/Wind Chill"          "Debris Flow"             
##  [7] "Dense Fog"                "Dense Smoke"             
##  [9] "Drought"                  "Dust Devil"              
## [11] "Dust Storm"               "Excessive Heat"          
## [13] "Extreme Cold/Wind Chill"  "Flash Flood"             
## [15] "Flood"                    "Freezing Fog"            
## [17] "Frost/Freeze"             "Funnel Cloud"            
## [19] "Hail"                     "Heat"                    
## [21] "Heavy Rain"               "Heavy Snow"              
## [23] "High Surf"                "High Wind"               
## [25] "Hurricane (Typhoon)"      "Ice Storm"               
## [27] "Lake-Effect Snow"         "Lakeshore Flood"         
## [29] "Lightning"                "Marine Hail"             
## [31] "Marine High Wind"         "Marine Strong Wind"      
## [33] "Marine Thunderstorm Wind" "Rip Current"             
## [35] "Seiche"                   "Sleet"                   
## [37] "Storm Surge/Tide"         "Strong Wind"             
## [39] "Thunderstorm Wind"        "Tornado"                 
## [41] "Tropical Depression"      "Tropical Storm"          
## [43] "Tsunami"                  "Volcanic Ash"            
## [45] "Waterspout"               "Wildfire"                
## [47] "Winter Storm"             "Winter Weather"
```


* `PROPDMG` and `CROPDMG` with multiple scales: damage magnitudes are expressed in a separate variable (`PROPDMGEXP` and `CROPDMGEXP`, respectively). We should normalize them (to the same scale), otherwise it would not be straighforward to compare damage values in different events and dates.

We start by capitalizing all magnitude codes. It'll make the following steps easier.

```r
# capitalize all 1-letter magnitude codes
storm$PROPDMGEXP = toupper(storm$PROPDMGEXP)
storm$CROPDMGEXP = toupper(storm$CROPDMGEXP)
```


Before normalizing, we have to check whether there are invalid values in the magnitude variables.

```r
# check invalid codes in property damages
valid.mag = c("", "H", "K", "M", "B")
with(storm[!storm$PROPDMGEXP %in% valid.mag, ], table(factor(PROPDMGEXP), format(BGN_DATE, 
    "%Y")))
```

```
##    
##     1993 1994 1995 2011
##   -    0    0    1    0
##   ?    2    1    5    0
##   +    0    1    4    0
##   0    1   28  186    1
##   1    0    0   25    0
##   2    0    0   13    0
##   3    0    0    4    0
##   4    0    1    3    0
##   5    0    2   26    0
##   6    1    0    3    0
##   7    0    0    5    0
##   8    0    0    1    0
```

```r
# check invalid codes in crop damages
with(storm[!storm$CROPDMGEXP %in% valid.mag, ], table(factor(CROPDMGEXP), format(BGN_DATE, 
    "%Y")))
```

```
##    
##     1993 1994 1995
##   ?    2    0    5
##   0    2    9    8
##   2    0    0    1
```

We see that, apart from one observation in 2011, all invalid magnitude codes are from the 1993-1995 period when, according to [NOAA's website](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype), events were extracted from Unformatted Text files, a task known to be more prone to error. We'll leave them alone and consider all to be invalid. That means that their normalized value will be set to `NA`.

We're going to create two more variables to store the normalized values.

```r
magnitude = 10^c(0, 2, 3, 6, 9)
storm$nPROPDMG = storm$PROPDMG * magnitude[match(storm$PROPDMGEXP, valid.mag)]
storm$nCROPDMG = storm$CROPDMG * magnitude[match(storm$CROPDMGEXP, valid.mag)]
```


Now we need to adjust them for inflation, using BLS Consumer Price Index data, mentioned in the **Data files** section.

```r
# read CPI data
cpi = read.csv("bls-cpi-1950-2013.csv", skip = 13, colClasses = c("integer", 
    "numeric", "NULL"))
# get last index
last.cpi = tail(cpi, 1)$Annual
# calculate each row's inflation index
adj.index = last.cpi/cpi[match(storm$YEAR, cpi$Year), "Annual"]
storm$nPROPDMG = floor(storm$nPROPDMG * adj.index)
```

```
## Error: replacement has 0 rows, data has 902297
```

```r
storm$nCROPDMG = floor(storm$nCROPDMG * adj.index)
```

```
## Error: replacement has 0 rows, data has 902297
```


* `PROPDMG` and `CROPDMG` are not adjusted for inflation: 
## Results
