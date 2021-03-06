

# Analysing NOAA's Storm Events Database to Prepare for Severe Weather Events

## Synopsis
This document provides an analysis of U.S. National Oceanic and Atmospheric Administration's (NOAA) [Storm Events Database](http://www.ncdc.noaa.gov/stormevents/) in order to address the question of which major weather events are most harmful with respect to population health and/or have the greatest economic consequences. Based on it, a government or municipal manager might be prepared for severe weather events and prioritize resources for different types
of events. It's part of the second assignment from the Coursera's Reproducible Research online course.

## Data files

The main data file used in this analysis is a 47Mb comma-separated-value file compressed via the bzip2 algorithm that can be downloaded from the course web site.
* Main data file: [repdata data StormData.csv.bz2](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

Additionally, we've used two more files that allowed us to 1) map inconsistent event type descriptions to the limited list of 48 event types, as defined in NOAA's National Weather Service (NWS) Directive 10-1605 and 2) adjust economic damages for inflation. These files are available at my GitHub [public repository](https://github.com/pjpjean/RepData_PeerAssessment2):
* [evtypes-directive-10-1605.csv](https://raw.githubusercontent.com/pjpjean/RepData_PeerAssessment2/master/evtypes-directive-10-1605.csv): a manually built mapping from the 985 different event types in the original data file to the NWS's 48-event types list, according to (my best judgment of) Directive 10-1605 guidelines.

* [bls-cpi-1950-2013.csv](https://raw.githubusercontent.com/pjpjean/RepData_PeerAssessment2/master/bls-cpi-1950-2013.csv): Average Consumer Price Index for all calendar years in the 1950-2013 period, adjusted relatively to 1982-84 (index=100). These indexes were downloaded from the Bureau Labor Statistics (BLS) [website](http://data.bls.gov/pdq/SurveyOutputServlet), with the following parameters: **Series ID**, CUUR0000SA0; **Year range**: 1950-2013; **One Time period**: Annual Data; **Output format**: Text, comma delimited.


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

It seems that, apart from one observation in 2011, all invalid magnitude codes are from the 1993-1995 period when, according to [NOAA's website](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype), events were extracted from Unformatted Text files, a task known to be more prone to error. We'll leave them alone and consider all to be invalid. That means that their normalized value will be set to `NA`.

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
adj.index = last.cpi/cpi[match(storm$BGN_DATE$year + 1900, cpi$Year), "Annual"]
# apply them
storm$nPROPDMG = floor(storm$nPROPDMG * adj.index)
storm$nCROPDMG = floor(storm$nCROPDMG * adj.index)
```


One last step would be to generate an aggregate dataset, with annual totals. That will be the dataset used in our analysis.

```r
storm.agg = aggregate(cbind(FATALITIES, INJURIES, nPROPDMG, nCROPDMG) ~ BGN_DATE$year + 
    EVTYPE48 + STATE, data = storm, sum)
names(storm.agg)[1] = "YEAR"
storm.agg$YEAR = storm.agg$YEAR + 1900
```

## Analysis
As mentioned in the NOAA's website, until 1992, only tornado, thunderstorm wind and hail events found their way into the database. Let's see what we have in our dataset.

```r
pre1993 = aggregate(cbind(FATALITIES, INJURIES, nPROPDMG, nCROPDMG) ~ EVTYPE48, 
    data = storm.agg[storm.agg$YEAR < 1993, ], sum)
pre1993
```

```
##            EVTYPE48 FATALITIES INJURIES  nPROPDMG nCROPDMG
## 1              Hail          5      401 0.000e+00        0
## 2 Thunderstorm Wind        263     3326 0.000e+00        0
## 3           Tornado       4012    68036 1.147e+11        0
```


As we can see, until 1992, property damages related to 'Tornado' events amount to   $114.7 billion. This is more than 19% of total property damages of all event types in the whole period. Tornado pre-1993 data also represents 26.5% of all fatalities and 48% of all injuries, as we show below.

```r
metrics = c("FATALITIES", "INJURIES", "nPROPDMG", "nCROPDMG")
pre1993[pre1993$EVTYPE48 == "Tornado", metrics]/colSums(storm.agg[metrics])
```

```
##   FATALITIES INJURIES nPROPDMG nCROPDMG
## 3     0.2652   0.4844   0.1909        0
```


Any attempt of making an unbiased analysis should leave pre-1993 data out and that's what we're going to do.


```r
storm.agg.93 = subset(storm.agg, YEAR >= 1993)
```


### Events harmful to population health
Now, we start to analyze the type of events most harmful to population health, according to 1993 and forward storm data.

```r
harmful = aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE48, data = storm.agg.93, 
    sum)
# ordering by FATALITIES and INJURIES
most.harmful.5 = lapply(c("FATALITIES", "INJURIES"), function(n) harmful[order(harmful[n], 
    decreasing = TRUE)[1:5], c("EVTYPE48", n)])
most.harmful.5
```

```
## [[1]]
##          EVTYPE48 FATALITIES
## 12 Excessive Heat       2016
## 40        Tornado       1643
## 20           Heat       1161
## 14    Flash Flood       1065
## 29      Lightning        817
## 
## [[2]]
##             EVTYPE48 INJURIES
## 40           Tornado    23303
## 15             Flood     6794
## 12    Excessive Heat     6680
## 39 Thunderstorm Wind     6231
## 29         Lightning     5232
```


We can see that, in the period including and after 1993, 'Tornado' was the event that caused more injuries, as most of us would pehaps expect.

What came as a surprise is that 'Excessive Heat' caused more fatalities in the same period (followed by 'Tornado' events). If we add to this number those 'Heat'-caused fatalities (3rd line of first list item), we'll have more than 3,100 heat-related fatalities, almost twice as much as 'Tornado' fatalities.

Let's investigate this a little further.


```r
heat.related = storm.agg.93$EVTYPE48 == "Excessive Heat" | storm.agg.93$EVTYPE48 == 
    "Heat"

library(ggplot2)
library(grid)
ggplot(storm.agg.93[heat.related, ], aes(x = as.factor(YEAR), y = FATALITIES)) + 
    geom_bar(stat = "identity", fill = "darkolivegreen3") + labs(title = "Heat-related fatalities (1993-2011)", 
    x = "Year", y = expression("Fatalities")) + guides(fill = FALSE) + theme_minimal() + 
    theme(plot.title = element_text(size = 18, face = "bold"), panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())
```

![plot of chunk heatrelated](figure/heatrelated.png) 


Look at the barplot above. Why is the number of 1995 heat-related fatalities so high? Let's check which states had more fatalities that year.


```r
states.heat = aggregate(FATALITIES ~ STATE, data = subset(storm.agg.93, heat.related & 
    YEAR == 1995), sum)
states.heat = states.heat[order(states.heat$FATALITIES, decreasing = TRUE), 
    ]
head(states.heat, 5)
```

```
##    STATE FATALITIES
## 12    IL        626
## 29    PA        198
## 38    WI         67
## 21    MO         34
## 24    NJ         20
```

We can see that Illinois was the state with the greatest number of heat-related fatalities in 1995. Searching on the internet, we find that it corresponds to the so-called "1995 Chicago heat wave". According to [Wikipedia](http://en.wikipedia.org/wiki/1995_Chicago_heat_wave), this heat wave "led to approximately 750 heat-related deaths in Chicago over a period of five days. Most of the victims of the heatwave were elderly poor residents of the inner city, who could not afford air conditioning and did not open windows or sleep outside for fear of crime."

Getting back to our list, we'll have flood-like events, thunderstorm winds and lightning events, besides heat and tornadoes, as the most harmful events to population health.


```r
most.harmful.5
```

```
## [[1]]
##          EVTYPE48 FATALITIES
## 12 Excessive Heat       2016
## 40        Tornado       1643
## 20           Heat       1161
## 14    Flash Flood       1065
## 29      Lightning        817
## 
## [[2]]
##             EVTYPE48 INJURIES
## 40           Tornado    23303
## 15             Flood     6794
## 12    Excessive Heat     6680
## 39 Thunderstorm Wind     6231
## 29         Lightning     5232
```


### Events with the greatest economic consequences
Applying a similar approach to find the events that caused the most damages to property and crop in the 1993-2011 period, we'll find these results.


```r
econ.cons = aggregate(cbind(nPROPDMG, nCROPDMG) ~ EVTYPE48, data = storm.agg.93, 
    sum)
# ordering by Property and Crop Damages (inflation adjusted)
econ.cons.5 = lapply(c("nPROPDMG", "nCROPDMG"), function(n) econ.cons[order(econ.cons[n], 
    decreasing = TRUE)[1:5], c("EVTYPE48", n)])
econ.cons.5
```

```
## [[1]]
##               EVTYPE48  nPROPDMG
## 15               Flood 1.780e+11
## 25 Hurricane (Typhoon) 1.058e+11
## 37    Storm Surge/Tide 5.678e+10
## 40             Tornado 3.417e+10
## 14         Flash Flood 2.149e+10
## 
## [[2]]
##               EVTYPE48  nCROPDMG
## 9              Drought 1.841e+10
## 15               Flood 1.534e+10
## 26           Ice Storm 7.891e+09
## 25 Hurricane (Typhoon) 7.186e+09
## 19                Hail 3.959e+09
```


'Flood' and 'Drought' come up as the most dangerous weather events to properties and crop, respectively, with total damages worth around $178 billion in the former's case and arount $18.4 in the latter's, inflation adjusted. 'Flood' have its evil share of $15.3 billion in crop damages too.

Alongside them, concerning property damages, we have 'Hurricane (Typhoon)' ($105 billion), 'Storm Surge/Tide' ($56 billion), 'Tornado' ($34 billion) and 'Flash Flood' ($21 billion); crop damages around $7.8 billion caused by 'Ice Storm', $7.1 billion caused again by 'Hurricane (Typhoon)' and $3.9 billion by 'Hail' complete this list of the most economically harmful weather events.

Lookint at the plot of property damage caused by top-5 events, we can't help but notice some prominent points.

```r
propdmg.top5 = as.character(econ.cons.5[[1]]$EVTYPE48)
propdmg.agg = aggregate(nPROPDMG ~ EVTYPE48 + YEAR, data = subset(storm.agg.93, 
    EVTYPE48 %in% propdmg.top5), sum)
ggplot(propdmg.agg, aes(x = as.factor(YEAR), y = nPROPDMG/10^9, group = EVTYPE48, 
    color = EVTYPE48)) + geom_line(size = 1) + geom_point(size = 4) + labs(title = "Property damage - 5 most destructive event types", 
    x = "Year", y = "Property damages ($ billion)", color = "Event type") + 
    guides(fill = FALSE) + theme_minimal() + theme(plot.title = element_text(size = 16, 
    face = "bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
    legend.position = c(0, 0.95), legend.justification = c(0, 1), legend.direction = "vertical", 
    legend.key.height = unit(5, "mm"))
```

![plot of chunk plot_propdmg](figure/plot_propdmg.png) 

The spikes in year 2005 are most likely to represent the economic losses of Hurricane Katrina, supposedly the [costliest natural disaster](http://en.wikipedia.org/wiki/Hurricane_Katrina) in the US history. So, it seems odd that the spike in 2006 is higher than that. We decided to investigate this.

Looking at the top states affected by flood in 2006, we get that California has the higher property damage annual total.

```r
states.flood = aggregate(nPROPDMG ~ STATE, data = subset(storm.agg.93, EVTYPE48 == 
    "Flood" & YEAR == 2006), sum)
states.flood = states.flood[order(states.flood$nPROPDMG, decreasing = TRUE), 
    ]
head(states.flood, 5)
```

```
##    STATE  nPROPDMG
## 5     CA 1.332e+11
## 18    LA 7.157e+08
## 34    NY 1.840e+08
## 38    PA 1.609e+08
## 1     AK 7.074e+07
```


Looking into the (almost) raw data, we see this suspect event, related to the period of storms and flooding in California between December 2005 and January 2006.

```r
ca.flood.2006 = subset(storm, STATE == "CA" & EVTYPE48 == "Flood" & BGN_DATE$year + 
    1900 == 2006)
ca.flood.2006[which.max(ca.flood.2006$nPROPDMG), ]
```

```
##          BGN_DATE COUNTY STATE EVTYPE FATALITIES INJURIES PROPDMG
## 605953 2006-01-01     55    CA  FLOOD          0        0     115
##        PROPDMGEXP CROPDMG CROPDMGEXP EVTYPE48  nPROPDMG nCROPDMG
## 605953          B    32.5          M    Flood 1.329e+11 37555071
```


We went to the online NOAA Storm Events Database to look at the [original record](http://www.ncdc.noaa.gov/stormevents/eventdetails.jsp?id=5486532). Yes, property damage is coded as $115 billion, but in the Episode Narrative field, we clearly see that it is likely to be an error.

That's the Episode Narrative: "Major flooding continued into the early hours of January 1st, before the Napa River finally fell below flood stage and the water receeded. Flooding was severe in Downtown Napa from the Napa Creek and the City and Parks Department was hit with $6 million in damage alone. The City of Napa had 600 homes with moderate damage, 150 damaged businesses with costs of at least $70 million."

So, we're pretty sure that this astounding value should be around $100 millions (not billions). Now, let's fix this error.


```r
diff.nPROPDMG = 0.999 * ca.flood.2006[which.max(ca.flood.2006$nPROPDMG), ]$nPROPDMG
wrong.obs = with(storm, STATE == "CA" & EVTYPE48 == "Flood" & BGN_DATE$year + 
    1900 == 2006)
storm[wrong.obs, ]$nPROPDMG = storm[wrong.obs, ]$nPROPDMG - diff.nPROPDMG

wrong.obs = with(storm.agg, STATE == "CA" & EVTYPE48 == "Flood" & YEAR == 2006)
storm.agg[wrong.obs, ]$nPROPDMG = storm.agg[wrong.obs, ]$nPROPDMG - diff.nPROPDMG

wrong.obs = with(storm.agg.93, STATE == "CA" & EVTYPE48 == "Flood" & YEAR == 
    2006)
storm.agg.93[wrong.obs, ]$nPROPDMG = storm.agg.93[wrong.obs, ]$nPROPDMG - diff.nPROPDMG
```


Time to redo the economic losses analysis.

```r
econ.cons = aggregate(cbind(nPROPDMG, nCROPDMG) ~ EVTYPE48, data = storm.agg.93, 
    sum)
# ordering by FATALITIES and INJURIES
econ.cons.5 = lapply(c("nPROPDMG", "nCROPDMG"), function(n) econ.cons[order(econ.cons[n], 
    decreasing = TRUE)[1:5], c("EVTYPE48", n)])
econ.cons.5
```

```
## [[1]]
##               EVTYPE48  nPROPDMG
## 25 Hurricane (Typhoon) 1.058e+11
## 37    Storm Surge/Tide 5.678e+10
## 15               Flood 4.526e+10
## 40             Tornado 3.417e+10
## 14         Flash Flood 2.149e+10
## 
## [[2]]
##               EVTYPE48  nCROPDMG
## 9              Drought 1.841e+10
## 15               Flood 1.534e+10
## 26           Ice Storm 7.891e+09
## 25 Hurricane (Typhoon) 7.186e+09
## 19                Hail 3.959e+09
```

This time, the first positions in the properties and crop damage lists are occupied by 'Hurricane (Typhoon)' and 'Drought'. Let's see how has our plot changed. Has Hurricane Katrina claimed its dreadful spot?


```r
propdmg.top5 = as.character(econ.cons.5[[1]]$EVTYPE48)
propdmg.agg = aggregate(nPROPDMG ~ EVTYPE48 + YEAR, data = subset(storm.agg.93, 
    EVTYPE48 %in% propdmg.top5), sum)
ggplot(propdmg.agg, aes(x = as.factor(YEAR), y = nPROPDMG/10^9, group = EVTYPE48, 
    color = EVTYPE48)) + geom_line(size = 1) + geom_point(size = 4) + labs(title = expression(atop("Property damage - 5 most destructive event types", 
    scriptstyle("After fixing CA flood error"))), x = "Year", y = "Property damages ($ billion)", 
    color = "Event type") + guides(fill = FALSE) + theme_minimal() + theme(plot.title = element_text(size = 16, 
    face = "bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
    legend.position = c(0, 0.95), legend.justification = c(0, 1), legend.direction = "vertical", 
    legend.key.height = unit(5, "mm"))
```

![plot of chunk plot_propdmg2](figure/plot_propdmg2.png) 


## Results

In our analysis, we found that the most harmful weather events to population health were heat-related events ('Excessive Heat' and 'Heat'), along with tornadoes, flood-like events, thunderstorm winds and lightning events.


```r
most.harmful.5
```

```
## [[1]]
##          EVTYPE48 FATALITIES
## 12 Excessive Heat       2016
## 40        Tornado       1643
## 20           Heat       1161
## 14    Flash Flood       1065
## 29      Lightning        817
## 
## [[2]]
##             EVTYPE48 INJURIES
## 40           Tornado    23303
## 15             Flood     6794
## 12    Excessive Heat     6680
## 39 Thunderstorm Wind     6231
## 29         Lightning     5232
```


Concerning property economic losses, the most destructive events were hurricanes, storm surges/tides, flood (including flash floods) and tornadoes; in the case of crop losses, drought, ice storm and hail, besides hurricanes and flood, are the most important ones.


```r
econ.cons.5
```

```
## [[1]]
##               EVTYPE48  nPROPDMG
## 25 Hurricane (Typhoon) 1.058e+11
## 37    Storm Surge/Tide 5.678e+10
## 15               Flood 4.526e+10
## 40             Tornado 3.417e+10
## 14         Flash Flood 2.149e+10
## 
## [[2]]
##               EVTYPE48  nCROPDMG
## 9              Drought 1.841e+10
## 15               Flood 1.534e+10
## 26           Ice Storm 7.891e+09
## 25 Hurricane (Typhoon) 7.186e+09
## 19                Hail 3.959e+09
```

