Analysis of Natural Disasters to Human Casualties and Property Damage
======================================================================

## Synopsis 

This report analyzes the impact of major storm events to human fatalities, injuries and property damage across the United States. Our hypothesis is that different storm types will have dramatically different effects on property damage and health risk based on less frequent, but more devastating characteristics.   To evaluate these hypotheses, we analyzed the US National Oceanic and Atmospheric Administration's (NOAA) Storm Database which tracks major storm events from the January 1950 through November 2011.  This US database contains characteristics of major US storms and weather events as well as estimated fatalities, injuries and property damage.

Our results show that overall economic damage and human health impact do depend significantly on the type of event. However, different weather events were responsible for economic damage than for effect on human life. The combination of flooding, hurricanes and tropical storms have had the most significant economic impact with over $285US billion of estimated damage.  However, tornados have caused the majority of health related injuries while a combination of tornados, heat and flash flooding has caused the most fatalities.


## Data Processing

Critical data from the NOAA database was loaded in order to perform the analysis including storm type, estimated fatalities, injuries, property and crop damage, date of event, dimensions (width and length), strength/severity (such as F strength for tornados or magnitude for earthquakes), and date of event.

Several procedures were used to load the data:

* load the raw data from the provided .bz2 file

* select only the columns of data related to our variables in question

* calculate the correct property and crop damage using the base values and corresponding exponents

* calculate a value for total economic damage

* tidy up the event names to get common and clean types for analysis

* discard events that did not have any economic or human impact

The details of the process are described in the following sections.

### Downloading and reading the database

The NOAA database file was downloaded from the [Coursera Reproducible Research project](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) on May 9, 2014 and processed in its native bz2 format.  Due to the size of the file, only the necessary columns of data were read and processed.  The event's starting date and time was converted to just the date.  Data that was needed in numeric format was converted to numeric values. String values were converted to factors. White spaces were trimmed from the columns of data while reading the file.

The file's internal format is comma delimited with a header row describes each of the columns.  


```r
# libraries and load data
library(ggplot2)
library(plyr)
library(reshape2)
library(xtable)
library(grid)  # required by gridExtra so load just to be safe
library(gridExtra)
```


```r
datafile <- "repdata-data-StormData.csv.bz2"
if (!file.exists(datafile)) {
    # download it if it does not exist
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url, datafile, mode = "wb")
}
# We read the first few rows and then reset the file to only read the
# columns we care about This is just to speed up the lenghty read and to
# reduce the required memory footprint
data <- read.table(bzfile(datafile), sep = ",", header = TRUE, na.strings = "", 
    stringsAsFactors = FALSE, nrows = 10)

# Now read all the data that we care about.  Originally we had all these
# fields: 'STATE__' 'BGN_DATE' 'BGN_TIME' 'TIME_ZONE' 'COUNTY' 'COUNTYNAME'
# 'STATE' 'EVTYPE' 'BGN_RANGE' 'BGN_AZI' 'BGN_LOCATI' 'END_DATE' 'END_TIME'
# 'COUNTY_END' 'COUNTYENDN' 'END_RANGE' 'END_AZI' 'END_LOCATI' 'LENGTH'
# 'WIDTH' 'F' 'MAG' 'FATALITIES' 'INJURIES' 'PROPDMG' 'PROPDMGEXP' 'CROPDMG'
# 'CROPDMGEXP' 'WFO' 'STATEOFFIC' 'ZONENAMES' 'LATITUDE' 'LONGITUDE'
# 'LATITUDE_E' 'LONGITUDE_' 'REMARKS' 'REFNUM'

# Now we just set the columns we need for our analysis to save space and
# time
neededCharCols <- c("STATE", "BGN_DATE", "EVTYPE", "F", "PROPDMGEXP", "CROPDMGEXP")
neededNumCols <- c("LENGTH", "WIDTH", "MAG", "FATALITIES", "INJURIES", "PROPDMG", 
    "CROPDMG")
# Set the columns we want, others set to NULL to ignore
classes <- rep("NULL", length(names(data)))
classes[names(data) %in% neededCharCols] <- "factor"  # set just the cols we want as char factors
classes[names(data) %in% neededNumCols] <- "numeric"  # set just the cols we want as numeric

# read the data and strip white spaces from any of the fields
data <- read.table(bzfile(datafile), sep = ",", header = TRUE, na.strings = "", 
    colClasses = classes, quote = "\"", strip.white = TRUE)

# Convert the begining date and time to just the date part and ignore the
# time and timezone data$BGN_DATE <- strptime(data$BGN_DATE, format =
# '%m/%d/%Y %H:%M:%s')
data$EVTDATE <- as.Date(data$BGN_DATE, "%m/%d/%Y")
```


The resulting dataset has 902297 rows of data.  There were a significant number of missing or NA values from event specfic parameters such as F-value, Property and Crop exponent values.  There were no NA or missing values from property damage, crop damage, event types, injuries, and fatailities.  


```r
dim(data)
```

```
## [1] 902297     14
```

```r
head(data)
```

```
##             BGN_DATE STATE  EVTYPE LENGTH WIDTH F MAG FATALITIES INJURIES
## 1  4/18/1950 0:00:00    AL TORNADO   14.0   100 3   0          0       15
## 2  4/18/1950 0:00:00    AL TORNADO    2.0   150 2   0          0        0
## 3  2/20/1951 0:00:00    AL TORNADO    0.1   123 2   0          0        2
## 4   6/8/1951 0:00:00    AL TORNADO    0.0   100 2   0          0        2
## 5 11/15/1951 0:00:00    AL TORNADO    0.0   150 2   0          0        2
## 6 11/15/1951 0:00:00    AL TORNADO    1.5   177 2   0          0        6
##   PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP    EVTDATE
## 1    25.0          K       0       <NA> 1950-04-18
## 2     2.5          K       0       <NA> 1950-04-18
## 3    25.0          K       0       <NA> 1951-02-20
## 4     2.5          K       0       <NA> 1951-06-08
## 5     2.5          K       0       <NA> 1951-11-15
## 6     2.5          K       0       <NA> 1951-11-15
```

```r

# Basic summary stats
sapply(data, class)
```

```
##   BGN_DATE      STATE     EVTYPE     LENGTH      WIDTH          F 
##   "factor"   "factor"   "factor"  "numeric"  "numeric"   "factor" 
##        MAG FATALITIES   INJURIES    PROPDMG PROPDMGEXP    CROPDMG 
##  "numeric"  "numeric"  "numeric"  "numeric"   "factor"  "numeric" 
## CROPDMGEXP    EVTDATE 
##   "factor"     "Date"
```

```r
summary(data)
```

```
##               BGN_DATE          STATE                      EVTYPE      
##  5/25/2011 0:00:00:  1202   TX     : 83728   HAIL             :288661  
##  4/27/2011 0:00:00:  1193   KS     : 53440   TSTM WIND        :219940  
##  6/9/2011 0:00:00 :  1030   OK     : 46802   THUNDERSTORM WIND: 82563  
##  5/30/2004 0:00:00:  1016   MO     : 35648   TORNADO          : 60652  
##  4/4/2011 0:00:00 :  1009   IA     : 31069   FLASH FLOOD      : 54277  
##  4/2/2006 0:00:00 :   981   NE     : 30271   FLOOD            : 25326  
##  (Other)          :895866   (Other):621339   (Other)          :170878  
##      LENGTH           WIDTH         F               MAG       
##  Min.   :   0.0   Min.   :   0   0   : 24993   Min.   :    0  
##  1st Qu.:   0.0   1st Qu.:   0   1   : 19475   1st Qu.:    0  
##  Median :   0.0   Median :   0   2   :  9878   Median :   50  
##  Mean   :   0.2   Mean   :   8   3   :  3179   Mean   :   47  
##  3rd Qu.:   0.0   3rd Qu.:   0   4   :  1072   3rd Qu.:   75  
##  Max.   :2315.0   Max.   :4400   5   :   137   Max.   :22000  
##                                  NA's:843563                  
##    FATALITIES     INJURIES         PROPDMG       PROPDMGEXP    
##  Min.   :  0   Min.   :   0.0   Min.   :   0   K      :424665  
##  1st Qu.:  0   1st Qu.:   0.0   1st Qu.:   0   M      : 11330  
##  Median :  0   Median :   0.0   Median :   0   0      :   216  
##  Mean   :  0   Mean   :   0.2   Mean   :  12   B      :    40  
##  3rd Qu.:  0   3rd Qu.:   0.0   3rd Qu.:   0   5      :    28  
##  Max.   :583   Max.   :1700.0   Max.   :5000   (Other):    84  
##                                                NA's   :465934  
##     CROPDMG        CROPDMGEXP        EVTDATE          
##  Min.   :  0.0   K      :281832   Min.   :1950-01-03  
##  1st Qu.:  0.0   M      :  1994   1st Qu.:1995-04-20  
##  Median :  0.0   k      :    21   Median :2002-03-18  
##  Mean   :  1.5   0      :    19   Mean   :1998-12-27  
##  3rd Qu.:  0.0   B      :     9   3rd Qu.:2007-07-28  
##  Max.   :990.0   (Other):     9   Max.   :2011-11-30  
##                  NA's   :618413
```

```r
range(data$EVTDATE)
```

```
## [1] "1950-01-03" "2011-11-30"
```


### Calculating financial impact to property and crops

To assess the correct financial impact of property and crop damage, values for damages required transformation using the base magnitude and the exponent fields.  Exponents were reported in 10^x , thousands (k or K), millions (m or M), billions (B), and hundreds (h or H).  Exponent values of "-, +, ?" or NA were ignored and assumed to imply no multiplication (i.e. multiply by 1).

New variables were created for the computed crop and property economic impacts and totaled into a 3rd variable.


```r
# Show how messy the exponents are
unique(data$PROPDMGEXP)
```

```
##  [1] K    M    <NA> B    m    +    0    5    6    ?    4    2    3    h   
## [15] 7    H    -    1    8   
## Levels: - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(data$CROPDMGEXP)
```

```
## [1] <NA> M    K    m    B    ?    0    k    2   
## Levels: ? 0 2 B k K m M
```

```r


# Let's build a table to get the real multiplier expTag and mult must align
# with key,values
expTag <- c(0:9, "h", "H", "k", "K", "m", "M", "b", "B", "-", "+", "?", NA)
mult = c(10^(0:9), 10^2, 10^2, 10^3, 10^3, 10^6, 10^6, 10^9, 10^9, 1, 1, 1, 
    1)

# Calculate the damages to property and crops based on the multiplier.
# Rather than merge, we will add a multiplier by matching so we don't change
# the original dataframe
data$PROPDAMAGE <- data$PROPDMG * mult[match(data$PROPDMGEXP, expTag)]
data$CROPDAMAGE <- data$CROPDMG * mult[match(data$CROPDMGEXP, expTag)]
# merge(data, expMultiplier, by.x='CROPDMGEXP', by.y='exp', all.x=TRUE)

# Add up the total damage for each observation
data$ECONOMICDAMAGE <- data$PROPDAMAGE + data$CROPDAMAGE
```


## Clean up messy data and filter injuries, death and damage events

While the rest of the data was fairly clean, the event types were extremely dirty and required transformation for processing.  There were 985 unique types of events, many of which were duplicate or misspellings of common events such as "TSTM WIND" and "THUNDERSTORM WIND," "FREEZING RAIN AND SNOW," and "FREEZING RAIN/SNOW"; or specific event types with detail such as "HAIL 1.75" and "HAIL 075," or "HURRICANE EMILY" and "HURRICANE ERIN." Additionally, there were many summary lines such as "Summary August 10," and "SUMMARAY OF MARCH 24-25" which needed to be removed from the data.

We cleaned many of these up based on assumptions on common patterns found in the data and reduced the number of event types by over a factor of 10. 



```r

# First let's remove any data that is a 'summary' type
data2 <- data[!grepl("^Summary", data$EVTYPE, ignore.case = TRUE), ]  #  No damage or death amounts but good to clean up
data2$EVTYPE = toupper(as.character(data2$EVTYPE))  # Convert everything as upper case and remove the factor


#################################################################################################### Now we clean up the event types that are common but named differently.
#################################################################################################### FROM HERE TO THE END OF THIS BIG BLOCK IS JUST CLEANUP OF EVENT TYPES
trace <- FALSE  # set to TRUE to view EVTYPES matched by this pattern

# This function has the side effect of changing the EVTYPE of data2
# dataframe.  It replaces all values that match the search pattern with the
# new value.
convertEVTYPE <- function(newValue, searchPat) {
    # Trace if we are looking at details
    if (trace) {
        print(newValue)
        print("Matches: ")
        print(sort(unique(grep(searchPat, data2$EVTYPE, ignore.case = TRUE, 
            value = TRUE))))
    }
    # NOTICE THE <<- OPERATOR!
    data2[grepl(searchPat, data2$EVTYPE, ignore.case = TRUE), "EVTYPE"] <<- newValue
}

# Thunderstorm misspellings/common names
searchPat = "^THUNDERSTORM|^THUDERSTORM|TSTM|^SEVERE THUNDERSTORM|^THUNERSTORM|^THUNDERSTROM|^THUNDERTORM|^THUNDEERSTORM|^THUNDERTSORM|^TUNDERSTORM|^THUNDERESTORM|^THUNDESTORM"
convertEVTYPE("THUNDERSTORM", searchPat)

# Flash Flood
searchPat <- "FLASH FLOO[O]*D|FLOOD FLASH|FLOOD/FLASHFLOOD|^FLOOD/FLASH|RAPIDLY RISING WATER|LOCAL FLASH FLOOD"
convertEVTYPE("FLASH FLOOD", searchPat)

# COASTAL FLOOD
searchPat <- "^COASTAL[ ]+FLOOD|COASTAL/TIDAL FLOOD|CSTL FLOODING|COASTALFLOOD|Erosion/Cstl Flood|^TIDAL FLOOD| COASTAL FLOOD|^BEACH FLOOD|^HEAVY SURF COASTAL FLOODING|^COASTAL SURGE|^STORM SURG|^BEACH EROSION/COASTAL FLOOD"
convertEVTYPE("COASTAL FLOOD", searchPat)

# Flood - note must come after other floods
searchPat <- "^FLOOD|^SMALL STREAM|Sml Stream Fld|URBAN FLOOD|URBAN[ /]SMALL STREAM|URBAN/SMALL STRM FLDG|URBAN AND SMALL STREAM|URBAN/SMALL FLOODING|^RIVER FLOODING|^RIVER AND STREAM FLOOD|^LOCAL FLOOD|^MAJOR FLOOD|^STREAM FLOODING|^MINOR FLOOD|^RIVER FLOOD|^RURAL FLOOD|^URBAN/STREET FLOODING"
convertEVTYPE("FLOOD", searchPat)

# Tornado
searchPat <- "^TORNADO|^TORNDAO|COLD AIR TORNADO|COLD AIR FUNNEL"
convertEVTYPE("TORNADO", searchPat)

# HURRICANE
searchPat <- "^HURRICANE|^REMNANTS OF FLOYD|^TROPICAL STORM|^TROPICAL DEPRESSION"
convertEVTYPE("HURRICANE/TROPICAL STORM", searchPat)

# LIGHTNING
searchPat <- "^LIGHTNING|LIGNTNING|^LIGHTING| LIGHTNING"
convertEVTYPE("LIGHTNING", searchPat)

# Wind
searchPat <- "^HIGH WIND|^WIND GUST|^WIND DAMAGE|^WIND STORM|^WIND AND|^WIND ADVISORY|^WINDS|^wind$|^WND|^STRONG WIND|^ WIND$|^HIGH  WINDS|^GUSTY WIND|^STORM FORCE WINDS|^NON-SEVERE WIND DAMAGE|^GUSTY THUNDERSTORM WINDS|^GRADIENT WINDS"
convertEVTYPE("WIND", searchPat)

# WATERSPOUT
searchPat <- "WATERSPOUT|^WATER SPOUT|^WAYTERSPOUT"
convertEVTYPE("WATERSPOUT", searchPat)

# DROUGHT
searchPat <- "^DROUGHT|^DRY$|^DRY CONDITIONS|^DRY HOT WEATHER|^DRY PATTERN|^DRY SPELL|^DRY WEATHER|^DRYNESS|EXCESSIVE HEAT/DROUGHT|EXCESSIVELY DRY|^VERY DRY|^WARM DRY CONDITIONS|^UNSEASONABLY DRY|^Record dry|ABNORMALLY DRY|^BELOW NORMAL PRECIPITATION|^DRIEST MONTH|^RECORD LOW RAINFALL"
convertEVTYPE("DROUGHT", searchPat)

# Wintery storm or mix
searchPat <- "^WINTER STORM|Winter[y]* mix|^WINTRY MIX|^Winter Weather"
convertEVTYPE("WINTER STORM", searchPat)

# Heavy Snow
searchPat <- "^HEAVY SNOW|^SNOW[^M]|Snow|BLIZZARD"
convertEVTYPE("SNOW", searchPat)

# Hail
searchPat <- "^HAIL|DEEP HAIL|^SMALL HAIL|^LATE SEASON HAIL|^NON SEVERE HAIL"
convertEVTYPE("HAIL", searchPat)

# FREEZING RAIN
searchPat <- "^SLEET|^FREEZING RAIN|^FREEZING DRIZZLE|^ICE PELLETS|^LIGHT FREEZING RAIN"
convertEVTYPE("SLEET/FREEZING RAIN", searchPat)

# FROST
searchPat <- "^FREEZE$|^FROST|^HARD FREEZE|^DAMAGING FREEZE|^EARLY FREEZE|^EARLY FROST|^AGRICULTURAL FREEZE|^LATE FREEZE"
convertEVTYPE("FREEZE", searchPat)

# Cold or record low
searchPat <- "^RECORD LOW$|^RECORD *COLD|^RECORD COOL|^COLD|^COOL|^UNSEASONABLY COOL|^UNSEASONABLY COLD|^UNUSUALLY COLD|^BITTER WIND CHILL|^EXTREME COLD|^Extended Cold|^LOW TEMPERATURE|^Excessive Cold|EXTREME/RECORD COLD|^Unseasonable Cold|^SEVERE COLD|^UNSEASONAL LOW TEMP|^PROLONG COLD"
convertEVTYPE("COLD", searchPat)

# ICE Jam
searchPat <- "^ICE JAM|^ICE FLOES|^BREAKUP FLOODING"
convertEVTYPE("ICE JAM", searchPat)

# ICE - be very specific here
searchPat <- "^ICE FOG|^ICE ON ROAD|^IC[EY] ROADS|^ICE STORM|^ICE/STRONG WINDS|^BLACK ICE|^GLAZE$|^GLAZE[ /]ICE|^PATCHY ICE"
convertEVTYPE("ICE", searchPat)

# WIND CHILL
searchPat <- "WIND.*CHILL"
convertEVTYPE("WIND CHILL", searchPat)

# Heat or record highs
searchPat <- "^RECORD HEAT|^RECORD HIGH|^RECORD TEMPERATURE|^RECORD WARM|^RECORD/EXCESSIVE HEAT|^HEAT|UNSEASONABLY HOT|UNSEASONABLY WARM|UNUSUAL WARM|UNUSUAL/RECORD WARMTH|UNUSUALLY WARM|ABNORMAL WARMTH|EXCESSIVE HEAT|EXTREME HEAT|^Temperature record|^VERY WARM|^HIGH TEMPERATURE RECORD|^HOT SPELL|^HOT SPELL|^HOT WEATHER|^VERY WARM|^WARM WEATHER|^PROLONG WARMTH"
convertEVTYPE("HEAT", searchPat)

# Rain
searchPat <- "^RAIN|^HEAVY RAIN|HEAVY PRECIP[AI]TATION|^EXCESSIVE RAINFALL|^EXCESSIVE WETNESS|^TORRENTIAL RAIN|^RECORD RAINFALL|^RECORD/EXCESSIVE RAINFALL|^RECORD PRECIPITATION|^RECORD/EXCESSIVE RAINFALL|^ABNORMALLY WET|^EXCESSIVE PRECIPITATION|^EXCESSIVE PRECIPITATION|^EXCESSIVE RAIN|^HVY RAIN|^HEAVY SHOWER|^LOCALLY HEAVY RAIN|^PROLONGED RAIN"
convertEVTYPE("RAIN", searchPat)

# Fire
searchPat <- "^WILD|BRUSH FIRE|^GRASS FIRES"  # NOTE '^WILD' was checked to make sure it only covered WILDFIRES
convertEVTYPE("WILDFIRE", searchPat)

# DRY MICROBURST
searchPat <- "DRY MICROBURST"
convertEVTYPE("DRY MICROBURST", searchPat)

# Beach erosion
searchPat <- "^BEACH EROSIN|^BEACH EROSION"
convertEVTYPE("BEACH EROSION", searchPat)

# COASTAL STORM
searchPat <- "^COASTALSTORM"
convertEVTYPE("COASTAL STORM", searchPat)

# DAM BREAK/DAM FAILURE
searchPat <- "^DAM"
convertEVTYPE("DAM FAILURE", searchPat)

# FUNNEL*CLOUDS
searchPat <- "^FUNNEL.*|^WALL CLOUD/FUNNEL CLOUD"
convertEVTYPE("FUNNEL", searchPat)

# High surf, Swells
searchPat <- "^[ ]*HIGH SURF|^HEAVY SURF|^HIGH[ ]*SWELLS|^HIGH SEAS|^HIGH WAVES|^HEAVY SEAS|^HEAVY SWELLS|^ROUGH SURF|^HAZARDOUS SURF"
convertEVTYPE("HIGH SURF/SWELLS", searchPat)

# RIP CURRENT
searchPat <- "^RIP.*CURRENT"
convertEVTYPE("RIP CURRENT", searchPat)

# MUDSLIDES
searchPat <- "^MUD.*SLIDE"
convertEVTYPE("MUD SLIDE", searchPat)

# VOLCANIC *
searchPat <- "^VOLCANIC"
convertEVTYPE("VOLCANIC", searchPat)

# FOG *
searchPat <- "^DENSE FOG|^FOG$|^FOG AND COLD TEMPERATURES|^FREEZING FOG|^PATCHY DENSE FOG"
convertEVTYPE("FOG", searchPat)

# These are the categories we are left with
if (trace) print(sort(unique(data2$EVTYPE)))

# END OF CLEANING UP EVENT TYPES

# Create a dataframe which only has fatalities, injuries or damage
damageHealthData <- data2[data2$ECONOMICDAMAGE != 0 | data2$INJURIES > 0 | data2$FATALITIES > 
    0, ]

# Convert the EVTYPEs back to factors
data2$EVTYPE <- factor(data2$EVTYPE)
damageHealthData$EVTYPE <- factor(damageHealthData$EVTYPE)

# delete data2 to save space but can be commented out if needing to preserve
# total cleaned data
rm(data2)
```


So at the end of all data loading and transformation processing we had 2 datasets.  The raw dataset from the data file (data), and a dataset with the consolidated, common events but only containing records that produced economic damage, injury or death (damageHealthData) which will be analyzed.  From the original data to the tidy data to be analyzed, we reduced the number of event types from 985 down to 88 .


```r
# Original Data basic dimensions and # events
length(levels(data$EVTYPE))
```

```
## [1] 985
```

```r
dim(data)
```

```
## [1] 902297     17
```

```r

# Final Data basic dimensions and # events
dim(damageHealthData)  # Size of resulting dataframe
```

```
## [1] 254633     17
```

```r
length(levels(damageHealthData$EVTYPE))  # Number of resulting event types (reduced by factor of 10!)
```

```
## [1] 88
```

```r

summary(data)
```

```
##               BGN_DATE          STATE                      EVTYPE      
##  5/25/2011 0:00:00:  1202   TX     : 83728   HAIL             :288661  
##  4/27/2011 0:00:00:  1193   KS     : 53440   TSTM WIND        :219940  
##  6/9/2011 0:00:00 :  1030   OK     : 46802   THUNDERSTORM WIND: 82563  
##  5/30/2004 0:00:00:  1016   MO     : 35648   TORNADO          : 60652  
##  4/4/2011 0:00:00 :  1009   IA     : 31069   FLASH FLOOD      : 54277  
##  4/2/2006 0:00:00 :   981   NE     : 30271   FLOOD            : 25326  
##  (Other)          :895866   (Other):621339   (Other)          :170878  
##      LENGTH           WIDTH         F               MAG       
##  Min.   :   0.0   Min.   :   0   0   : 24993   Min.   :    0  
##  1st Qu.:   0.0   1st Qu.:   0   1   : 19475   1st Qu.:    0  
##  Median :   0.0   Median :   0   2   :  9878   Median :   50  
##  Mean   :   0.2   Mean   :   8   3   :  3179   Mean   :   47  
##  3rd Qu.:   0.0   3rd Qu.:   0   4   :  1072   3rd Qu.:   75  
##  Max.   :2315.0   Max.   :4400   5   :   137   Max.   :22000  
##                                  NA's:843563                  
##    FATALITIES     INJURIES         PROPDMG       PROPDMGEXP    
##  Min.   :  0   Min.   :   0.0   Min.   :   0   K      :424665  
##  1st Qu.:  0   1st Qu.:   0.0   1st Qu.:   0   M      : 11330  
##  Median :  0   Median :   0.0   Median :   0   0      :   216  
##  Mean   :  0   Mean   :   0.2   Mean   :  12   B      :    40  
##  3rd Qu.:  0   3rd Qu.:   0.0   3rd Qu.:   0   5      :    28  
##  Max.   :583   Max.   :1700.0   Max.   :5000   (Other):    84  
##                                                NA's   :465934  
##     CROPDMG        CROPDMGEXP        EVTDATE             PROPDAMAGE      
##  Min.   :  0.0   K      :281832   Min.   :1950-01-03   Min.   :0.00e+00  
##  1st Qu.:  0.0   M      :  1994   1st Qu.:1995-04-20   1st Qu.:0.00e+00  
##  Median :  0.0   k      :    21   Median :2002-03-18   Median :0.00e+00  
##  Mean   :  1.5   0      :    19   Mean   :1998-12-27   Mean   :4.75e+05  
##  3rd Qu.:  0.0   B      :     9   3rd Qu.:2007-07-28   3rd Qu.:5.00e+02  
##  Max.   :990.0   (Other):     9   Max.   :2011-11-30   Max.   :1.15e+11  
##                  NA's   :618413                                          
##    CROPDAMAGE       ECONOMICDAMAGE    
##  Min.   :0.00e+00   Min.   :0.00e+00  
##  1st Qu.:0.00e+00   1st Qu.:0.00e+00  
##  Median :0.00e+00   Median :0.00e+00  
##  Mean   :5.44e+04   Mean   :5.29e+05  
##  3rd Qu.:0.00e+00   3rd Qu.:1.00e+03  
##  Max.   :5.00e+09   Max.   :1.15e+11  
## 
```

```r
# summary(data2)
summary(damageHealthData)
```

```
##               BGN_DATE          STATE                 EVTYPE      
##  4/27/2011 0:00:00:   761   TX     : 22144   THUNDERSTORM:119786  
##  4/4/2011 0:00:00 :   649   IA     : 16093   TORNADO     : 39961  
##  6/21/2011 0:00:00:   511   OH     : 13337   HAIL        : 26162  
##  5/31/1998 0:00:00:   504   MS     : 12023   FLASH FLOOD : 21605  
##  6/4/2008 0:00:00 :   486   GA     : 11207   LIGHTNING   : 13303  
##  5/25/2011 0:00:00:   440   AL     : 11121   FLOOD       : 11314  
##  (Other)          :251282   (Other):168708   (Other)     : 22502  
##      LENGTH           WIDTH         F               MAG      
##  Min.   :   0.0   Min.   :   0   0   :  9974   Min.   :   0  
##  1st Qu.:   0.0   1st Qu.:   0   1   : 15930   1st Qu.:   0  
##  Median :   0.0   Median :   0   2   :  8933   Median :   0  
##  Mean   :   0.7   Mean   :  22   3   :  2998   Mean   :  31  
##  3rd Qu.:   0.0   3rd Qu.:   0   4   :   994   3rd Qu.:  52  
##  Max.   :1845.0   Max.   :4400   5   :   129   Max.   :3430  
##                                  NA's:215675                 
##    FATALITIES       INJURIES         PROPDMG       PROPDMGEXP    
##  Min.   :  0.0   Min.   :   0.0   Min.   :   0   K      :231428  
##  1st Qu.:  0.0   1st Qu.:   0.0   1st Qu.:   2   M      : 11320  
##  Median :  0.0   Median :   0.0   Median :   5   0      :   210  
##  Mean   :  0.1   Mean   :   0.6   Mean   :  43   B      :    40  
##  3rd Qu.:  0.0   3rd Qu.:   0.0   3rd Qu.:  25   5      :    18  
##  Max.   :583.0   Max.   :1700.0   Max.   :5000   (Other):    32  
##                                                  NA's   : 11585  
##     CROPDMG        CROPDMGEXP        EVTDATE             PROPDAMAGE      
##  Min.   :  0.0   K      : 99932   Min.   :1950-01-03   Min.   :0.00e+00  
##  1st Qu.:  0.0   M      :  1985   1st Qu.:1997-01-23   1st Qu.:2.00e+03  
##  Median :  0.0   k      :    21   Median :2002-08-02   Median :1.00e+04  
##  Mean   :  5.4   0      :    17   Mean   :2000-06-13   Mean   :1.68e+06  
##  3rd Qu.:  0.0   B      :     7   3rd Qu.:2008-05-07   3rd Qu.:3.50e+04  
##  Max.   :990.0   (Other):     7   Max.   :2011-11-30   Max.   :1.15e+11  
##                  NA's   :152664                                          
##    CROPDAMAGE       ECONOMICDAMAGE    
##  Min.   :0.00e+00   Min.   :0.00e+00  
##  1st Qu.:0.00e+00   1st Qu.:2.50e+03  
##  Median :0.00e+00   Median :1.00e+04  
##  Mean   :1.93e+05   Mean   :1.87e+06  
##  3rd Qu.:0.00e+00   3rd Qu.:5.00e+04  
##  Max.   :5.00e+09   Max.   :1.15e+11  
## 
```



## Results

By analyzing just the storm events that caused economic damage, fatalities or injuries, we can assess the economic and public health impacts of significant weather events.  Overall, thunderstorms were most frequently responsible for some type of economic or public health event.  The frequency of tornados, hail, flooding, lightning and wind combined contributed to about as many events as thunderstorms.  The total counts of the most frequent events and the proportion of those events to the total number of reported events with impacts to health or property/crop damage is presented in the table below.



```r

totalDamage <- colSums(damageHealthData[c("ECONOMICDAMAGE", "FATALITIES", "INJURIES")])
totalRecords <- nrow(damageHealthData)
# Summarize by Event Type
damageHealthSummary <- ddply(damageHealthData, .(EVTYPE), summarize, Count = length(EVTYPE), 
    Proportion = length(EVTYPE)/totalRecords * 100, Economic_Damage = sum(ECONOMICDAMAGE), 
    Economic_Percent = sum(ECONOMICDAMAGE)/totalDamage["ECONOMICDAMAGE"] * 100, 
    Fatalities = sum(FATALITIES), Fatality_Percent = sum(FATALITIES)/totalDamage["FATALITIES"] * 
        100, Injuries = sum(INJURIES), Injury_Percent = sum(INJURIES)/totalDamage["INJURIES"] * 
        100)
names(damageHealthSummary)[1] <- "Event_Type"
topX <- 15

# Print a table of the frequency of events
ds <- format(damageHealthSummary[order(damageHealthSummary$Count, decreasing = TRUE), 
    c("Event_Type", "Count", "Proportion")][1:topX, ], big.mark = ",", scientific = FALSE, 
    digits = 1)
xt <- xtable(ds, display = c("s", "s", "d", "f"), caption = "Table 1: Frequency of Events in Analyzed Dataset")
align(xt) <- c("l", "l", "r", "r")

print(xt, include.rownames = FALSE, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Thu May 22 18:57:38 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 1: Frequency of Events in Analyzed Dataset </CAPTION>
<TR> <TH> Event_Type </TH> <TH> Count </TH> <TH> Proportion </TH>  </TR>
  <TR> <TD> THUNDERSTORM </TD> <TD align="right"> 119,786 </TD> <TD align="right"> 47.0 </TD> </TR>
  <TR> <TD> TORNADO </TD> <TD align="right">  39,961 </TD> <TD align="right"> 15.7 </TD> </TR>
  <TR> <TD> HAIL </TD> <TD align="right">  26,162 </TD> <TD align="right"> 10.3 </TD> </TR>
  <TR> <TD> FLASH FLOOD </TD> <TD align="right">  21,605 </TD> <TD align="right">  8.5 </TD> </TR>
  <TR> <TD> LIGHTNING </TD> <TD align="right">  13,303 </TD> <TD align="right">  5.2 </TD> </TR>
  <TR> <TD> FLOOD </TD> <TD align="right">  11,314 </TD> <TD align="right">  4.4 </TD> </TR>
  <TR> <TD> WIND </TD> <TD align="right">   9,788 </TD> <TD align="right">  3.8 </TD> </TR>
  <TR> <TD> SNOW </TD> <TD align="right">   2,153 </TD> <TD align="right">  0.8 </TD> </TR>
  <TR> <TD> WINTER STORM </TD> <TD align="right">   2,062 </TD> <TD align="right">  0.8 </TD> </TR>
  <TR> <TD> WILDFIRE </TD> <TD align="right">   1,257 </TD> <TD align="right">  0.5 </TD> </TR>
  <TR> <TD> RAIN </TD> <TD align="right">   1,154 </TD> <TD align="right">  0.5 </TD> </TR>
  <TR> <TD> HEAT </TD> <TD align="right">     979 </TD> <TD align="right">  0.4 </TD> </TR>
  <TR> <TD> ICE </TD> <TD align="right">     782 </TD> <TD align="right">  0.3 </TD> </TR>
  <TR> <TD> HURRICANE/TROPICAL STORM </TD> <TD align="right">     680 </TD> <TD align="right">  0.3 </TD> </TR>
  <TR> <TD> RIP CURRENT </TD> <TD align="right">     643 </TD> <TD align="right">  0.3 </TD> </TR>
   </TABLE>

Table 1 shows the total number and proportion of events causing health or economic damage in the entire dataset for the most frequent 15 event types.  This data is useful when comparing the number of events to the size and significance of impacts for each event classification (see Tables 2 and 3).


### Economic impact

But when measuring the economic contribution of all storm events which caused health or economic damage, flooding, hurricanes and tropical storms were responsible for over 50% of all the estimated economic impact. Tornados accounted for 12%.  While not as frequent, these events had catastrophic impacts on property.  For example, hurricanes/tropical storms occurred as only 0.3% of the events, but contributed to over 20% of all economic damage. Similarly floods accounted for 4.4% of events but 33.7% of estimated economic impact.



```r
# Top Economic Damage of events
economicTop <- damageHealthSummary[order(damageHealthSummary$Economic_Damage, 
    decreasing = TRUE), c("Event_Type", "Economic_Damage", "Economic_Percent")][1:topX, 
    ]

# Print a table of top damage events
ds <- format(economicTop, big.mark = ",", scientific = FALSE, digits = 1)  # separate variable to print to screens
xt <- xtable(ds, display = c("s", "s", "d", "f"), caption = "Table 2: Top 15 Highest Economic Damage Totals by Event Type in US Dollars")
align(xt) <- c("l", "l", "r", "r")

print(xt, include.rownames = FALSE, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Thu May 22 18:57:39 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 2: Top 15 Highest Economic Damage Totals by Event Type in US Dollars </CAPTION>
<TR> <TH> Event_Type </TH> <TH> Economic_Damage </TH> <TH> Economic_Percent </TH>  </TR>
  <TR> <TD> FLOOD </TD> <TD align="right"> 161,057,708,359 </TD> <TD align="right"> 33.7 </TD> </TR>
  <TR> <TD> HURRICANE/TROPICAL STORM </TD> <TD align="right">  98,682,496,360 </TD> <TD align="right"> 20.7 </TD> </TR>
  <TR> <TD> TORNADO </TD> <TD align="right">  57,367,113,947 </TD> <TD align="right"> 12.0 </TD> </TR>
  <TR> <TD> COASTAL FLOOD </TD> <TD align="right">  48,409,964,060 </TD> <TD align="right"> 10.1 </TD> </TR>
  <TR> <TD> FLASH FLOOD </TD> <TD align="right">  19,122,009,246 </TD> <TD align="right">  4.0 </TD> </TR>
  <TR> <TD> HAIL </TD> <TD align="right">  19,024,427,636 </TD> <TD align="right">  4.0 </TD> </TR>
  <TR> <TD> DROUGHT </TD> <TD align="right">  15,018,677,780 </TD> <TD align="right">  3.1 </TD> </TR>
  <TR> <TD> THUNDERSTORM </TD> <TD align="right">  14,058,714,288 </TD> <TD align="right">  2.9 </TD> </TR>
  <TR> <TD> ICE </TD> <TD align="right">   8,984,854,860 </TD> <TD align="right">  1.9 </TD> </TR>
  <TR> <TD> WILDFIRE </TD> <TD align="right">   8,894,410,130 </TD> <TD align="right">  1.9 </TD> </TR>
  <TR> <TD> WIND </TD> <TD align="right">   6,950,953,183 </TD> <TD align="right">  1.5 </TD> </TR>
  <TR> <TD> WINTER STORM </TD> <TD align="right">   6,823,751,751 </TD> <TD align="right">  1.4 </TD> </TR>
  <TR> <TD> RAIN </TD> <TD align="right">   4,174,854,990 </TD> <TD align="right">  0.9 </TD> </TR>
  <TR> <TD> FREEZE </TD> <TD align="right">   2,016,261,000 </TD> <TD align="right">  0.4 </TD> </TR>
  <TR> <TD> SNOW </TD> <TD align="right">   1,939,376,800 </TD> <TD align="right">  0.4 </TD> </TR>
   </TABLE>

Table 2 shows the sum of all crop and property damage by each classified event type and the devastation caused by flooding (including FLOODS, FLASH FLOODS, and COASTAL FLOODS) and hurricanes/tropical storms compared to other event types.  Over 68% of all estimated damage was caused by flooding and hurricane/tropic storm event types while 12% was caused by tornados.  These 5 event types contributed to over 80% of the total economic impact.

We see that flooding caused most property damage (33.7% flood, 10.1% coastal flood, and 4% flash flood) while hurricanes and tropical storms followed closely behind (20.7%).  Property damage contributed significantly more than crop damage for all events and overall financial impact as demonstrated in the following graph.


```r
# First shape our data into a format for stack bar charts
topNames <- unique(economicTop$Event_Type)
economicMelt <- melt(data=damageHealthData[damageHealthData$EVTYPE %in% topNames,], 
                     id.vars="EVTYPE", measure.vars=c("PROPDAMAGE","CROPDAMAGE"), 
                     variable.name="DAMAGETYPE", value.name="ECONOMICDAMAGE")

# reorder the levels based on the total damage to make the charts look better
economicMelt$EVTYPE <- reorder(economicMelt$EVTYPE, -economicMelt$ECONOMICDAMAGE, sum, ordered=TRUE)

# y axis formatter to print nice lables in Billions US dollars
axisfmt <- function(x) { paste0("$", (x/10^9), "B") }

# this is the economic damage chart
g <- ggplot(economicMelt, aes(y=ECONOMICDAMAGE, x=EVTYPE, fill=DAMAGETYPE)) + 
  geom_bar(stat="identity") +  # could do position="dodge"
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        plot.title = element_text(size=rel(1.5), face="bold", vjust=2)) +
  scale_y_continuous(labels=axisfmt) + 
  ggtitle("Top 15 Economic Damage Event Types") +
  guides(fill=guide_legend(reverse=TRUE)) +  # flip the legend to look like the chart
  ylab("Econmic Damage in billions US $ ") + xlab("") 

grid.arrange(g, sub=textGrob("Fig 1: Display of weather related economic impact", vjust=-1, gp=gpar(font=2))) 
```

![plot of chunk c1](figure/c1.png) 

Figure 1 shows flooding and hurricanes/tropical storms are major contributors to economic damage especially when combining all types of flooding. The difference in property and crop damage cleary shows significantly more economic damage to property than to crops by these types of events.  


### Health related impact

Tornados are responsible for most injuries and deaths as reported in the NOAA Storm database.  Tornados account for 65% of all reported injuries and 37% of all fatalities.  Heat related deaths followed tornados as being the most fatal type of events and also caused a significantly high number of estimated injuries.  The frequency of thunderstorms also contributed to a high number of injuries although death was fairly rare.

It is important to point out that fatalities and injuries per event type is much higher for *heat* than any other event as seen in the tables below.



```r
# Top injury of events
injuryTop <- damageHealthSummary[order(damageHealthSummary$Injuries, decreasing = TRUE), 
    c("Event_Type", "Injuries", "Injury_Percent", "Count")][1:topX, ]
injuryTop$InjuryPerEvent <- with(injuryTop, Injuries/Count)

dsInjuries <- format(injuryTop, big.mark = ",", digits = 1)  # separate variable to print to screens
xt <- xtable(dsInjuries, display = c("s", "s", "d", "f", "d", "f"), caption = "Table 3: Top 15 Highest Injury Totals by Event Type")
align(xt) <- c("l", "l", "r", "r", "r", "r")

print(xt, include.rownames = FALSE, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Thu May 22 18:57:57 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 3: Top 15 Highest Injury Totals by Event Type </CAPTION>
<TR> <TH> Event_Type </TH> <TH> Injuries </TH> <TH> Injury_Percent </TH> <TH> Count </TH> <TH> InjuryPerEvent </TH>  </TR>
  <TR> <TD> TORNADO </TD> <TD align="right"> 91,364 </TD> <TD align="right"> 65.0 </TD> <TD align="right">  39,961 </TD> <TD align="right"> 2.29 </TD> </TR>
  <TR> <TD> THUNDERSTORM </TD> <TD align="right">  9,517 </TD> <TD align="right">  6.8 </TD> <TD align="right"> 119,786 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD> HEAT </TD> <TD align="right">  9,243 </TD> <TD align="right">  6.6 </TD> <TD align="right">     979 </TD> <TD align="right"> 9.44 </TD> </TR>
  <TR> <TD> FLOOD </TD> <TD align="right">  6,873 </TD> <TD align="right">  4.9 </TD> <TD align="right">  11,314 </TD> <TD align="right"> 0.61 </TD> </TR>
  <TR> <TD> LIGHTNING </TD> <TD align="right">  5,232 </TD> <TD align="right">  3.7 </TD> <TD align="right">  13,303 </TD> <TD align="right"> 0.39 </TD> </TR>
  <TR> <TD> ICE </TD> <TD align="right">  2,399 </TD> <TD align="right">  1.7 </TD> <TD align="right">     782 </TD> <TD align="right"> 3.07 </TD> </TR>
  <TR> <TD> WINTER STORM </TD> <TD align="right">  1,968 </TD> <TD align="right">  1.4 </TD> <TD align="right">   2,062 </TD> <TD align="right"> 0.95 </TD> </TR>
  <TR> <TD> SNOW </TD> <TD align="right">  1,964 </TD> <TD align="right">  1.4 </TD> <TD align="right">   2,153 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD> WIND </TD> <TD align="right">  1,878 </TD> <TD align="right">  1.3 </TD> <TD align="right">   9,788 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD> FLASH FLOOD </TD> <TD align="right">  1,802 </TD> <TD align="right">  1.3 </TD> <TD align="right">  21,605 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD> HURRICANE/TROPICAL STORM </TD> <TD align="right">  1,711 </TD> <TD align="right">  1.2 </TD> <TD align="right">     680 </TD> <TD align="right"> 2.52 </TD> </TR>
  <TR> <TD> WILDFIRE </TD> <TD align="right">  1,608 </TD> <TD align="right">  1.1 </TD> <TD align="right">   1,257 </TD> <TD align="right"> 1.28 </TD> </TR>
  <TR> <TD> HAIL </TD> <TD align="right">  1,371 </TD> <TD align="right">  1.0 </TD> <TD align="right">  26,162 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD> FOG </TD> <TD align="right">  1,077 </TD> <TD align="right">  0.8 </TD> <TD align="right">     189 </TD> <TD align="right"> 5.70 </TD> </TR>
  <TR> <TD> RIP CURRENT </TD> <TD align="right">    529 </TD> <TD align="right">  0.4 </TD> <TD align="right">     643 </TD> <TD align="right"> 0.82 </TD> </TR>
   </TABLE>

Table 3 shows the significance of tornados to high injury rates. Tornadic events were reported as the main cause of 65% of all estimated injuries.  Heat and thunderstorm related health impacts are also far more significant than found in property damage. However, heat related injury was significantly higher per event than any other event type, while thunderstorms injuries per event were fairly rare.



```r
# Top Fatalities of events
fatalityTop <- damageHealthSummary[order(damageHealthSummary$Fatalities, decreasing = TRUE), 
    c("Event_Type", "Fatalities", "Fatality_Percent", "Count")][1:topX, ]
fatalityTop$FatalsPerEvent <- with(fatalityTop, Fatalities/Count)

dsFatalities <- format(fatalityTop, big.mark = ",", digits = 1)  # separate variable to print to screens
xt <- xtable(dsFatalities, display = c("s", "s", "d", "f", "d", "f"), caption = "Table 4: Top 15 Highest Fatality Totals by Event Type")
align(xt) <- c("l", "l", "r", "r", "r", "r")

print(xt, include.rownames = FALSE, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Thu May 22 18:57:57 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 4: Top 15 Highest Fatality Totals by Event Type </CAPTION>
<TR> <TH> Event_Type </TH> <TH> Fatalities </TH> <TH> Fatality_Percent </TH> <TH> Count </TH> <TH> FatalsPerEvent </TH>  </TR>
  <TR> <TD> TORNADO </TD> <TD align="right"> 5,633 </TD> <TD align="right"> 37.2 </TD> <TD align="right">  39,961 </TD> <TD align="right"> 0.141 </TD> </TR>
  <TR> <TD> HEAT </TD> <TD align="right"> 3,176 </TD> <TD align="right"> 21.0 </TD> <TD align="right">     979 </TD> <TD align="right"> 3.244 </TD> </TR>
  <TR> <TD> FLASH FLOOD </TD> <TD align="right"> 1,036 </TD> <TD align="right">  6.8 </TD> <TD align="right">  21,605 </TD> <TD align="right"> 0.048 </TD> </TR>
  <TR> <TD> LIGHTNING </TD> <TD align="right">   817 </TD> <TD align="right">  5.4 </TD> <TD align="right">  13,303 </TD> <TD align="right"> 0.061 </TD> </TR>
  <TR> <TD> THUNDERSTORM </TD> <TD align="right">   745 </TD> <TD align="right">  4.9 </TD> <TD align="right"> 119,786 </TD> <TD align="right"> 0.006 </TD> </TR>
  <TR> <TD> RIP CURRENT </TD> <TD align="right">   577 </TD> <TD align="right">  3.8 </TD> <TD align="right">     643 </TD> <TD align="right"> 0.897 </TD> </TR>
  <TR> <TD> FLOOD </TD> <TD align="right">   512 </TD> <TD align="right">  3.4 </TD> <TD align="right">  11,314 </TD> <TD align="right"> 0.045 </TD> </TR>
  <TR> <TD> COLD </TD> <TD align="right">   442 </TD> <TD align="right">  2.9 </TD> <TD align="right">     466 </TD> <TD align="right"> 0.948 </TD> </TR>
  <TR> <TD> WIND </TD> <TD align="right">   434 </TD> <TD align="right">  2.9 </TD> <TD align="right">   9,788 </TD> <TD align="right"> 0.044 </TD> </TR>
  <TR> <TD> WINTER STORM </TD> <TD align="right">   279 </TD> <TD align="right">  1.8 </TD> <TD align="right">   2,062 </TD> <TD align="right"> 0.135 </TD> </TR>
  <TR> <TD> SNOW </TD> <TD align="right">   267 </TD> <TD align="right">  1.8 </TD> <TD align="right">   2,153 </TD> <TD align="right"> 0.124 </TD> </TR>
  <TR> <TD> AVALANCHE </TD> <TD align="right">   224 </TD> <TD align="right">  1.5 </TD> <TD align="right">     268 </TD> <TD align="right"> 0.836 </TD> </TR>
  <TR> <TD> HURRICANE/TROPICAL STORM </TD> <TD align="right">   201 </TD> <TD align="right">  1.3 </TD> <TD align="right">     680 </TD> <TD align="right"> 0.296 </TD> </TR>
  <TR> <TD> HIGH SURF/SWELLS </TD> <TD align="right">   171 </TD> <TD align="right">  1.1 </TD> <TD align="right">     237 </TD> <TD align="right"> 0.722 </TD> </TR>
  <TR> <TD> ICE </TD> <TD align="right">   109 </TD> <TD align="right">  0.7 </TD> <TD align="right">     782 </TD> <TD align="right"> 0.139 </TD> </TR>
   </TABLE>

Table 4 also demonstrates the impact of tornados to fatality rates. While tornados were significant in property damage, they are the most significant contributor to fatalities.  Heat related health fatalities are also far more significant than found in property damage.



Most injuries and deaths are caused by outlier events as seen in the diagrams below.


```r
# First shape our data into a format for stack bar charts
topFatalityNames <- unique(fatalityTop$Event_Type)
fatalityData <- damageHealthData[damageHealthData$EVTYPE %in% topFatalityNames, 
    ]
fatalityData$EVTYPE = factor(fatalityData$EVTYPE)


topInjuryNames <- unique(injuryTop$Event_Type)
injuryData <- damageHealthData[damageHealthData$EVTYPE %in% topInjuryNames, 
    ]
injuryData$EVTYPE = factor(injuryData$EVTYPE)

# Fatalities
g1 <- ggplot(fatalityData, aes(y = FATALITIES, x = reorder(EVTYPE, -FATALITIES, 
    sum, order = TRUE))) + geom_boxplot() + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, 
    hjust = 1), plot.title = element_text(size = rel(1.5), face = "bold", vjust = 2)) + 
    ggtitle("Top 15 Fatal Event Types") + ylab("Fatalities (Log10 scale)") + 
    xlab("")


# Injuries
g2 <- ggplot(injuryData, aes(y = INJURIES, x = reorder(EVTYPE, -INJURIES, sum, 
    order = TRUE))) + geom_boxplot() + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, 
    hjust = 1), plot.title = element_text(size = rel(1.5), face = "bold", vjust = 2)) + 
    ggtitle("Top 15 Injury Event Types") + ylab("Injuries (Log10 scale)") + 
    xlab("")

# Note due to log scale, 0 values will produce warnings which is ok

grid.arrange(g1, g2, nrow = 1, as.table = TRUE, main = "Estimated Fatalities and Injuries", 
    sub = textGrob("Fig 2: Display of Event Type Fatalities (left) and Injuries (right) using logarithmic scales", 
        vjust = -1, gp = gpar(font = 2)))
```

```
## Warning: Removed 217329 rows containing non-finite values (stat_boxplot).
## Warning: Removed 233510 rows containing non-finite values (stat_boxplot).
```

![plot of chunk c2](figure/c2.png) 

Figure 2 shows the most significant contributions to estimated injuries or death.  Most events contributing to high injury or deaths came from outlier events as seen by dots above the box plot wiskers.  

## Conclusion

By analyzing the NOAA Storm database of records from Jan 1950 through Nov 2011, we have determined the major economic and public health impacts related to major weather events.  By far, the combination of flooding, hurricanes and tropical storms has had the most significant economic impact with over $285US billion of estimated contribution.  However, tornados have caused the majority of health related injuries and deaths.  We also discovered that heat had a far more signficant contribution to health related events than property or crop damage.  These impacts were caused by the severity of the events and not the frequency.

Further research could be performed to correctly assess the inflation adjusted values of economic damage and the distribution of the economic and health related impacts to local geography such as states or counties or over time.

