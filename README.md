# qaqc_tools
A suite of tools to address common yet critical data curating tasks to address the particular needs of the LTER research community, but that  also that apply to QC needs of the broader research community.

# List of functions

##Date/Time Issues

# an initial crack at the date_ingest_checker function

# function dependencies:
# parse_iso_8601(), parse_date() functions from package parsedate

library(parsedate)

# arguments:
#   x: some input string we believe to be a time stamp
#   format: either "ISO8601", a format string we can pass to strptime, or NULL;
#   if NULL, we will use parsedate() to try and load (user will be warned)
#   optional arguments: precision, TZ (will be detected automatically if present
#   in the data)

function(x, format = c("ISO8601"), precision = NULL, TZ = NULL)) {
  
  # provide user with warnings and messages
  
      # check for precision input
  
  if (is.null(precision)) {
    
    cat("User did not specify a timestamp precision. Attempting to",
        "detect timestamp precision from supplied data...\n")
    
  }
  
  # check for TZ input
  
  if (is.null(TZ)) {
    
    cat("User did not specify a time zone. Attempting to detect time",
        "zone from supplied data...\n")
    
  }
  
  # check for format input
  
    if (is.null(format)) {
  
      warning("User did not specify a date-time format. Attempting to",
        "detect format from supplied data and parse using the function parsedate...\n")
        
        }

  # ensure input x is a character vector
  
  x <- as.character(x)
  
  # read in the raw datetime based on format
  
  if (format=="ISO8601") { # use parse_iso_8601() from parsedate
    
    timedate <- parse_iso_8601(x) # returns date in UTC
    
  } else if (is.null(format)) {
      
      if (!is.null(TZ)) {
          
         #timedate <- parse_date(x)
    
      } else { # we assume the user has provided a format we can pass to strptime

     timedate <- as.POSIXct(strptime(x, format=format,tz=TZ))

 }
  

https://www.r-project.org/nosvn/pandoc/parsedate.html


date_ingest_checker <- function(x, precision, TZ) {
    
    # x: some input string we believe to be a time stamp
    # optional arguments: precision, TZ (will be detected automatically if present in the data)

  # Description:
  1. Detemine input format (probably grepl string matching a set of options)
      Accept ISO format - addressed with parse_iso
      Accept 24 hour or 12 hour plus AM/PM #as.POSIXct("2018-06-13 6:10:36 PM", format='%Y-%m-%d %I:%M:%OS %p')

      Accept additional formats as needed? parse_date() but ambiguous decisions
      See https://www.r-project.org/nosvn/pandoc/parsedate.html
  2. Determine precision for output format based on input information

      ! need to think of how this would be interpreted in a situation where a sensor takes a measurement on the hour -- but not necessarily consistently (e.g. telemetry collars set to 1 hour fixes, but depending on # of satellites the collar is able to require, may be +/- sec./min.)

    in cases where some data elements are not as precise as others (e.g., all but a few timestamps appeared to have minutes and seconds, and those that did not occured right on the hour), we will assume that the overall level of precision is the precision of the most precise element; provenance information will be captured for those values which are padded with zeros

  3. If data isn't a valid format output why to user for gathering correct information
  4. Require year, month, and day at a minimum
      Allow users to specify a specific precision, e.g. hour and minutes, or just date that is expected/required
  5. Require unambiguous input timezone (requires legwork to determine, likely not automatable, but may be consisitent over time for a site or type of data)
  ** Provide feedback to user informing them which TZ was recognized & applied
      Additional dataframe with timezones by station/site
      Timezone in input dateTimes
      Single TZ value if all of the data is coming in in the same TZ and is known
      Potentially detect timezone based off of lat/lon? Prefer to get info from data provider/source before this step
  6. Clean input times to be in ISO format
      YYYY-MM-DDTHH:MM:SS.SSSZ
      May involve removing + offset from UTC, adding T, others?
  7. Convert to R dateTime object (POSIXct or POSIXlt?)
  # x <- as.POSIXct(strptime(x, format="%m/%d/%Y %H:%M:%OS",tz="Etc/GMT-7"))

  
  ** Provide feedback to user: timestamp minimum/maximum in UTC and in TZ of input data, if TZ was given in input data
  8. Plot review of earlierst/latest/disctributin of times to the user to check that the dates/times are reasonable (descTools?)
  9. Output dataframe with columns appended for input TZ and input precision (maybe check is they're all the same and output another way? Separate table?)
      TZ is a string value
      Precision options: day, minute, second, milliSecond
      for milliseconds, format needs to be e.g. "%Y-%m-%d %H:%M:%OS" . posixct doesn't display ms but stores them internally (would pad with zeroes if undefined). 

  # Returns:
  
  # Team members:
  
}

timestamp_output_formatter <- function(x, precision, suppOutput) {
    
    # x: properly formatted ISO 8601 string (in UTC) which we receive as output from date_ingest_checker()
    # precision is a required argument for this function; may be automatically detected or specified when date_ingest_checker() is called, but either way we should have value(s) for precison before calling timestamp_output_formatter()
    # suppOutput ("supplemental output"): optional argument allowing user to request timestamp output in "local" or other time zone in addition to UTC; no localities allowed

#Add offset to be applied

switch(precision,

    day = {formatOutISO <- "%Y-%m-%d"},

    minute={formatOutISO <- "%Y-%m-%dT%H:%M"},

    second={formatOutISO <- "%Y-%m-%dT%H:%M:%S"},

    milliSecond={formatOutISO <- "%Y-%m-%dT%H:%M:%OS"})


dateTimeUTC <- format(x, tz = "UTC", format = formatOutISO)
dateTimeSuppOutput <- format(x, tz = suppOutput, format = formatOutISO)

  # Description: 
      1. Additional QA/QC or timestamps updates based on some sort of input/tracking log (see Celeste's code snippet for incidents)
      2. Format date for output in ISO 8601 UTC as text string and user specified "local" or other time with appropriate level of precision from input 
      3. Remove extra columns from dataframe for input timezone and precision and create an output similar to the ingest provenance output

  (fail or fix?).
  
  # Returns:
  
  # Team members: Jamie, Amanda, kaelin

Outstanding/unresolved issues for the date-time working group:
    1. What does format of provenance information look like? Harmonize with whatever format the ingest group is coming up with?
}

sorted_by_timestamp <- function(x) {

  # Description: Check that data are sorted by timestamp
  
  # Returns
  
  # Team members: John

}

#####


#####(Other) dataframe formatting issuesKristin
spaces_checker <- function(x) {

  # Description: Check character data for inadvertant spaces
  
  # Returns:
  
  # Team members:

}

complete_cases_checker <- function(x) {
  
  # Description: row by row check that all value columns 
  
  # Returns:
  
  # Team members: Patrick, Amanda
  
}

col_type_checker <- function(x) {
  
  # Description:

  # Returns:
  
  # Team members: Patrick, Amanda

}

fuzzy_duplicate_checker <- function(x) {

  # Description: checks for near duplicate rows: 1) rows with the same keys but 
  
  different values, 2) rows with the same values but (slightly) different keys, 3)
  
  other cases TBD

  # Returns:
  
  # Team members: Patrick, Amanda, Jamie

}

(consistent coding)
encoding_standard_checker <- function(x) {

  # Description:

  # Returns:
  
  # Team members:
  
}

###Flagging & checks###
NEON example R code for looking at the flags
  outTarn2 <- def.plau(data = data.frame(PARdataUpTarn5$new.data), RngMin = -10, RngMax = 3000, DiffStepMax = 500, NumGap = 4, DiffPersMin = rep.int(0.0001,length(data)),TestNull = T, Vrbs = T)
  str(outTarn2)
  unique(outTarn2$PARdataUpTarn5.new.data$qfRng)
  unique(outTarn2$PARdataUpTarn5.new.data$qfStep)
  unique(outTarn2$PARdataUpTarn5.new.data$qfGap)
  unique(outTarn2$PARdataUpTarn5.new.data$qfNull)
  unique(outTarn2$PARdataUpTarn5.new.data$qfPers)

gap_checker <- function(x) {

  # Description:

  # Returns:
  
  # Team members: Celeste, stevan

}

numberic_step_checker() 
  # Description: Flags time series (e.g., sensor data) for unexpected
  # jumps (increase or decrease) in a single time step. 
  
  # Methods: 
  #  "simpleThreshold": manually set a threshold step value to flag observations
  # that exceed the limit
  #  "movingAverageDeviants": take the diffs based moving average and flag 
  # 'large' changes
  #  "movingMedianDeviants": take the diffs based moving medians and flag 
  # 'large' changes
  
  # Dependencies:
  
  # Inputs: datetime and value
  
  # Returns: original dataframe as 
  
  # Tests:
  
  # Examples:
  
  # Team members:

numeric_range_checker <- function(x) {
  
  # Description:

  # Returns:
  
  # Team members: Celeste

}

acceptable_string_checker <- function(x) {
  
  # Description:

  # Returns:
  
  # Team members:

}

numeric_spike_checker <- function(x) {

  # Description:

  # Returns:
  
  # Team members: Patrick, kaelin

}

numeric_slope_checker <- function(x) {

  # Description:

  # Returns:
  
  # Team members: Celeste Patrick

}

check_timeseries_anomalies <- function(x) {

  # Description:

  # Returns:
  
  # Team members: Patrick

}

check_number_bensford <- function(x) {

  # Description: For numeric values that span multiple orders of magnitude, Benfords Law predicts a distribution rich in low values with few high values for the first digit.  The result applies to an entire series of data values, not to any particular data value. 

  # Returns: T - fits Benford distribution, F - does not fit, NA - data not suitable (not >2 orders of magnitude)
  
  # Team members: John

}

anomaly_outlier_vis_tool <- function(args) {
 
  # Description: 
  
  # Inputs:
  
  # Returns:
  
  # Tests:
      
 # Examples:
  
  # Team members: Patrick
   
    }
