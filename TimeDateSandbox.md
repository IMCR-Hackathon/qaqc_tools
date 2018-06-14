# Notes and brainstorming sandbox for the time and date functions

date_ingest_checker <- function(x, precision, TZ) {
  # x: some input string we believe to be a time stamp
  # optional arguments: precision, TZ (will be detected automatically if present in the data)
  
  # Description:
  1. Detemine input format (probably grepl string matching a set of options)
  Accept ISO format - addressed with parse_iso
  Accept 24 hour or 12 hour plus AM / PM - addressed in tryFormats
  Accept additional formats as needed ? -addressed in tryFormats
  
  2. Determine precision for output format based on input information ## may not be necessary as posixct can handle different levels of precision. only weird thing is with milliseconds!need to think of how this would be interpreted in a situation where a sensor takes a measurement on the hour--but not necessarily consistently (
  e.g. telemetry collars set to 1 hour fixes,
  but depending on # of satellites the collar is able to require, may be +/- sec./min.)
  in cases where some data elements are not as precise as others (
    e.g.,
    all but a few timestamps appeared to have minutes and seconds,
    and those that did not occured right on the hour
  ),
  we will assume that the overall level of precision is the precision of the most precise element
  provenance information will be captured for those values which are padded with zeros
  3. If data isn't a valid format output why to user for gathering correct information
  4. Require year, month, and day at a minimum
  Allow users to specify a specific precision, e.g. hour and minutes, or just date that is expected/required
  5. Require unambiguous input timezone (requires legwork to determine, likely not automatable, but may be consisitent over time for a site or type of data)
  ** Provide feedback to user informing them which TZ was recognized & applied
  Additional dataframe with timezones by station/site
  Timezone in input dateTimes
  Single TZ value if all of the data is coming in in the same TZ and is known
  6. Clean input times to be in ISO format
  YYYY-MM-DDTHH:MM:SS.SSSZ
  format_iso_8601 from parsedate package
  7. Convert to R dateTime object (POSIXct or POSIXlt?) ## Leave as character?
  
  ** Provide feedback to user: timestamp minimum/maximum in UTC and in TZ of input data, if TZ was given in input data
  before we convert to character, can do range(), min, max, etc.
  can also print TZ.
  8. Plot review of earlierst/latest/disctributin of times to the user to check that the dates/times are reasonable (descTools?)
  what kind of data visualizations/summary ouputs would we like for this?
  9. Output dataframe with columns appended for input TZ and input precision (maybe check is they're all the same and output another way ? Separate table ?
  )
TZ is a string value
Precision options:day, minute, second, milliSecond
for milliseconds, format needs to be e.g. "%Y-%m-%d %H:%M:%OS" . posixct doesn't display ms but stores them internally (would pad with zeroes if undefined).

# Returns:
character vector in ISO 8601 format

# Team members:

## Random bench notes from date/time issues::

https://www.r-project.org/nosvn/pandoc/parsedate.html
#did not proceed with parsedate because of TZ issue - might want to make note of this decision somewhere

#tryFormats from posix documentation: http://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
# %T is equivalent to %H:%M:%S
# %F is equivalent to %Y-%m-%d

# there may be value in throwing out an error if date is ambiguous e.g. 10-04-02
# rather than trying to accommodate for every possible date-time format that can be thrown our way (and unspecified)
# do we want to accommodate for names? e.g. June 10 2018


}



# Description:
1. Additional QA / QC or timestamps updates based on some sort of input /
  tracking log (
    see Celeste's code snippet for incidents)
    2. Format date for output in ISO 8601 UTC as text string and user specified "local" or other time with appropriate level of precision from input
    3. Remove extra columns from dataframe for input timezone and precision and create an output similar to the ingest provenance output
    
    (fail or fix?).
    
    # Returns: datetime as text string in ISO 8601 format, with appropriate level of precision
    
    # Team members: Jamie, Amanda, kaelin
    
    Outstanding/unresolved issues for the date-time working group:
    1. What does format of provenance information look like? Harmonize with whatever format the ingest group is coming up with?
    }
    
    
    
    
    sorted_by_timestamp<-function(timeVector,allowTies=TRUE){
    if(mode(timeVector) != "numeric"){
    e<-simpleError(paste("Error: Input must be Date, POSIXct, POSIXlt or numeric.\n Input vector was ",mode(timeVector),sep=''))
    stop(e)
    }
    timeVec1<-tail(timeVector,-1)
    # print(head(timeVector))
    # print(head(timeVec1))
    if(allowTies==FALSE){
    result<-ifelse(head(timeVector,length(timeVector)-1) < timeVec1,0,1)
    }else{
    result<-ifelse(head(timeVector,length(timeVector)-1)<=timeVec1,0,1)
    }
    result<-ifelse(is.na(result),-1,result)
    result<-append(result,-1)
    return(result)
    }