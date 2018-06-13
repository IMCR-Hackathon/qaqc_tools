##Date/Time Issues

# an initial crack at the date_ingest_checker function

# function dependencies:
# parse_iso_8601() functions from package parsedate
# some functions such as %nin% in Hmisc

library(parsedate)
library(Hmisc)

# arguments:
#   x: some input string we believe to be a time stamp
#   format: either "ISO8601", a format string we can pass to strptime, or NULL;
#   if NULL, we will use the tryformats functionality in as.POSIXct
#   optional arguments: precision, TZ (will be detected automatically if present
#   in the data)

date_ingest_checker <-
  function(x,
           format = c("ISO8601"),
           precision = NULL,
           TZ = NULL)
    ) {
      #Would someone ever enter YYYYMMDD (%Y%m%d would be ok) or YYMMDD (%y%m%d) with no "-" or "/", but it wouldn't be an excel format
      
      #Just remembered that sometimes when reading in excel files the date format is an integer
      #Some sort of integer check to see if it is coming in as a number instead of a date (and time)
      if (!is.na(as.numeric(x)) ||
          (is.na(as.numeric(x)) && (format == "%Y%m%d" ||
                                    format == "%y%m%d"))) {
        stop(
          'The input date is numeric, please re-format as date and time before trying again. Valid numeric formats are "%Y%m%d" or "%y%m%d"'
        )
      }
      
      #For the moment the function won't accept arbitrary whitespace on input
      if (grepl("%e|%t", format)) {
        stop(
          "This function does not currently accept formats containing arbitrary whitespace on input, %n and %t"
        )
      }
      
      # ensure input x is a character vector
      x <- as.character(x)
      
      #If the timezone is appended as a character at the end of the format (%Z), parse it out and reformat the data and format
      numSpacesFmt <- unlist(gregexpr(" ", format))
      numSpacesX <- unlist(gregexpr(" ", x))
      
      #Some of the formats can contain whitespace:
      #%e Day of the month as decimal number (1–31), with a leading space for a single-digit number.
      #%n Newline on output, arbitrary whitespace on input.
      %t Tab on output, arbitrary whitespace on input.
      
      if (grepl("%Z$", format)) {
        #Parse timezone from the end of the data and format
        locZ <- unlist(gregexpr("%Z", format))
        beforeZ <- substr(format, (locZ - 1), (locZ - 1))
        if (beforeZ != " ") {
          stop(
            "This function currently doesn't handle string time zones that are not preceded by a space. Submit  a request for a feature update or reformat your data"
          )
        }
        afterZ <- substr(format, (locZ + 2), (locZ + 2))
        if (afterZ == "") {
          afterZ <- "$"
        }
        regexTZ <- paste0(beforeZ, "[A-Za-z0-9+-\\/]*", afterZ)
        locTimeZone <- gregexpr(regexTz, x)
        if (length(locTimeZone)) {
          stop("Unique timezone could not be parsed from the input data and format")
        }
        timeZone <-
          substr(x, unlist(locTimeZone), (unlist(locTimeZone) + attr(locTimeZone[[1]], "match.length")))
        #Clean leading and trailing whitespace from the parsed timeZone
        timeZone <- trimws(timeZone)
        
        #Move this check down so that is verifies the TZ as well as the timeZone
        #Check that the parsed timezone is in the Olson list
        if (timeZone %nin% OlsonNames()) {
          stop("Parsed timezone is not valid according to OlsonNames()")
        }
        #Check that the parsed timezone matches the input TZ if there is one
        if (!is.null(TZ) && TZ != timeZone) {
          stop(
            "Parsed timezone and input argument TZ do not match. Correct input or data to match before trying again."
          )
        }
        #Set the TZ to timeZone if there wasn't one specified in the input
        TZ = timeZone
      }
      
      #Ok, really just need to look for three parts to the dateTime
      #1, date part can contain "-" or "/", must be present
      #2, time part contains ":", starts with "T" or " ", may or may not be there
      #3, timezone may or may not be there, starts with a "+" "-" or " " - at this point in the code we know what the timezone is
      
      #Check that the time is after the date using a colon to indicate time and a space or T to indicate the character between the date and time
      firstColon <- min(unlist(gregexpr(":", x)))
      firstSpaceOrT <- min(unlist(gregexpr("[0-9]T[0-9]| ", x)))
      
      if (firstColon < firstSpaceOrT) {
        #The time is before the date
        timePart <- substr(x, 1, (firstSpaceOrT - 1))
      }
      
      #Add T in nearly ISO format, test format to know where spaces are
      numSpaces <- unlist(gregexpr(" ", format))
      if (length(numSpaces) > 0) {
        #The first space should always be between the date and time
        firstSpaceIdx <- numSpaces[1]
        substr(x, fistSpaceIdx, firstSpaceIdx) <- "T"
        substr(format, fistSpaceIdx, firstSpaceIdx) <- "T"
        #Add something here for provenance where we record that the T was added to the date string and input format
      }
      
      # provide user with warnings and messages
      
      #Check for timezone appended to dateTime data
      
      alreadyInUTC <- grepl("Z$", x) #True or False
      
      if (!alreadyInUTC) {
        cat(
          "User did not specify a time zone. Attempting to detect time",
          "zone from supplied data...\n"
        )
        
        TZ <- gsub(, "")
      }
      
      if (TZ %nin% OlsonNames())
        
        # check for precision input
        
        if (is.null(precision)) {
          cat(
            "User did not specify a timestamp precision. Timestamp precision will be",
            "detected from supplied data...\n"
          )
          
        }
      
      # read in the raw datetime based on format
      
      
      
    } if (is.null(format)) {
      warning(
        "User did not specify a date-time format. Attempting to",
        "detect format from supplied data using tryFormats in the function as.POSIXct...\n"
      )
      
      timedate <- as.POSIXct(
        x,
        tz = TZ,
        tryFormats = c(
          "%F %T",
          "%Y/%m/%d %T",
          "%Y-%m-%d %H:%M:%OS",
          # for milliseconds
          "%Y/%m/%d %H:%M:%OS",
          # for milliseconds
          "%Y-%m-%d %H:%M",
          "%Y/%m/%d %H:%M",
          "%Y-%m-%d %I:%M:%OS %p",
          # for AM/PM
          "%Y/%m/%d %I:%M:%OS %p",
          "%Y-%m-%d",
          "%Y/%m/%d"
        )
      )
      
    } else if (format == "ISO8601") {
      # use parse_iso_8601() from parsedate
      
      timedate <-
        parse_iso_8601(x) # returns date in UTC ##probably redundant with format_iso_8601
      
    } else {
      # we assume the user has provided a format we can pass to strptime
      
      timedate <- as.POSIXct(x, tz = TZ, format = format)
      
    }

# final output as an ISO character
timedateISO <-
  format_iso_8601(timedate) #note this returns +00:00 not Z

## if converting back to character for millisecond will need to specify # of decimal places for MS e.g.
## format(y, "%Y-%m-%d %H:%M:%OS6")

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

library(parsedate)

timestamp_output_formatter <- function(x, precision = NULL, suppOutput = NULL) {

# x: properly formatted ISO 8601 string (in UTC) which we receive as output from date_ingest_checker()
# precision: optional argument, may be passed along from the value user supplied for the precision argument in date_ingest_checker(); if not supplied, we will report to a precision of seconds (consistent with ISO 8601 specification)
# suppOutput ("supplemental output"): optional argument allowing user to request timestamp output in "local" or other time zone in addition to UTC; no localities allowed

# first, convert the ISO 8601 string to an R datetime object
# shouldn't need to do any format - checking since we assume this is a correctly formatted string received directly from the date_ingest_checker function

ISOdate <- parse_iso_8601(x)

if (!is.null(precision)) {
  precision <-
    match.arg(precision,
              c("day", "minute", "second", "milliSecond"),
              several.ok = FALSE)
  
  # create format string for level of precision to be applied, based on function argument
  
  switch(
    precision,
    day = {
      formatOutISO <- "%Y-%m-%d"
    },
    minute = {
      formatOutISO <- "%Y-%m-%dT%H:%M"
    },
    second = {
      formatOutISO <- "%Y-%m-%dT%H:%M:%S"
    },
    milliSecond = {
      formatOutISO <- "%Y-%m-%dT%H:%M:%OS"
    }
  )
  
} else {
  # just go out to seconds, consistent with ISO 8601 specification
  
  formatOutISO <- "%Y-%m-%dT%H:%M:%S"
  
}

# produce our string(s) for output
# have to use as.POSIXlt rather than as.POSIXct for this task so that time zone conversion works properly

# first, make sure the offset time string is correct; ISO 8601 does not require the offset designator if only Y-m-d is given
if (formatOutISO == "%Y-%m-%d") {
  UTC_offsetString <- ""
  suppOutput_offsetString <- ""
  
} else {
  UTC_offsetString <- "+00:00"
  suppOutput_offsetString <- "%z"
  
}

dateTimeUTC <-
  format(as.POSIXlt(x, tz = "UTC"),
         format = paste0(formatOutISO, UTC_offsetString))

outList <-
  list("dateTimeUTC" = dateTimeUTC) # create our output list object

if (!is.null(suppOutput)) {
  dateTimeSuppOutput <-
    format(as.POSIXlt(x, tz = suppOutput),
           format = paste0(formatOutISO, suppOutput_offsetString))
  
  # need to insert the colon in the right place in the optional-timezone output string to make it ISO 8601 compliant
  dateTimeSuppOutput <-
    gsub('^(.*)(.{2})$', '\\1:\\2', dateTimeSuppOutput)
  
  outList$dateTimeSuppOutput <-
    dateTimeSuppOutput # append the supplemental timestamp to the output list
  
}

# return output
return(outList)

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