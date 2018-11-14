##############################################################################################
#' @title Adds a colon to the time offset component of an ISO8601 string

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Jamie Collins \email{jamesrco@uw.edu} \cr
#' Amanda Droghini \email{adroghini@alaska.edu} \cr

#' @description Adds a colon to the time offset component of an ISO8601 string that 
#' doesn't already have it

#' @param ISOstring ISO date in string format that may or may not include a colon [character]

#' @return ISO 8601 format string based on degree of time precision and desired time zone 
#' (choices are either UTC or another, non-UTC time) [character]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords datetime, date, time, timestamp

#' @examples
#' ISO_string_out <- addISO8601colon("%Y-%m-%dT%H:%M+0600")

#' @seealso date_ingest_checker.R, handleTimestamp.R, ISOStringPrecformatter.R, and 
#' timestamp_output_formatter.R related functions for formatting dates in ISO standard format

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley, Jamie Collins, Amanda Droghini (2018-06-11)
#     original creation @ IMCR Hackathon in New Mexico
#   Kaelin M. Cawley (2018-11-14)
#     updates to formatting and examples @ IMCR virtual hackathon
###############################################################################################
addISO8601colon <- function(ISOstring) {

  # detect whether the string has a colon in it already
  if (grepl('^(.*)[\\+\\-](.{2})\\:(.{2})$', ISOstring)) {

    # appears to be an ISO string with a colon already; just return output
    cat("Your string appears to have a colon in it already; returning it unaltered.\n")
    ISOstringout <- ISOstring

  } else if (grepl('^(.*)[\\+\\-](.{4})$', ISOstring)) {

    # appears to be an ISO 8601 string having a four digit offset without a colon
    ISOstringout <- gsub('^(.*)(.{2})$', '\\1:\\2', ISOstring)

  } else {

    stop("Input doesn't appear to be a properly formatted ISO 8601 timestamp with a numeric time offset component.\n")

  }

  return(ISOstringout)

}

##############################################################################################
#' @title Wrapper function for date_ingest_checker() and timestamp_output_formatter()

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Jamie Collins \email{jamesrco@uw.edu} \cr
#' Amanda Droghini \email{adroghini@alaska.edu} \cr

#' @description Wrapper function for date_ingest_checker() and timestamp_output_formatter()

#' @param x some input string we believe to be a time stamp; passed to 
#' date_ingest_checker() [character]
#' @param Format a format string we can pass to strptime, or NULL; if NULL, we will use 
#' the tryformats functionality in as.POSIXct; passed to date_ingest_checker() [character]
#' @param precision optional argument passed to date_ingest_checker() and then 
#' timestamp_output_formatter() [character]
#' @param TZ optional argument passed to date_ingest_checker(); otherwise time zone will be 
#' assessed from the data itself; if a value is supplied for TZ, this will be checked against 
#' any time zone information present in the data to harmonize [character]
#' @param suppOutput ("supplemental output"): optional argument passed to request timestamp 
#' output in "local" or other time zone/locality in addition to UTC; must be something in 
#' the Olson list [character]

#' @return ISO 8601 format string based on degree of time precision and desired time zone 
#' (choices are either UTC or another, non-UTC time) [character]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords datetime, date, time, timestamp

#' @examples
#' precISODateTime <- handleTimestamp("2018-01-01T13:30","%Y-%m-%dT%H:%M","minute","MDT","UTC")

#' @seealso date_ingest_checker.R, addISO8601colon.R, ISOStringPrecformatter.R, and 
#' timestamp_output_formatter.R related functions for formatting dates in ISO standard format

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley, Jamie Collins, Amanda Droghini (2018-06-11)
#     original creation @ IMCR Hackathon in New Mexico
#   Kaelin M. Cawley (2018-11-14)
#     updates to formatting and examples @ IMCR virtual hackathon
###############################################################################################
handleTimestamp <- function (x, 
                             Format = NULL, 
                             precision = NULL,
                             TZ = NULL, 
                             suppOutput = NULL) {
  
  # first, quick argument check for the precision argument
  if (!is.null(precision)) {
    
    precision <- match.arg(precision,
                           c("day", "minute", "second", "milliSecond"),
                           several.ok = FALSE)
    
  }
  
  initISODateTime <- date_ingest_checker(x, Format, precision, TZ)
  precISODateTime <- timestamp_output_formatter(initISODateTime,
                                                precision, suppOutput)
  return(precISODateTime)
  
}

##############################################################################################
#' @title Creates a format string for a desired ISO format precision

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Jamie Collins \email{jamesrco@uw.edu} \cr
#' Amanda Droghini \email{adroghini@alaska.edu} \cr

#' @description Creates complete ISO 8601 format string based on degree of time precision and 
#' desired time zone (choices are either UTC or another, non-UTC time); if no precision is 
#' supplied, generates format string with seconds as default

#' @param precision argument that can have values of "day", "minute", "second", 
#' or "milliSecond" [character]
#' @param desiredZone Desired output timezone [character]

#' @return ISO 8601 format string based on degree of time precision and desired time zone 
#' (choices are either UTC or another, non-UTC time) [character]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords datetime, date, time, timestamp

#' @examples
#' formatOutISO_complete <- ISOStringPrecformatter("minute","UTC")

#' @seealso date_ingest_checker.R, addISO8601colon.R, handleTimestamp.R, and 
#' timestamp_output_formatter.R related functions for formatting dates in ISO standard format

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley, Jamie Collins, Amanda Droghini (2018-06-11)
#     original creation @ IMCR Hackathon in New Mexico
#   Kaelin M. Cawley (2018-11-14)
#     updates to formatting and examples @ IMCR virtual hackathon
###############################################################################################
ISOStringPrecformatter <- function(precision = NULL,
                                   desiredZone = c("UTC","other")) {
  
  # check for desiredZone argument
  
  if (!methods::hasArg(desiredZone) || (is.null(desiredZone))) {
    
    stop("Must specify a desiredZone to generate the right format string.\n")
    
  } else {
    
    desiredZone <- match.arg(desiredZone,
                             c("UTC", "other"))
    
  }
  
  # create base format string
  
  if (is.null(precision)) {
    
    formatOutISO_base <- "%Y-%m-%dT%H:%M:%S"
    
  } else {
    
    precision <- match.arg(precision,
                           c("day", "minute", "second", "milliSecond"),
                           several.ok = FALSE)
    
    switch(precision,
           day = {formatOutISO_base <- "%Y-%m-%d"},
           minute = {formatOutISO_base <- "%Y-%m-%dT%H:%M"},
           second = {formatOutISO_base <- "%Y-%m-%dT%H:%M:%S"},
           milliSecond = {formatOutISO_base <- "%Y-%m-%dT%H:%M:%OS3"}
    )
    
  }
  
  # generate secondary time offset format string that will be appended to
  # the main format string generated above; ISO 8601 does not require the offset
  # designator if only Y-m-d is given
  
  # have to use as.POSIXlt rather than as.POSIXct for this task so that time zone
  # conversion works properly
  
  if (formatOutISO_base == "%Y-%m-%d") {
    
    formatOutISO_complete <- "%Y-%m-%d"
    
  } else {
    
    if (desiredZone == "UTC") {
      
      formatOutISO_complete <- paste0(formatOutISO_base, "+00:00")
      
    } else if (desiredZone == "other") {
      
      formatOutISO_complete <- paste0(formatOutISO_base, "%z")
      
    }
    
  }
  
  # return the format string
  
  return(formatOutISO_complete)
  
}

