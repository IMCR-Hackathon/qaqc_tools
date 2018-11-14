##############################################################################################
#' @title Checks timestamp inputs and converts to POSIXct R object

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Jamie Collins \email{jamesrco@uw.edu} \cr
#' Amanda Droghini \email{adroghini@alaska.edu} \cr

#' @description Function to read in a timestamp and format as POSICct in R to help avoid 
#' timestamp problems with data QAQC, e.g. daylight savings time changes

#' @param x some input string that contains the dateTime data [character]
#' @param Format a format string we can pass to strptime, or NULL; if NULL, we will use 
#' the tryformats functionality in as.POSIXct; passed to date_ingest_checker() [character]
#' @param precision optional argument that can have values of "day", "minute", "second", 
#' or "milliSecond" for the input and output; if not specified [character]
#' @param TZ optional argument specifying the timezone in which the dateTime data was 
#' collected;must be in the OlsonNames list; if not specified, uses whatever is given 
#' in the format string for \%z or \%Z [character]
#' @param check User input of TRUE or FALSE of whether or not to check timestamp against a 
#' range of valid values [boolean]
#' @param checkMin User input of minimum valid dateTime range, default 1980-01-01 [POSIXct]
#' @param checkMax User input of maximum valid dateTime range, default 
#' system time + 24 hours [POSIXct]

#' @return a timestamp as POSIXct format [POSIXct]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords datetime, date, time, timestamp

#' @examples
#' date_asPOSIXct <- date_ingest_checker("2018-01-01T13:30","%Y-%m-%dT%H:%M","minute","MDT")

#' @seealso addISO8601colon.R, handleTimestamp.R, ISOStringPrecformatter.R, and 
#' timestamp_output_formatter.R related functions for formatting dates in ISO standard format

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley, Jamie Collins, Amanda Droghini (2018-06-11)
#     original creation @ IMCR Hackathon in New Mexico
#   Kaelin M. Cawley (2018-11-14)
#     updates to formatting and examples @ IMCR virtual hackathon
###############################################################################################
date_ingest_checker <- function(x, 
                                Format = NULL, 
                                precision = NULL,
                                TZ = NULL,
                                check = TRUE,
                                checkMin = as.POSIXct("1980-01-01"),
                                checkMax = as.POSIXct(Sys.time() + 86400)) {


  ### Initial checks on formatting of the input string and the timezone

  # remembered that sometimes when reading in Excel files the date format is an
  # integer; so, perform some sort of integer check to see if it is coming in as
  # a number instead of a date (and time)

  if (!is.na(suppressWarnings(as.numeric(x))) ||
      (is.na(suppressWarnings(as.numeric(x))) && !is.null(Format) &&
       (Format == "%Y%m%d" || Format == "%y%m%d"))) {

    stop("The input date is numeric, please re-format as date and time ",
         "before trying again. Valid numeric formats are '%Y%m%d' or ",
         "'%y%m%d'\n")

  }

  # check for timezone appended to dateTime data
  alreadyInUTC <- grepl("Z$", x) # yields true or false

  if (alreadyInUTC) {

    if (!is.null(TZ) && (!grepl("GMT$|UTC$",TZ)) &&
        (grepl("GMT[+-][0]*[:]*[0-9][1-9]",TZ))) {

      stop("Format string indicates UTC timezone, but value supplied for ",
           "function argument 'TZ' is not UTC or GMT.\n")

    } else {

      TZ <- "UTC"

    }

  }

  # for the moment the function won't accept arbitrary whitespace on input
  if (!is.null(Format) && grepl("%e|%t", Format)) {
    stop("This function does not currently accept formats containing ",
         "arbitrary whitespace on input, %n and %t\n")
  }

  # ensure input x is a character vector
  x <- as.character(x)

  # perform a series of additional formatting checks if there is a string format
  # timezone appended to the input dateTime data
  if (!is.null(Format) && grepl("%Z$", Format)) {

    cat("Format indicates that a timezone is part of the input dateTime data.",
        " Attempting to detect time zone from supplied data...\n")

    # parse timezone from the end of the data and format
    locZ <- unlist(gregexpr("%Z", Format))
    beforeZ <- substr(Format, (locZ - 1), (locZ - 1))

    # first, a check since some of the formats can contain whitespace
    if (beforeZ != " ") {

      stop("This function currently doesn't handle string time zones that ",
           "are not preceded by a space. Submit  a request for a feature ",
           "update or reformat your data.\n")

    }

    afterZ <- substr(Format, (locZ + 2), (locZ + 2))

    if (afterZ == "") {

      afterZ <- "$"

    }

    regexTZ <- paste0(beforeZ, "[A-Za-z0-9+-\\/]*", afterZ)
    locTimeZone <- gregexpr(regexTZ, x)

    if (length(locTimeZone)) {

      stop("Unique timezone could not be parsed from the input data and ",
           "format.\n")

    }

    timeZone <-
      substr(x, unlist(locTimeZone), (unlist(locTimeZone) +
                                        attr(locTimeZone[[1]],
                                             "match.length")))

    # clean leading and trailing whitespace from the parsed timeZone
    timeZone <- trimws(timeZone)

    # check that the parsed timezone from the data matches the input TZ
    # argument if there is one
    if (!is.null(TZ) && TZ != timeZone) {

      stop("Parsed timezone and input argument TZ do not match. Correct ",
           "input or data to match before trying again.\n")

    }

    # set the TZ to timeZone if there wasn't one specified in the input
    TZ = timeZone

  } else if (is.null(TZ) && precision!="day") {

    stop("Error, no timezone input and no timezone could be parsed from data.")

  } else if (is.null(TZ) && precision=="day") {
    
    TZ = "UTC"
    cat("Warning, a timezone of UTC will be used for dates without times. Specify a timezone using the TZ input parameter to choose a different timezone.\n")
    
  } else {

    if (grepl("[A-Za-z]",x)&&!grepl("T|AM|PM",x)) {

      stop("Format does not include a string other than ISO 'T' in date ",
           "format found. Cannot proceed with date string containing ",
           "alphabetic characters that aren't part of a timezone.\n")

    }

  }

  ### Extract & read components of the timestamp

  # clean up timezones that are GMT+##:## or GMT+##, etc.
  # remove all 0 and ":"
  TZ <- gsub("0|:","",TZ)
  # remove a remaining + or - if the offset was all zeros
  TZ <- gsub("\\+$|\\-$","",TZ)

  # if the Timezone is one of the GMT ones that has a non-zero offset add
  # Etc/ before the GMT part

  if (grepl("GMT\\+[0-9]|GMT\\-[0-9]",TZ)) {

    TZ <- paste0("Etc/",TZ)

  }

  # if the Timezone is "MDT", "CDT", "EDT", or "PDT"

  switch(TZ,
         "EDT"={TZ = "EST5EDT"},
         "CDT"={TZ = "CST6CDT"},
         "MDT"={TZ = "MST7MDT"},
         "PDT"={TZ = "PST8PDT"})

  if (grepl("EDT|CDT|MDT|PDT",TZ)) {

    warning(paste0("The timezone you entered is daylight savings time and was ",
                   "converted to TZ. Consider whether the US/locale that ",
                   "accounts for standard and daylight time would be more ",
                   "appropriate.\n"))

  }

  # check that the parsed timezone is in the Olson list

  if (!(TZ%in%OlsonNames())) {

    stop("Parsed timezone is not valid according to the R OlsonNames() list.")

  }

  ### Provide user with some additional warnings and messages

  # check for precision input

  if (is.null(precision)) {

    cat("User did not specify a timestamp precision. Timestamp ",
        "precision will be detected from supplied data...\n")

  }

  ### Now, do the actual reading in of the timestamp

  # Two cases: User did not supply a format at all; or, user supplied a
  # formatting string

  if (is.null(Format)) {

    # Case 1: User did not supply any formatting information; attempt to
    # read in using tryFormats argument to as.POSIXct()

    warning("User did not specify a date-time format. Attempting to",
            "detect format from supplied data using tryFormats in the function ",
            "as.POSIXct...\n")

    timedate <- as.POSIXct(x, tz = TZ,
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

  } else {

    # Case 2: User has provided a format we can pass to as.POSIXct
    timedate <- as.POSIXct(x,tz = TZ, format = Format)

  }
  
  #Check date object is within common sense range check
  if (check) {
    
    if(timedate<checkMin){
      
      stop(paste0("Date and/or time earlier than ",checkMin,"\n"))
      
    } else if (timedate>checkMax){
      
      stop(paste0("Date and/or time later than ",checkMax,"\n"))
      
    }
    
  }
  
  ### Generate, return function output as POSIXct
  return(timedate)

}

