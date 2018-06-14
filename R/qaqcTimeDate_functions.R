# qaqcTimeDate_functions.R

# Purpose: Functions for working with timestamps and dates and times in
# environmental data. Contains functions for ingesting, formatting, and
# detecting/manipulating time zones given different kinds of supplied timestamps

# Dependencies: hasArg from methods

# library(parsedate)
# library(methods)

### handleTimestamp

# Wrapper function for date_ingest_checker() and timestamp_output_formatter()

handleTimestamp <- function (x, Format = NULL, precision = NULL,
                             TZ = NULL, suppOutput = NULL) {

  #   x: some input string we believe to be a time stamp; passed to
  #      date_ingest_checker()
  #   Format: a format string we can pass to strptime, or NULL; if NULL, we will
  #      use the tryformats functionality in as.POSIXct; passed to
  #      date_ingest_checker()
  #   precision: optional argument passed to date_ingest_checker() and then
  #      timestamp_output_formatter()
  #   TZ: optional argument passed to date_ingest_checker(); otherwise time zone
  #      will be assessed from the data itself; if a value is supplied for TZ,
  #      this will be checked against any time zone information present in the
  #      data to harmonize
  #   suppOutput ("supplemental output"): optional argument passed to
  #      request timestamp output in "local" or other time zone/locality in
  #      addition to UTC; must be something in the Olson list

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

### ISOStringPrecformatter

# Creates complete ISO 8601 format string based on degree of time precision and
# desired time zone (choices are either UTC or another, non-UTC time); if no
# precision is supplied, generates format string with seconds as default

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

### addISO8601colon

# Adds a colon to the time offset component of an ISO8601 string that doesn't
# have it

addISO8601colon <- function(ISOstring) {

  # detect whether the string has a colon in it already

  if (grepl('^(.*)[\\+\\-](.{2})\\:(.{2})$', ISOstring)) {

    # appears to be an ISO string with a colon already; just return output

    cat("Your string appears to have a colon in it already; returning it",
        "unaltered.\n")

    ISOstringout <- ISOstring

  } else if (grepl('^(.*)[\\+\\-](.{4})$', ISOstring)) {

    # appears to be an ISO 8601 string having a four digit offset without a
    # colon

    ISOstringout <- gsub('^(.*)(.{2})$', '\\1:\\2', ISOstring)

  } else {

    stop("Input doesn't appear to be a properly formatted ISO 8601 timestamp ",
         "with a numeric time offset component.\n")

  }

  return(ISOstringout)

}

### date_ingest_checker: Workhorse function for initial ingestion and formatting

date_ingest_checker <- function(x, Format = NULL, precision = NULL,
                                TZ = NULL) {

  #   x: some input string that contains the dateTime data [string]
  #   Format: a format string we can pass to strptime, or NULL; if NULL, we will
  #      use the tryformats functionality in as.POSIXct; passed to
  #      date_ingest_checker()
  #   precision: optional argument that can have values of "day", "minute",
  #      "second", or "milliSecond" for the input and output; if not specified
  #      [string]
  #   TZ: optional argument specifying the timezone in which the dateTime data
  #      was collected; must be in the OlsonNames() list [string]; if not
  #      specified, uses whatever is given in the format string for %z or %Z

  # date_ingest_checker returns a timestamp as POSIXct format

  ### Initial checks on formatting of the input string and the timezone

  # remembered that sometimes when reading in Excel files the date format is an
  # integer; so, perform some sort of integer check to see if it is coming in as
  # a number instead of a date (and time)

  if (!is.na(suppressWarnings(as.numeric(x))) ||
      (is.na(suppressWarnings(as.numeric(x))) &&
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
  if (grepl("%e|%t", Format)) {
    stop("This function does not currently accept formats containing ",
         "arbitrary whitespace on input, %n and %t\n")
  }

  # ensure input x is a character vector
  x <- as.character(x)

  # perform a series of additional formatting checks if there is a string format
  # timezone appended to the input dateTime data
  if (grepl("%Z$", Format)) {

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
    locTimeZone <- gregexpr(regexTz, x)

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

  } else if (is.null(TZ)) {

    stop("Error, no timezone input and no timezone could be parsed from data.")

  } else {

    if (grepl("[A-Za-z]",x)&&!grepl("T",x)) {

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

  ### Generate, return function output as POSIXct

  return(timedate)

}

### timestamp_output_formatter

# Takes a POSIXct object, combines with precision argument (if supplied),
# and returns an ISO 8601 text string that has the appropriate level of
# precision. Optionally, also returns the same datetime for a "alternate" time
# zone other than UTC; this would allow user to easily evaluate whether output
# makes sense based on their knowledge of the input data.

# We envision that this function could be incorporated into a wrapper function
# to handle output received from date_ingest_checker()

timestamp_output_formatter <- function(x, precision = NULL,
                                       suppOutput = NULL) {

  #   x: a POSIXct object; in our concept, this would
  #      ideally be the output from date_ingest_checker(); however, the function
  #      is designed to operate by itself with user-supplied arguments
  #   precision: optional argument, may be passed along from the value user
  #      supplied for the precision argument in date_ingest_checker() or the
  #      wrapper function (once we build it); if not supplied, we will report
  #      to a precision of seconds (consistent with ISO 8601 specification)
  #  suppOutput ("supplemental output"): optional argument allowing user to
  #      request timestamp output in "local" or other time zone/locality in
  #      addition to UTC; must be something in the Olson list

  ### Check arguments

  if (!is.null(suppOutput)) {

    if (!(suppOutput %in% OlsonNames())) {

      stop("The time zone or locality supplied for suppOutput must be one of ",
           "the values present in the R OlsonNames() list. Try OlsonNames() ",
           "at the R console prompt to view a full list of the valid options.")

    }

  }

  if (!is(x, "POSIXct")) {

    stop("Input must be an object of class 'POSIXct'\n")

  }

  ### Generate the output

  # generate the UTC timestamp; store in list object
  
  attributes(x)$tzone <- "UTC" # convert time to UTC
  dateTimeUTC <- format(x, format = ISOStringPrecformatter(precision = precision,
                                                        desiredZone = c("UTC")))

  outList <- list("dateTimeUTC" = dateTimeUTC) # create our output list object

  # generate the supplementary output, if requested
  if (!is.null(suppOutput)) {
    
    attributes(x)$tzone <- suppOutput # convert time to whatever TZ user wants
    dateTimeSuppOutput <- format(x, format = ISOStringPrecformatter(precision = precision,
                                                          desiredZone = c("other")))

    # need to insert the colon in the right place in the optional-timezone output
    # string to make it ISO 8601 compliant

    dateTimeSuppOutput <- addISO8601colon(dateTimeSuppOutput)

    # append the supplemental timestamp to the output list
    outList$dateTimeSuppOutput <- dateTimeSuppOutput

  }

  # return output
  return(outList)

}
