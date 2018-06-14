# qaqcTimeDate_functions.R

# Purpose: Functions for working with timestamps and dates and times in
# environmental data. Contains functions for ingesting, formatting, and
# detecting/manipulating time zones given different kinds of supplied timestamps

# Dependencies: parse_iso_8601(), format_iso_8601() functions from package
# parsedate

# library(parsedate)

### handleTimestamp

# Wrapper function for date_ingest_checker() and timestamp_output_formatter()

handleTimestamp <- function (x, format = c("ISO8601"), precision = NULL,
                             TZ = NULL, suppOutput = NULL) {
  
  #   x: some input string we believe to be a time stamp; passed to
  #      date_ingest_checker()
  #   format: either "ISO8601", a format string we can pass to strptime, or
  #      NULL; if NULL, we will use the tryformats functionality in
  #      as.POSIXct; passed to date_ingest_checker()
  #   precision: optional argument passed to date_ingest_checker() and then
  #      timestamp_output_formatter()
  #   TZ: optional argument passed to date_ingest_checker(); otherwise time zone
  #      will be assessed from the data itself; if a value is supplied for TZ,
  #      this will be checked against any time zone information present in the
  #      data to harmonize
  #   suppOutput ("supplemental output"): optional argument passed to 
  #      request timestamp output in "local" or other time zone/locality in
  #      addition to UTC; must be something in the Olson list
  
  # quick argument check for the precision argument
  
  if (!is.null(precision)) {
    
    precision <- match.arg(precision,
                           c("day", "minute", "second", "milliSecond"),
                           several.ok = FALSE)
    
  }
  
}

### date_ingest_checker: Workhorse function for initial ingestion and formatting

date_ingest_checker <- function(x, format = c("ISO8601"), precision = NULL,
                                TZ = NULL) {

  #   x: some input string we believe to be a time stamp
  #   format: either "ISO8601", a format string we can pass to strptime, or
  #      NULL; if NULL, we will use the tryformats functionality in
  #      as.POSIXct
  #   optional arguments: precision, TZ (can be detected automatically from
  #      the data)

  ## *** Would someone ever enter YYYYMMDD (%Y%m%d would be ok) or YYMMDD
  ## (%y%m%d) with no "-" or "/", but it wouldn't be an excel format ***

  ### Initial checks on formatting of the input string and the timezone

  # remembered that sometimes when reading in Excel files the date format is an
  # integer; so, perform some sort of integer check to see if it is coming in as
  # a number instead of a date (and time)

  if (!is.na(as.numeric(x)) ||
      (is.na(as.numeric(x)) && (format == "%Y%m%d" ||
                                format == "%y%m%d"))) {
    stop("The input date is numeric, please re-format as date and time ",
         "before trying again. Valid numeric formats are '%Y%m%d' or ",
         "'%y%m%d'")

  }

  # for the moment the function won't accept arbitrary whitespace on input

  if (grepl("%e|%t", format)) {

    stop("This function does not currently accept formats containing ",
         "arbitrary whitespace on input, %n and %t")

  }

  # ensure input x is a character vector

  x <- as.character(x)

  # if the timezone is appended as a character at the end of the format
  # (%Z), parse it out and reformat the data and format

  numSpacesFmt <- unlist(gregexpr(" ", format))
  numSpacesX <- unlist(gregexpr(" ", x))

  # perform a series of additional formatting checks

  if (grepl("%Z$", format)) {

    # parse timezone from the end of the data and format
    locZ <- unlist(gregexpr("%Z", format))
    beforeZ <- substr(format, (locZ - 1), (locZ - 1))

    # first, a check since some of the formats can contain whitespace, e.g.,
    # %e: Day of the month as decimal number (1–31), with a leading space
    # for a single-digit number
    # %n: Newline on output, arbitrary whitespace on input
    # %t: Tab on output, arbitrary whitespace on input

    if (beforeZ != " ") {

      stop("This function currently doesn't handle string time zones that ",
           "are not preceded by a space. Submit  a request for a feature ",
           "update or reformat your data.")

    }

    afterZ <- substr(format, (locZ + 2), (locZ + 2))

    if (afterZ == "") {

      afterZ <- "$"

    }

    regexTZ <- paste0(beforeZ, "[A-Za-z0-9+-\\/]*", afterZ)
    locTimeZone <- gregexpr(regexTz, x)

    if (length(locTimeZone)) {

      stop("Unique timezone could not be parsed from the input data and ",
           "format.")

    }

    timeZone <-
      substr(x, unlist(locTimeZone), (unlist(locTimeZone) +
                                        attr(locTimeZone[[1]],
                                             "match.length")))

    # clean leading and trailing whitespace from the parsed timeZone
    timeZone <- trimws(timeZone)

    # move this check down so that is verifies the TZ as well as the
    # timeZone; check that the parsed timezone is in the Olson list

    if (!(timeZone %in% OlsonNames())) {

      stop("Parsed timezone is not valid according to OlsonNames()")

    }

    # check that the parsed timezone from the data matches the input TZ
    # argument if there is one

    if (!is.null(TZ) && TZ != timeZone) {

      stop("Parsed timezone and input argument TZ do not match. Correct ",
           "input or data to match before trying again.")

    }

    # set the TZ to timeZone if there wasn't one specified in the input
    TZ = timeZone

  }

  ### Extract & read components of the timestamp

  # really just need to look for three parts to the dateTime:

  # 1. date part can contain "-" or "/", must be present
  # 2. time part contains ":", starts with "T" or " ", may or may not be there
  # 3. timezone may or may not be there, starts with a "+" "-" or " " - at this
  #    point in the code we know what the timezone is

  # check that the time is after the date using a colon to indicate time and a
  # space or T to indicate the character between the date and time

  firstColon <- min(unlist(gregexpr(":", x)))
  firstSpaceOrT <- min(unlist(gregexpr("[0-9]T[0-9]| ", x)))

  if (firstColon < firstSpaceOrT) {

    # the time is before the date
    timePart <- substr(x, 1, (firstSpaceOrT - 1))

  }

  # add T in nearly ISO format, test format to know where spaces are
  numSpaces <- unlist(gregexpr(" ", format))

  if (length(numSpaces) > 0) {

    # the first space should always be between the date and time
    firstSpaceIdx <- numSpaces[1]
    substr(x, fistSpaceIdx, firstSpaceIdx) <- "T"
    substr(format, fistSpaceIdx, firstSpaceIdx) <- "T"

    ## *** add something here for provenance where we record that the T was
    ## added to the date string and input format ***

  }

  ### Provide user with some additional warnings and messages

  # check for timezone appended to dateTime data

  alreadyInUTC <- grepl("Z$", x) # yields true or false

  if (!alreadyInUTC) {

    cat("User did not specify a time zone. Attempting to detect time ",
        "zone from supplied data...\n")

    TZ <- gsub(, "")

  }

    # check for precision input

    if (is.null(precision)) {

      cat("User did not specify a timestamp precision. Timestamp ",
          "precision will be detected from supplied data...\n")

    }

  ### Now, do the actual reading in of the timestamp

  # Three cases: User did not supply a format at all; user indicated
  # that the timestamp was ISO 8601; or, user supplied a formatting string

  if (is.null(format)) {

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

  } else if (format == "ISO8601") {

    # Case 2: User indicated timestamp was ISO 8601; will use parse_iso_8601()
    # from parsedate as a check (e.g. in case T was missing)
    # then reformat to ISO format

    timedate <- parsedate::parse_iso_8601(x) # returns date in UTC

  } else {

    # Case 3: User has provided a format we can pass to as.POSIXct

    timedate <- as.POSIXct(x, tz = TZ, format = format)

  }

  ### Generate, return function output

  # final output as an ISO character string

  timedateISO <- parsedate::format_iso_8601(timedate) # note this returns +00:00
                                                      # not Z

  ## *** if converting back to character for millisecond will need to specify #
  ## of decimal places for MS e.g. format(y, "%Y-%m-%d %H:%M:%OS6") ***

  return(timedateISO)

}

### timestamp_output_formatter

# Takes an ISO 8601 string in UTC, combines with precision argument
# (if supplied), and returns another ISO 8601 string that has the appropriate
# level of precision. Optionally, also returns the same datetime for a
# "alternate" time zone other than UTC; this would allow user to easily evaluate
# whether output makes sense based on their knowledge of the input data.

# We envision that this function would be incorporated into a wrapper function
# to handle output received from date_ingest_checker()

timestamp_output_formatter <- function(x, precision = NULL, suppOutput = NULL) {

  #   x: properly formatted ISO 8601 string (in UTC); in our concept, this would
  #      ideally be the output from date_ingest_checker(); however, the function
  #      is designed to operate by itself with user-supplied arguments
  #   precision: optional argument, may be passed along from the value user
  #      supplied for the precision argument in date_ingest_checker() or the
  #      wrapper function (once we build it); if not supplied, we will report
  #      to a precision of seconds (consistent with ISO 8601 specification)
  #  suppOutput ("supplemental output"): optional argument allowing user to
  #      request timestamp output in "local" or other time zone in addition to
  #      UTC

  ### First, convert the ISO 8601 string to an R datetime object

  # shouldn't need to do any format-checking since we assume this is a correctly
  # formatted ISO 8601 string (ideally, received directly from the
  # date_ingest_checker function)

  ISOdate <- parsedate::parse_iso_8601(x)

  ### Generate format string(s) appropriate to desired level of precision

  # main format string

  if (!is.null(precision)) {

    precision <- match.arg(precision,
                           c("day", "minute", "second", "milliSecond"),
                           several.ok = FALSE)

    # create appropriate basic format string for level of precision to be
    # applied, based on function argument

    switch(precision,
           day = {formatOutISO <- "%Y-%m-%d"},
           minute = {formatOutISO <- "%Y-%m-%dT%H:%M"},
           second = {formatOutISO <- "%Y-%m-%dT%H:%M:%S"},
           milliSecond = {formatOutISO <- "%Y-%m-%dT%H:%M:%OS"}
    )

  } else {

    # no precision argument suppled: just go out to seconds, consistent with
    # ISO 8601 specification

    formatOutISO <- "%Y-%m-%dT%H:%M:%S"

  }

  # generate shorter appropriate offset time strings that will be appended to
  # the main format string generated above; ISO 8601 does not require the offset
  # designator if only Y-m-d is given

  # have to use as.POSIXlt rather than as.POSIXct for this task so that time zone
  # conversion works properly

  if (formatOutISO == "%Y-%m-%d") {

    UTC_offsetString <- ""
    suppOutput_offsetString <- ""

  } else {

    UTC_offsetString <- "+00:00"
    suppOutput_offsetString <- "%z"

  }

  ### Generate the output

  # generate the UTC timestamp; store in list object
  dateTimeUTC <- format(as.POSIXlt(x, tz = "UTC"),
                        format = paste0(formatOutISO, UTC_offsetString))
  outList <- list("dateTimeUTC" = dateTimeUTC) # create our output list object

  # generate the supplementary output, if requested
  if (!is.null(suppOutput)) {

    dateTimeSuppOutput <- format(as.POSIXlt(x, tz = suppOutput),
                                 format = paste0(formatOutISO, suppOutput_offsetString))

    # need to insert the colon in the right place in the optional-timezone output
    # string to make it ISO 8601 compliant

    dateTimeSuppOutput <- gsub('^(.*)(.{2})$', '\\1:\\2', dateTimeSuppOutput)

    # append the supplemental timestamp to the output list
    outList$dateTimeSuppOutput <- dateTimeSuppOutput

  }

  # return output
  return(outList)

}
