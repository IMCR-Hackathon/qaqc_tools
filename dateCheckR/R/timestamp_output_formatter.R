##############################################################################################
#' @title Function for making timestamps "pretty" for output

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Jamie Collins \email{jamesrco@uw.edu} \cr
#' Amanda Droghini \email{adroghini@alaska.edu} \cr

#' @description This function takes a POSIXct object, combines with precision argument 
#' (if supplied),and returns an ISO 8601 text string that has the appropriate level of 
#' precision. Optionally, also returns the same datetime for a "alternate" time zone other 
#' than UTC; this would allow user to easily evaluate whether output makes sense based on 
#' their knowledge of the input data.

#' @importFrom methods is

#' @param x a POSIXct object; in our concept, this wouldideally be the output from 
#' date_ingest_checker(); however, the function is designed to operate by itself with 
#' user-supplied arguments [POSIXct]
#' @param precision optional argument, may be passed along from the value usersupplied for 
#' the precision argument in date_ingest_checker() or the wrapper function (once we build it); 
#' if not supplied, we will report to a precision of seconds 
#' (consistent with ISO 8601 specification) [character]
#' @param suppOutput ("supplemental output"): optional argument allowing user to request 
#' timestamp output in "local" or other time zone/locality in addition to UTC; must be something 
#' in the Olson list [character]

#' @return ISO 8601 format string based on degree of time precision and desired time zone 
#' (choices are either UTC or another, non-UTC time) [character]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords datetime, date, time, timestamp

#' @examples
#' pretty_date <- timestamp_output_formatter(as.POSIXct("2018-01-01T13:30"),"minute")

#' @seealso addISO8601colon.R, handleTimestamp.R, ISOStringPrecformatter.R, and 
#' date_ingest_checker.R related functions for formatting dates in ISO standard format

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley, Jamie Collins, Amanda Droghini (2018-06-11)
#     original creation @ IMCR Hackathon in New Mexico
#   Kaelin M. Cawley (2018-11-14)
#     updates to formatting and examples @ IMCR virtual hackathon
###############################################################################################
timestamp_output_formatter <- function(x, 
                                       precision = NULL,
                                       suppOutput = NULL) {

  ### Check arguments
  if (!is.null(suppOutput)) {

    if (!(suppOutput %in% OlsonNames())) {

      stop("The time zone or locality supplied for suppOutput must be one of ",
           "the values present in the R OlsonNames() list. Try OlsonNames() ",
           "at the R console prompt to view a full list of the valid options.")

    }

  }

  if (!methods::is(x, "POSIXct")) {

    stop("Input must be an object of class 'POSIXct'\n")

  }

  ### Generate the output
  # generate the UTC timestamp; store in list object
  dateTimeUTC <- format(x,
                        format = ISOStringPrecformatter(precision = precision,
                                                        desiredZone = c("UTC")),
                        tz = "UTC")

  outList <- list("dateTimeUTC" = dateTimeUTC) # create our output list object

  # generate the supplementary output, if requested
  if (!is.null(suppOutput)) {

    dateTimeSuppOutput <- format(x,
                                 format = ISOStringPrecformatter(precision = precision,
                                                          desiredZone = c("other")),
                                 tz = suppOutput)
    # need to insert the colon in the right place in the optional-timezone output
    # string to make it ISO 8601 compliant
    dateTimeSuppOutput <- addISO8601colon(dateTimeSuppOutput)
    # append the supplemental timestamp to the output list
    outList$dateTimeSuppOutput <- dateTimeSuppOutput

  }

  # return output
  return(outList)

}
