#' @title Check for gaps in a time series
#'
#' @description checks for gaps in time sequences larger than a threshold identified by the user 
#' 
#' @param dat A data frame with a time series column to evaluate
#' @param datetimeVar A time series column in the target data frame
#' @gapThreshold Time intevals above which an error code should be supplied
#' 
#' @importFrom lubridate is.POSIXct
#' @importFrom dplyr lag
#' 
#' @return Vector of check results for each row in target dataframe
#' 
#' @export

gap_checker <- function(dat, datetimeVar, gapThreshold) {
  
  if (!is.POSIXct(dat[[datetimeVar]])) {
    stop("variable to check must be of type POSIXct")
  }
  
  datetimeVec <- dat[[datetimeVar]]
  timeGap <- datetimeVec - lag(datetimeVec)
  sequenceGap <- timeGap > gapThreshold
  sequenceGap <- ifelse(is.na(sequenceGap), -1,
                        ifelse(sequenceGap == TRUE, 1,
                               ifelse(sequenceGap == FALSE, 0, NA)
                        )
  )
  
  return(sequenceGap)
  
}
