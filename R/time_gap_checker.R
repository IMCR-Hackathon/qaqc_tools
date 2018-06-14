#' @title Check for gaps in a time series
#'
#' @description Checks for gaps in time sequences larger than a threshold
#'   identified by the user or the standard deviation of the time step if a
#'   threshold is not provided.
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

time_gap_checker <- function(dat, datetimeVar, gapThreshold) {
  
  # confirm that the date time variable is of type posixct or posixlt
  if (!(is.POSIXct(tower[['timestamp']]) | is.POSIXlt(tower[['timestamp']]))) {
    stop("variable to check must be of type POSIXct or type POSIXlt")
  }
  
  datetimeVec <- dat[[datetimeVar]]
  timeGap <- datetimeVec - lag(datetimeVec)
  
  # set gap threshold to the standard deviation of the time step if not provided
  if (missing(gapThreshold)) {
    gapThreshold <- sd(timeGap, na.rm = T)
  }
  
  sequenceGap <- timeGap > gapThreshold
  
  # set flags according to:
  # -1 not run
  # 0 passed
  # 1 fail
  sequenceGap <- ifelse(is.na(sequenceGap), -1,
                        ifelse(sequenceGap == TRUE, 1,
                               ifelse(sequenceGap == FALSE, 0, NA)
                        )
  )
  
  return(sequenceGap)
  
}
