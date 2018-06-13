# numerical check functions

numeric_range_checker <- function(dat,
                                  parms,
                                  ...) {
  # Description: checks the range to determine within acceptible realistic ranges, 
  # specified by user (e.g., relative humidity must be between 0 and 100, range of 
  # callibrated temperatures). 
  
  # Inputs: dat a data.frame with 
  # Returns: 0 = pass; 1 = fail; -1 = cannot be used
  # Tests: range_test1, range_test2
  # Examples: 
  # parms <- data.frame()
  # numeric_range_checker(dat = range_test1, 
  #                       parms = parms)
  # Team members: Celeste
  
  if(!("rangeChecks" %in% names(parms))) 
    stop("rangeChecks must be a column in parms table")
  if( length(parms$rangeChecks) != 2 ) 
    stop("rangeChecks must be a length 2 with a lower and upper bound")
  
  if( parms$rangeChecks[1] >= parms$rangeChecks[2] ) 
    stop("lower bound of rangeChecks must be less than upper bound")
  
  if (!is.numeric(dat$values)) {
    attr(dat, "range_fails") <- rep(-1, length(dat$values))
  } else {
    lower_lim <- parms$rangeChecks[1]
    upper_lim <- parms$rangeChecks[2]
    
    attr(dat, "range_fails") <- which( ( dat$values < lower_lim) | (dat$values > upper_lim)  )
  }
  return(dat)
}

set.seed(42)
# range test data set 1
# catch values that exceed abs threshold
t <- 1:300
values <- 25 * sin(t) + rnorm(length(t), 0, 10)
dates <- seq.Date(as.Date("2017-01-01"), by = "day", length.out = length(t))
range_test1 <- data.frame(dates = dates, 
                          values = values)
# plot(range_test1, type = "l")
parms <- data.frame(rangeChecks = c(-40, 40))
dat <- range_test1

test1 <- numeric_range_checker(dat = range_test1, 
                               parms = parms)

all.equal(attributes(test1)$range_fails, 
          c(18, 36, 246, 269, 300)) 

# range test 2 data 
# test that non-numeric data
range_test2 <- data.frame(dates = dates,
                          values = sample(letters, 
                                          size = length(dates),
                                          replace = TRUE))

test2 <- numeric_range_checker(dat = range_test2, 
                               parms = parms)  

all.equal(attributes(test2)$range_fails, 
          rep(-1, length(test2$values))) 
 
# range test 3 data 
# test that parms file has correct name
parms_bad_nm <- data.frame(rangeCheckers = c(-40, 40))      
test2 <- numeric_range_checker(dat = range_test2, 
                               parms = parms_bad_nm)  

all.equal(attributes(test2)$range_fails, 
          rep(-1, length(test2$values))) 



numeric_slope_checker <- function(x) {
  
  # Description: 
  
  # Inputs:
  
  # Returns:
  
  # Tests:
  
  # Examples:
  
  # Team members: Celeste
  
}

numeric_step_checker <- function(x) {
  
  # Description: 
  
  # Inputs:
  
  # Returns:
  
  # Tests:
  
  # Examples:
  
  # Team members: Celeste
  
}