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
  
  if(!("range_checks" %in% names(parms))) 
    stop("range_checks must be a column in parms table")
  if( length(parms$range_checks) != 2 ) 
    stop("range_checks must be a length 2 with a lower and upper bound")
  
  if( parms$range_checks[1] >= parms$range_checks[2] ) 
    stop("lower bound of range_checks must be less than upper bound")
  
  if (!is.numeric(dat$values)) {
    attr(dat, "range_fails") <- rep(-1, length(dat$values))
  } else {
    lower_lim <- parms$range_checks[1]
    upper_lim <- parms$range_checks[2]
    
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
parms <- data.frame(range_checks = c(-40, 40))
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
parms_bad_nm <- data.frame(range_checker = c(-40, 40))      
test2 <- numeric_range_checker(dat = range_test2, 
                               parms = parms_bad_nm)  

all.equal(attributes(test2)$range_fails, 
          rep(-1, length(test2$values))) 



numeric_slope_checker <- function(dat1, dat2, dat3, ...) {
  
  # Description: test sensor drift - requires at least three sensor readings with
  # concurrent measurements
  # Inputs:
  # Returns:
  # Tests:
  # Examples:
}



numeric_spike_window_checker <- function(dat, 
                                         spike_value = c(3, 5), 
                                         verbose=FALSE) {
  # use 5 element moving window to detect spikes
  lag1<-c(NA,head(dat$value,-1)) # lag-1
  lagMinus1<-c(tail(dat$value,-1),NA) # lag+1
  lag2<-c(NA,NA,head(dat$value,-2)) # lag-2
  lagMinus2<-c(tail(dat$value,-2),NA,NA) # lag+2
  s<-cbind(lag1,lagMinus1,lag2,lagMinus2,dat$value)
  #calculate the median
  medianVec=apply(s[,c("lag1","lagMinus1","lag2","lagMinus2","dat$value")],1,median)
  isSpikeVal<- ((abs(dat$value-medianVec) > spike_value) 
                &(!is.na(lag1))&(!is.na(lagMinus1))
                &(!is.na(lag2))&(!is.na(lagMinus2)))
  if(verbose){
    print(summary(isSpikeVal))
  } 
}


test_dat <- read.csv("data/sample_corrupted_data.csv", stringsAsFactors = FALSE)
table(nchar(test_dat$Station))
test_dat$Station <- trimws(test_dat$Station)
test_dat_1 <- test_dat[which(test_dat$Station == "CB"),]
plot(test_dat_1$Temperature_C, type = "l")

length(test_dat_1$Temperature_C)
S <- 20
test_dat_1
# https://stackoverflow.com/questions/43345486/median-filter-in-r 
if (type == "median_filter" | is.null(type)) {
  smoothed <- runmed(test_dat_1$Temperature_C, k = 5)
  resids <- test_dat_1$Temperature_C - smoothed
}

moving_windows <- function(dat, win_size) {
  win_size <- 5
  dat <- t
  n <- length(dat)
  n_windows <- n - win_size + 1
  out <- vector(mode = "list", length = n_windows)
  for (i in 1:(n_windows)) {
    out[[i]] <- dat[i:(i+win_size-1)]
  }
  return(out)
}

set.seed(42)
t <- rpois(40, 10)
out <- moving_windows(t, win_size = 5)
all.equal(out[[36]],
          c(12,  7,  8, 14, 10))
all.equal(out[[1]], 
          c(14, 8, 11, 12, 11))
  
  if all NA
  if one_non_NA
  if >= 2 non_NA (range, na.rm = TRUE
                  
}

mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)} #Average


# https://dsp.stackexchange.com/questions/30213/spikes-in-time-series
# Let your original signal be f[n]
# Median filter f[n]
# using N pixels, where N>2×S+1, where S is the maximum number of samples in the spike. 
# The resulting signal, lets call it g[n]
# should have all the spikes removed.
# Find the absolute of the difference between the two signals, h[n]=|f[n]−g[n]|
# This signal represents the spikes.

# Count the number of positive transitions in h[n]
# that are above a threshold. This is the number of spikes.


dat <- t
numeric_spike_med_filter_check <- function(dat, thresh = NULL, 
                                           win_size = NULL, ...) {
  # Needs NA handling and more tests
  # better default win_size based on length of data and NA freq
  # NEON code def.dspk.window.R
  # # Store na positions as "unable to evaluate" spikes
  # posSpk[[idxVar]]$posQfSpk$na <- which(is.na(trns))
  # 
  # if(Trt$NaTrt == "approx") {
  #   trns <- approx(x=index(trns), y=trns, xout=index(trns))$y
  # }
  if (is.null(thresh)) {
    out <- moving_windows(dat = dat, win_size = win_size)
    thresh <- 2 * median(unlist( lapply(out, sd, na.rm = TRUE)))
    # unlist( lapply(out, mad, na.rm = TRUE))
    # unlist( lapply(out, median, na.rm = TRUE))
  }
  if (is.null(win_size)) win_size <- 5

  med_smoothed <- runmed(x = dat, k = win_size)
  resids <- abs(dat - med_smoothed)
  fails <- which(resids > thresh)
  attr(dat, "spike_med_filter_fails") <- fails
  return(dat)

}

numeric_spike_checker <- function(x, spike_min = NA, verbose = FALSE) {
  # Description: Tests for spikes in data
  # Inputs: vector to be tested, optional threshold for how large a spike 
  # must be, verbose = TRUE/FALSE
  # Returns: result contains 0 for no spike, 1 for spike detected and -1 
  # for test could not be performed.  If verbose is selected,  the 
  # threshold value and a summary of the frequency of the codes is printed. 
  # # Team members: John, Patrick, Kaelin
  
  # if spike_min isn't specified, try to define a spike minimum based on 
  # the variance of the data stream. Default is 1.96 * standard deviation
  # of the sequential differences
  # NOTE if the default step is used, about 5% of the data will be flagged
  # even if there are no real problems with the data
  spike_min <- ifelse(is.na(spike_min), 
                      (1.96 * sd(diff(x, lag=1), na.rm=TRUE)), 
                      spike_min)
  # use 3 element moving window to detect spikes
  lag1 <- c(NA, head(x, -1)) # lag-1
  lag_minus1 <- c(tail(x, -1), NA) # lag+1
  s <- cbind(lag1, lag_minus1, x)
  #calculate the median
  median_vec <- apply(s[, c("lag1", "lag_minus1", "x")], 1, median)
  is_spike_val<- ((abs(x - median_vec) > spike_min) 
                  & (!is.na(lag1)) & (!is.na(lag_minus1)))
  result <- ifelse(is_spike_val, 1, 0)
  result <- ifelse(is.na(result), -1, result)
  if(verbose) {
    print(paste0("Looking for jumps or drops in the data > ",
                 spike_min))
    print("Frequency of result codes:")
    print(summary(as.factor(result)))
  }
  return(result)
}
}



complete_cases_checker <- function(x) {
  # returns 0 if case is complete, 1 if it is incomplete
  result<-ifelse(rowSums(is.na(x))==0,0,1)
  # complete.cases can also work if none of the data are complex structures
  # result<-ifelse(complete.cases(x),0,1)
  return(result)
}

# Description: row by row check that all value columns contain values

# Returns: 0 if no missing values are encountered, 1 if one or more columns contain missing data

# Team members: John, Patrick, Amanda

}


spaces_checker <- function(x) {
  
  # Description: Check character data for inadvertant spaces and remove spaces
  
  # Returns: A data frame without unwanted white spaces
  
  # Team members:  Kristin
  
  remove_spaces <- function(df) {
    if(class(df) != "data.frame"){
      e<-simpleError(paste("Input must be a data frame. You provided a ",class(df),sep=''))
      stop(e)
    }
    for (i in names(df)) {
      if(class(df[, i]) %in% c("factor", "character")){
        df[, i] <- trimws(df[, i])
      }
    }
    return(df)
  }
  
  noSpacesDF<-remove_spaces(df)
  
}

