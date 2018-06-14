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



numeric_slope_checker <- function(dat1, dat2) {
  
  # Description: test sensor drift, 
  # Inputs:
  # Returns:
  # Tests:
  # Examples:
}

#
dat <- 

numeric_step_checker <- function(x) {
  
  # Description: 
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

moving_range <- function(dat, window = 5) {
  
  total <- length(dat)
  spots <- seq(from=1, to=(total-window), by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- mean(data[spots[i]:(spots[i]+window)])
  }
  return(result)
  
  if all NA
  if one_non_NA
  if >= 2 non_NA (range, na.rm = TRUE
                  
}

mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)} #Average
mmed <- function(x,n=5){runmed(x,n)} #Median

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



numeric_spike_med_filter_check <- function(dat, thresh = NULL, 
                                           window = NULL, ...) {
  if (is.null(thresh)) 
  if (is.null(window)) do
  # NEON code def.dspk.window.R
  # # Store na positions as "unable to evaluate" spikes
  # posSpk[[idxVar]]$posQfSpk$na <- which(is.na(trns))
  # 
  # if(Trt$NaTrt == "approx") {
  #   trns <- approx(x=index(trns), y=trns, xout=index(trns))$y
  }
}


