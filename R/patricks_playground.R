data <- c(runif(100000, min=0, max=.1),runif(100000, min=.05, max=.1),runif(10000, min=.05, max=1), runif(100000, min=0, max=.2))
plot(data, type = "l", lwd = 0.3)

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
ranges <- unlist( lapply( lapply(out, range, na.rm = TRUE), diff ) )
mean(ranges)
median(ranges)




sorted_by_timestamp <- function(timeVector, allowTies = TRUE){
  if(mode(timeVector) != "numeric"){
    e <- simpleError(paste("Error: Input must be Date, POSIXct, POSIXlt or numeric.\n Input vector was ",mode(timeVector),sep=''))
    stop(e)
  }
  timeVec1 <- tail(timeVector, -1)
  
  if (allowTies == FALSE) {
    result <- ifelse(head(timeVector,length(timeVector)-1) < timeVec1, 0, 1)
  } else {
    result <- ifelse(head(timeVector,length(timeVector)-1) <= timeVec1, 0, 1)
  }
  result <- ifelse(is.na(result), -1, result)
  result <- append(result, -1)
  return(result)
}
