
library(devtools)
library(roxygen2)

setwd("C:/Users/kcawley/Documents/GitHub/qaqc_tools")
#create("dateCheckR")

setwd("C:/Users/kcawley/Documents/GitHub/qaqc_tools/dateCheckR")
document()
devtools::check()

install("dateCheckR")
library(dateCheckR)
