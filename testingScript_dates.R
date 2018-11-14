library(devtools)
library(roxygen2)

setwd("~/GitHub/qaqc_tools")
#create("dateCheckR")
install("dateCheckR")
library(dateCheckR)

setwd("~/GitHub/qaqc_tools/dateCheckR")
document()
devtools::check()

##### Issues to address durign virtual hackathon #####
#Timezone required for day precision, which doesn't make sense
date_asPOSIXct <- date_ingest_checker(x="2018-01-01",Format="%Y-%m-%d",precision="day")

#Add common sense checks, data collected prior to 1980 or so and dates aren't more that 24 hours in the future from system time
date_asPOSIXct <- date_ingest_checker(x="1974-01-01",Format="%Y-%m-%d",precision="day")
date_asPOSIXct <- date_ingest_checker(x="2020-11-01T13:30",Format="%Y-%m-%d",TZ = "US/Mountain",precision="minute")

#Test wiith real data


#Add some checks for max, min, distribution using descTools