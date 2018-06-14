# qaqc\_tools

A suite of tools addressing common yet critical data curating tasks aimed at the particular needs of the LTER research and Information Managers community, and that also that applies to the QA/QC needs of the broader research community. These tools are designed to automated quality control of sensor data, but may also be useful for non-sensor data sets.

## List of functions
qaqc_TimeDate_functions: Functions for working with timestamps and dates and times in
environmental data. Contains functions for ingesting, formatting, and detecting/manipulating time zones given different kinds of supplied timestamps

gap_checker: includes gap_checker function that checks for gaps in time sequences which excede a user-defined threshold for data gaps

numeric_functions: includes numeric_range_checker function that applies range limit tests to data frame, and includes skeletons for numeric_slope_checker, numeric_step_checker functions.

### qaqc_TimeDate_functions

  Provides user with warnings and messages: check for precision input, check for TZ input, check for format input

  Where possible, tries to detect precision, TZ, and date-time format without being too liberal in its assumptions. If cannot be reliably determined, stops and provides user with message as to why it failed.

  Several date-time formats are supported including ISO8601, AM/PM

  #### Description:
  1. Determine input format
  2. Determine precision for output format based on input information
  3. If data isn't a valid format output why to user for gathering correct information
  4. Allow users to specify a specific precision, e.g. hour and minutes, or just date that is expected/required. Require year, month, and day at a minimum
  5. Require unambiguous input timezone (requires legwork to determine, likely not automatable, but may be consistent over time for a site or type of data)
  ** Provide feedback to user informing them which TZ was recognized & applied
      Additional dataframe with timezones by station/site
      Timezone in input dateTimes
      Single TZ value if all of the data is coming in in the same TZ and is known
      Potentially detect timezone based off of lat/lon? Prefer to get info from data provider/source before this step
  6. Clean input times to be in ISO format
      YYYY-MM-DDTHH:MM:SS.SSSZ
  7. Convert to R dateTime object (POSIXct or POSIXlt?)
  8. Output dataframe with columns appended for input TZ and input precision (maybe check is they're all the same and output another way? Separate table?)
      TZ is a string value
      Precision options: day, minute, second, milliSecond

```
date_ingest_checker <- function(x, precision, TZ) {

    # x: some input string we believe to be a time stamp
    # optional arguments: precision, TZ (will be detected automatically if present in the data)
```

```R
timestamp_output_formatter <- function(x, precision, suppOutput) {

    # x: properly formatted ISO 8601 string (in UTC) which we receive as output from date_ingest_checker()
    # precision is a required argument for this function; may be automatically detected or specified when date_ingest_checker() is called, but either way we should have value(s) for precison before calling timestamp_output_formatter()
    # suppOutput ("supplemental output"): optional argument allowing user to request timestamp output in "local" or other time zone in addition to UTC; no localities allowed

  # Description:
      1. Additional QA/QC or timestamps updates based on some sort of input/tracking log (see Celeste's code snippet for incidents)
      2. Format date for output in ISO 8601 UTC as text string and user specified "local" or other time with appropriate level of precision from input
      3. Remove extra columns from dataframe for input timezone and precision and create an output similar to the ingest provenance output

  (fail or fix?).


}
```

Outstanding/unresolved issues for the date-time working group:
    1. What does format of provenance information look like? Harmonize with whatever format the ingest group is coming up with?
    2. Provide summary feedback to user: timestamp minimum/maximum in UTC and in TZ of input data, if TZ was given in input data
    3. Plot review of frequency distribution of times to the user to check that the dates/times are reasonable (descTools?)

```R
sorted_by_timestamp <- function(x) {

  # Description: Check that data are sorted by timestamp

  # Returns

  # Team members: John

}
```



###(Other) dataframe formatting issues

```R
spaces_checker <- function(x) {

  # Description: Check character data for inadvertant spaces

  # Returns:

  # Team members: Kristin

}
```

```R
complete_cases_checker <- function(x) {

  # Description: row by row check that all value columns

  # Returns:

  # Team members: Patrick, Amanda

}
```

```R
col_type_checker <- function(x) {

  # Description:

  # Returns:

  # Team members: Patrick, Amanda

}
```

```R
fuzzy_duplicate_checker <- function(x) {

  # Description: checks for near duplicate rows: 1) rows with the same keys but

  different values, 2) rows with the same values but (slightly) different keys, 3)

  other cases TBD

  # Returns:

  # Team members: Patrick, Amanda, Jamie

}
```

### consistent coding

```R
encoding_standard_checker <- function(x) {

  # Description:

  # Returns:

  # Team members:

}
```

Flagging & checks
NEON example R code for looking at the flags
  outTarn2 <- def.plau(data = data.frame(PARdataUpTarn5$new.data), RngMin = -10, RngMax = 3000, DiffStepMax = 500, NumGap = 4, DiffPersMin = rep.int(0.0001,length(data)),TestNull = T, Vrbs = T)
  str(outTarn2)
  unique(outTarn2$PARdataUpTarn5.new.data$qfRng)
  unique(outTarn2$PARdataUpTarn5.new.data$qfStep)
  unique(outTarn2$PARdataUpTarn5.new.data$qfGap)
  unique(outTarn2$PARdataUpTarn5.new.data$qfNull)
  unique(outTarn2$PARdataUpTarn5.new.data$qfPers)

```R
gap_checker <- function(x) {

  # Description:

  # Returns:

  # Team members: Celeste, stevan

}
```

```R
numberic_step_checker()
  # Description: Flags time series (e.g., sensor data) for unexpected
  # jumps (increase or decrease) in a single time step.

  # Methods:
  #  "simpleThreshold": manually set a threshold step value to flag observations
  # that exceed the limit
  #  "movingAverageDeviants": take the diffs based moving average and flag
  # 'large' changes
  #  "movingMedianDeviants": take the diffs based moving medians and flag
  # 'large' changes

  # Dependencies:

  # Inputs: datetime and value

  # Returns: original dataframe as

  # Tests:

  # Examples:

  # Team members:
```

```R
numeric_range_checker <- function(x) {

  # Description:

  # Returns:

  # Team members: Celeste

}
```

```R
acceptable_string_checker <- function(x) {

  # Description:

  # Returns:

  # Team members:

}
```

```R
numeric_spike_checker <- function(x) {

  # Description:

  # Returns:

  # Team members: Patrick, kaelin

}
```

```R
numeric_slope_checker <- function(x) {

  # Description:

  # Returns:

  # Team members: Celeste Patrick

}
```


```R
check_timeseries_anomalies <- function(x) {

  # Description:

  # Returns:

  # Team members: Patrick

}
```


```R
check_number_bensford <- function(x) {

  # Description: For numeric values that span multiple orders of magnitude, Benfords Law predicts a distribution rich in low values with few high values for the first digit.  The result applies to an entire series of data values, not to any particular data value.

  # Returns: T - fits Benford distribution, F - does not fit, NA - data not suitable (not >2 orders of magnitude)

  # Team members: John

}
```

```R
anomaly_outlier_vis_tool <- function(args) {

  # Description:

  # Inputs:

  # Returns:

  # Tests:

 # Examples:

  # Team members: Patrick

}
```
