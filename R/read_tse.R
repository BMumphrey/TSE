##read first two lines of a formatted TSE file
##returns a data frame containing meta data
read_tse_meta <- function(filename) {
  ##Read first line into a data frame
  col_names1 <- c("Run_Title", "Experiment_Number", "Note_1", "Note_2", "Note_3")
  col_classes1 <- rep("character", 5)
  tse_meta <- read.csv(filename, header = FALSE,
                       nrows = 1,
                       col.names = col_names1,
                       colClasses = col_classes1)

  ##Read second line into data frame and append to first data frame
  col_names2 <- c("NA", "TSE_Version")
  col_classes2 <- rep("character", 2)
  tse_version <- read.csv(filename, header = FALSE,
                                   skip = 1,
                                   nrows = 1,
                                   col.names = col_names2,
                                   colClasses = col_classes2)
  tse_meta <- mutate(tse_meta, TSE_Version = tse_version$TSE_Version)
}

##Read box information from a formatted TSE file
##returns a data frame containing box information
read_tse_boxes <- function(filename, n_boxes) {
  col_classes <- c("integer", "integer",
                   "numeric", "character",
                   "character", "character",
                   "logical")

  tse_boxes <- read.csv(filename, skip = 2,
                        nrows = n_boxes,
                        colClasses = col_classes,
                        na.strings = " ")

  ##TSE data files come with an extra column after box information
  ##This trims that null column
  tse_boxes <- subset(tse_boxes, select = Box:Text3)
}

##Read TSE data from a formatted TSE file
##Returns a data table adhering to tidy data principles
read_tse_data <- function(filename, n_boxes, lights_on = 7,
                          lights_off = 19, na.rm = TRUE, add_factors = TRUE) {

  ##read in raw TSE data
  ##The TSE system reports null values as "-"
  tse_data <- read.csv(filename, skip = 3 + n_boxes, na.strings = "-")

  #Convert data frame to a tidy data format
  tse_data <- melt(tse_data,
                   id.vars = c(1, 2),
                   variable.name = "Time")

  ##Convert box variable to numeric to support na.rm
  tse_data <- mutate(tse_data, Box = as.character(Box))
  tse_data <- mutate(tse_data, Box = as.numeric(substring(Box, 4, nchar(Box))))

  ##Remove empty boxes as indicated by a weight of 1
  if (na.rm == TRUE) {
    tse_boxes <- read_tse_boxes(filename, n_boxes)
    tse_data <- subset(tse_data, Box %in% subset(tse_boxes, Weight..g. != 1)$Box)
  }

  ##Convert time variable into a string to prep for POSIXct conversion
  tse_data <- mutate(tse_data, Time = as.character(Time))
  tse_data <- mutate(tse_data, Time = substring(Time, 2, nchar(Time)))

  ##Create a new table of unique times to avoid converting timestamps
  ##hundreds of thousands of times

  unique_times <- unique(tse_data$Time)
  unique_times <- as.data.frame(unique_times)
  unique_times <- mutate(unique_times,
                         parsed_times = parse_date_time(unique_times,
                                                        "mdyHM",
                                                        tz = "America/Chicago"
                                                        )
                         )
  names(unique_times) <- c("Time", "Parsed_Time")

  ##Add variable for light/dark status
  unique_times <- mutate(unique_times,
                         Lights = ifelse(hour(Parsed_Time) %in% lights_on:(lights_off - 1),
                                         "Light", "Dark"
                                         )
                         )

  ##Add variable for experiment day number
  ##Days officially begin at lights on, so first day will
  ##generally not be a full 24h period
  experiment_start <- min(unique_times$Parsed_Time)
  offset <- time_since_lights_on(experiment_start, lights_on)
  unique_times <- mutate(unique_times, Day = floor((as.numeric(Parsed_Time - experiment_start) + offset) / 86400))

  ##After conversion, merge back into original data table
  tse_data <- merge(tse_data, unique_times, by = "Time")

  ##Reorder columns and drop unparsed time
  ##Then sort based on Box, then Parameter, then Time
  tse_data <- tse_data[, c(2, 3, 5, 4, 6, 7)]
  names(tse_data) <- c("Box", "Parameter", "Time", "Value", "Lights", "Day")
  tse_data <- arrange(tse_data, Box, Parameter, Time)

  ##If requested, add factors based on optional box text fields
  if (add_factors[[1]] != FALSE) {
    fields <- append("Box", c("Text1", "Text2", "Text3"))
    merge(tse_data, tse_boxes[, fields], by = "Box")
  } else {
    tse_data
  }
}

##Returns time since lights on in seconds
##Lights on is an int giving the hour that lights come on
time_since_lights_on <- function(time, lights_on) {
  lights_on_timestamp <- time

  ##If the original time was after midnight but before lights on,
  ##lights on took place on the previous day
  if (hour(time) < lights_on){
    lights_on_timestamp - 86400
  }

  ##Set timestamp equal to the hour corresponding to lights on
  hour(lights_on_timestamp) <- lights_on
  minute(lights_on_timestamp) <- 0
  second(lights_on_timestamp) <- 0

  as.numeric(difftime(time, lights_on_timestamp, units = "secs"))
}
