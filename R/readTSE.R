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
read_tse_data <- function(filename, n_boxes, lights_on = 7, lights_off = 19) {
  ##read in raw TSE data
  ##TSE reports null values as "-"
  tse_data <- read.csv(filename, skip = 3 + n_boxes, na.strings = "-")

  #Convert data frame to a tidy data format
  tse_data <- melt(tse_data,
                   id.vars = c(1, 2),
                   variable.name = "Time")

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

  ##After conversion, merge back into original data table
  tse_data <- merge(tse_data, unique_times, by = "Time")

  ##Reorder columns and drop unparsed time
  ##Then sort based on Box, then Parameter, then Time
  tse_data <- tse_data[, c(2, 3, 5, 4, 6)]
  names(tse_data) <- c("Box", "Parameter", "Time", "Value", "Lights")
  tse_data <- arrange(tse_data, Box, Parameter, Time)
}
