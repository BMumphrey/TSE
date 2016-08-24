##read first two lines of a formatted TSE file
##returns a data frame containing meta data
read.tse.meta <- function(filename)
{
  ##Read first line into a data frame
  columns1 <- c("Run Title", "Experiment Number", "Note 1", "Note 2", "Note 3")
  col.classes1 <- rep("character", 5)
  tse.meta <- read.csv(filename, header = FALSE, 
                       nrows = 1, 
                       col.names = columns1, 
                       colClasses = col.classes1)
  
  ##Read second line into data frame and append to first data frame
  columns2 <- c("NA", "TSE.Version")
  col.Classes2 <- rep("character", 2)
  tse.version <- read.csv(filename, header = FALSE,
                                   skip = 1,
                                   nrows = 1,
                                   col.names = columns2,
                                   colClasses = col.Classes2)
  tse.meta <- mutate(tse.meta, TSE.Version = tse.version$TSE.Version)
}

##Read box information from a formatted TSE file
##returns a data frame containing box information
read.tse.boxes <- function(filename, n.boxes)
{
  col.classes <- c("integer", "integer", 
                   "numeric", "character", 
                   "character", "character", 
                   "logical")
  
  tse.boxes <- read.csv(filename, skip = 2, 
                        nrows = n.boxes, 
                        colClasses = col.classes, 
                        na.strings = " ")
  ##TSE data files come with an extra column after box information
  ##This trims that null column
  tse.boxes <- subset(tse.boxes, select = Box:Text3)
}

##Read TSE data from a formatted TSE file
##Returns a data table adhering to tidy data principles
read.tse.data <- function(filename, n.boxes, lights.on = 7, lights.off = 19)
{
  ##read in raw TSE data
  ##TSE reports null values as "-"
  tse.data <- read.csv(filename, skip = 3 + n.boxes, na.strings = "-")
  
  #Convert data frame to a tidy data format
  tse.data <- melt(tse.data, 
                   id.vars = c(1, 2),
                   variable.name = "Time")
  
  ##Convert time variable into a string to prep for POSIXct conversion
  tse.data <- mutate(tse.data, Time = as.character(Time))
  tse.data <- mutate(tse.data, Time = substring(Time, 2, nchar(Time)))
  
  ##Create a new table of unique times to avoid converting timestamps
  ##hundreds of thousands of times
  
  unique.times <- unique(tse.data$Time)
  unique.times <- as.data.frame(unique.times)
  unique.times <- mutate(unique.times, 
                         parsed.times = parse_date_time(unique.times, 
                                                        "mdyHM", 
                                                        tz = "America/Chicago"
                                                        )
                         )
  names(unique.times) <- c("Time", "Parsed.Time")
  
  ##Add variable for light/dark status
  unique.times <- mutate(unique.times, 
                         Lights = ifelse(hour(Parsed.Time) %in% lights.on:(lights.off - 1), 
                                         "Light", "Dark"
                                         )
                         )
  
  ##After conversion, merge back into original data table
  tse.data <- merge(tse.data, unique.times, by = "Time")
  
  ##Reorder columns and drop unparsed time
  ##Then sort based on Box, then Parameter, then Time
  tse.data <- tse.data[, c(2, 3, 5, 4, 6)]
  names(tse.data) <- c("Box", "Parameter", "Time", "Value", "Lights")
  tse.data <- arrange(tse.data, Box, Parameter, Time)
}