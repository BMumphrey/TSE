##Returns the 24hr average for a given parameter for all boxes
##in a given tide tse_data frame.
##Returns a data frame in wide format
daily_average <- function(tse_data, parameter) {

  param_subset <- subset(tse_data, Parameter == parameter)
  param_subset <- aggregate(param_subset$Value,
                            list(Box = param_subset$Box, Day = param_subset$Day),
                            FUN = mean)
  ##Cast into wide format
  dcast(param_subset, Box ~ Day, value.var = "x")
}

##Returns the light period average for a given parameter for all boxes
##in a given tide tse_data frame.
##Returns a data frame in wide format
light_average <- function(tse_data, parameter) {
  param_subset <- subset(tse_data, Parameter == parameter & Lights == "Light")
  param_subset <- aggregate(param_subset$Value,
                            list(Box = param_subset$Box, Day = param_subset$Day),
                            FUN = mean)
  ##Cast into wide format
  dcast(param_subset, Box ~ Day, value.var = "x")
}

##Returns the dark period average for a given parameter for all boxes
##in a given tide tse_data frame.
##Returns a data frame in wide format
dark_average <- function(tse_data, parameter) {
  param_subset <- subset(tse_data, Parameter == parameter & Lights == "Dark")
  param_subset <- aggregate(param_subset$Value,
                            list(Box = param_subset$Box, Day = param_subset$Day),
                            FUN = mean)
  ##Cast into wide format
  dcast(param_subset, Box ~ Day, value.var = "x")
}

##Creates a wide format table interleaving the light, dark, and
##daily data for a given parameter
##This is the preferred format for reading data in excel by my PI
interleave_averages <- function(light_data, dark_data, daily_data, range = "All") {
  if (range[[1]] == "All") {
    if (length(light_data) != length(dark_data)
        | length(light_data) != length(daily_data)) {
      stop("Data frames are not the same size")
    }
  } else {
    if (class(range) != "integer" & class(range) != "numeric") {
      stop("Not an integer range")
    }
    if (length(range) > length(light_data)
        | length(range) > length(dark_data)
        | length(range) > length(daily_data)) {
      stop("Range too large")
    }
    light_data <- light_data[range]
    dark_data <- dark_data[range]
    daily_data <- daily_data[range]
  }

  ##Add prefixes to titles so that they are easily recognizable
  ##after interleaving
  colnames(light_data) <- paste("Light", colnames(light_data))
  colnames(dark_data) <- paste("Dark", colnames(dark_data))
  colnames(daily_data) <- paste("Daily", colnames(daily_data))

  ##Bind all three data frames and re-order with interleaving
  bound_data <- cbind(light_data, dark_data, daily_data)
  s <- rep(1:length(light_data), each = 3) + (0:2) * length(light_data)
  bound_data[s]
}
