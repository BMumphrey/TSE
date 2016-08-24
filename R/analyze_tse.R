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
interleave_averages <- function(light_data, dark_data, daily_data, trim = TRUE) {
  ##If necessary, trim final day from light and daily data
  ##so it matches dark data length
  ##This imbalance occurs because runs are ended during the light period
  ##of the final day, and therefore no dark period takes place
  if (trim == TRUE) {
    if (length(light_data) > length(dark_data)) {
      light_data <- light_data[, 1:length(light_data) - 1]
      daily_data <- daily_data[, 1:length(daily_data) - 1]
    }
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
