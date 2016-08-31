##Returns period data for a given parameter
##Sum for activity, max - min for drink and feed,
##and average for all other parameters
period_data <- function(tse_data, parameter, period) {
  param_subset <- subset(tse_data, Parameter == parameter & Lights %in% period)

  ## Locomotor activity is summed over the period
  if (parameter %in% c("XA", "XF", "X", "YA", "YF", "YT", "Z", "XT+YT")) {
    param_subset <- aggregate(param_subset$Value,
                              list(Box = param_subset$Box, Day = param_subset$Day),
                              FUN = sum)
  }
  ## Drink and feed are cumulative, so take the min/max diff over the period
  else if (parameter %in% c("Drink", "Feed")) {
    param_subset <- aggregate(param_subset$Value,
                              list(Box = param_subset$Box, Day = param_subset$Day),
                              FUN = function(x) {max(x) - min(x)})
  }
  ## All other parameters are averaged over the period
  else {
    param_subset <- aggregate(param_subset$Value,
                              list(Box = param_subset$Box, Day = param_subset$Day),
                              FUN = mean)
  }

  ##Cast into wide format
  dcast(param_subset, Box ~ Day, value.var = "x")
}

##Creates a wide format table interleaving the light, dark, and
##daily data for a given parameter
##This is the preferred format for reading data in excel by my PI
interleave_periods <- function(tse_data, parameter, range = "All") {
  light_data <- period_data(tse_data, parameter, "Light")
  dark_data <- period_data(tse_data, parameter, "Dark")
  daily_data <- period_data(tse_data, parameter, c("Light", "Dark"))

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
