##Returns period data for a given parameter
##Sum for activity, max - min for drink and feed,
##and average for all other parameters
get_period_data <- function(tse_data, parameter, period,
                            wide = FALSE, factors = FALSE) {
  param_subset <- subset(tse_data, Parameter == parameter & Lights %in% period)

  ##Generate list of factors to include in aggregate function
  if (factors == TRUE) {
    aggregate_by <- list(Box = param_subset$Box, Day = param_subset$Day,
                         Text1 = param_subset$Text1, Text2 = param_subset$Text2)
  }
  else {
    aggregate_by <- list(Box = param_subset$Box, Day = param_subset$Day)
  }

  ## Locomotor activity is summed over the period
  if (parameter %in% c("XA", "XF", "X", "YA", "YF", "YT", "Z", "XT+YT")) {
    param_subset <- aggregate(param_subset$Value,
                              aggregate_by,
                              FUN = sum)
  }
  ## Drink and feed are cumulative, so take the min/max diff over the period
  else if (parameter %in% c("Drink", "Feed")) {
    param_subset <- aggregate(param_subset$Value,
                              aggregate_by,
                              FUN = function(x) {max(x) - min(x)})
  }
  ## All other parameters are averaged over the period
  else {
    param_subset <- aggregate(param_subset$Value,
                              aggregate_by,
                              FUN = mean)
  }

  ##Cast into wide format if requested
  if (wide == TRUE) {
    dcast(param_subset, Box ~ Day, value.var = "x")
  }
  else param_subset
}

##Creates a wide format table interleaving the light, dark, and
##daily data for a given parameter
##This is the preferred format for reading data in excel by my PI
interleave_periods <- function(tse_data, parameter, range = "All", box_data = NULL) {
  light_data <- get_period_data(tse_data, parameter, "Light", wide = TRUE)
  dark_data <- get_period_data(tse_data, parameter, "Dark", wide = TRUE)
  daily_data <- get_period_data(tse_data, parameter, c("Light", "Dark"), wide = TRUE)

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
  bound_data <- bound_data[s]

  ##If box data is provided, add meta data to first three columns
  if (!(is.null(box_data))) {
    bound_data <- cbind(box_data[1:length(bound_data[[1]]), 4:6], bound_data)
  }

  ##Append parameter name as first column, and fixes column name
  bound_data <- cbind(rep(parameter, length(bound_data[[1]])), bound_data)
  colnames(bound_data)[1] <- "Parameter"
  bound_data
}
