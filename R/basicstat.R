#' 1.Calculate the mean of a numeric vector.
#'
#' This function calculates the mean of a numeric vector. If the vector is empty,
#' it returns an error message.
#'
#' @param data A numeric vector.
#' @return The mean value of data if it is not empty; otherwise, an error message.
#' @export
calculate_mean <- function(data) {
  if (length(data) == 0) {
    return("Error: Empty vector")
  } else {
    return(mean(data))
  }
}


#' 2.Calculate the median of a numeric vector.
#'
#' This function calculates the median of a numeric vector. If the vector is empty,
#' it returns an error message.
#'
#' @param data A numeric vector.
#' @return The median value of data if it is not empty; otherwise, an error message.
#' @importFrom stats median
#' @export
calculate_median <- function(data) {
  if (length(data) == 0) {
    return("Error: Empty vector")
  } else {
    return(median(data))
  }
}


#' 3.Calculate the mode(s) of a numeric vector.
#'
#' This function calculates the mode(s) of a numeric vector. If the vector is empty,
#' it returns an error message. If there is no mode (all values are unique), it
#' returns "No mode found".
#'
#' @param data A numeric vector.
#' @return The mode(s) of data if it exists; otherwise, an error message or "No mode found".
#' @export
calculate_mode <- function(data) {
  if (length(data) == 0) {
    return("Error: Empty vector")
  } else {
    unique_values <- unique(data)
    frequencies <- tabulate(match(data, unique_values))
    mode_index <- which.max(frequencies)
    modes <- unique_values[frequencies == frequencies[mode_index]]

    if (length(modes) == length(unique_values)) {
      return("No mode found")
    } else {
      return(modes)
    }
  }
}


#' 4.Calculate the variance of a numeric vector.
#'
#' @param x A numeric vector.
#' @return The variance of x.
#' @export
calculate_variance <- function(x) {
  mean_value <- mean(x)
  sum_squared_differences <- sum((x - mean_value)^2)
  variance <- sum_squared_differences / (length(x) - 1)
  return(variance)
}


#' 5.Calculate the standard deviation of a numeric vector.
#'
#' @param x A numeric vector.
#' @return The standard deviation of x.
#' @importFrom stats var
#' @export
calculate_standard_deviation <- function(x) {
  variance <- var(x)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}



