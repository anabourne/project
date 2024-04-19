#x <- individual[1, ]


#geosphere::destPoint(
#  p = c(x$decimalLongitude, x$decimalLatitude),
#  b = x$stemAzimuth, 
#  d = x$stemDistance
#)
# given the center of the plot, we're now getting a longitude and a latitude


# If I try to do the same with a vector, it gives an error. We have to make it 2 
# matrix. Instead of using c(), we can use cbind()
#x <- individual[1:5, ]
#checkmate::assert_numeric(x$decimalLatitude) # check the input is numeric, it 
# doesnt return anything because it is numeric. Otherwise, you get an error with 
# a message explaining what the error was.

#geosphere::destPoint(
#  p = cbind(x$decimalLongitude, x$decimalLatitude),
# b = x$stemAzimuth, 
#  d = x$stemDistance
#)

# Our function
#' Calculate the location of stem based on azimuth and distance
#' 
#' @param decimalLongitude numeric vector of decimal longitudes
#' @param decimalLatitude numeric vector of decimal latitudes
#' @param stemAzimuth numeric vector of stem azimuths
#' @param stemDistance numeric vector of stem distances
#'
#' @return A tibble of pairs of coordinates


get_stem_location <- function(decimalLongitude,decimalLatitude,
                              stemAzimuth, stemDistance) {
  # input validation checks
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)
  
  out <- geosphere::destPoint(
    p = cbind(decimalLongitude, decimalLatitude),
    b = stemAzimuth, 
    d = stemDistance
  ) |>
    tibble::as_tibble() #it gives us back a tibble
  
  # check NA in out and if there's a single NA values, any will return true, but
  # we want our output to be false and that what assert does.
  checkmate::assert_false(any(is.na(out)))
  
  return(out)
}

# Testing function
#test <- get_stem_location(
#  x$decimalLongitude, x$decimalLatitude,
#  x$stemAzimuth, x$stemDistance
#)
