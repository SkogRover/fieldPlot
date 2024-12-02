

#' Fit Height-Diameter Model
#'
#' Fits a nonlinear height-diameter model using input vectors for tree height and diameter.
#'
#' @param d Numeric vector representing the diameter at breast height (dbh) of trees (in cm).
#' @param h Numeric vector representing the height of trees (in m).
#' @return A fitted nonlinear least squares model.
#' @references
#' Naslund, M. (1936). Thinning experiments in pine forest conducted by the forest experiment station.
#' Meddelanden fran Statens Skogsforsoksanstalt, 29, 1-169.
#' @examples
#' fitHD(trees)
#' @export
fitHD <- function(d, h) {
  # Prepare the data: exclude rows with missing values in d or h
  data <- data.frame(
    d = as.numeric(d),
    h = as.numeric(h)
  )
  data <- data[!is.na(data$h * data$d), ]

  # Fit the model
  fit <- nls(h ~ 1.3 + (d / (a + b * d))^3,
             data = data,
             start = c(a = 1.5, b = 0.34))

  return(fit)
}

