

#' Fit height-diameter model
#'
#' Fits a height-diameter model.
#'
#' @param trees Data frame with columns h (height in m) and dbh (diameter at breast height in cm).
#' @return A fitted nonlinear least squares model.
#' @references
#' Naslund, M. (1936). Thinning experiments in pine forest conducted by the forest experiment station. Meddelanden fran Statens Skogsforsoksanstalt, 29, 1-169.
#' @examples
#' fitHD(trees)
#' @export
fitHD <- function(trees) {
  data1 <- trees[!is.na(data[[h]] * trees$dbh), ]
  fit <- nls(h ~ 1.3 + (dbh / (a + b * dbh))^3,
             data = data1,
             start = c(a = 1.5, b = 0.34))
  return(fit)
}
