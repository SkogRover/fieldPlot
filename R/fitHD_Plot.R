#' Fit height-diameter model by plot
#'
#' Fits a height-diameter model that accounts for the plot.
#'
#' @param trees Data frame with columns plotID, h (height in m), and dbh (diameter at breast height in cm).
#' @return A fitted mixed-effects nonlinear model.
#' #' @references
#' Sharma, R. P.and Breidenbach, J. (2015). Modeling height-diameter relationships for Norway spruce, Scots pine, and downy birch using Norwegian national forest inventory data. Forest Science and Technology, 11(1), 44-53.
#' @examples
#' fitHD_Plot(trees)
#' @export
fitHD_Plot <- function(trees) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The 'nlme' package is required for this function. Please install it using install.packages('nlme').")
  }

  data1 <- trees[!is.na(trees$h * trees$dbh), ] # trees with measured height
  start <- c(1.5, 0.34) # start values
  fit <- NULL
  nlme::nlme(h ~ 1.3 + (dbh / (a + b * dbh))^3,
       data = data1,
       start = start,
       fixed = list(as.formula("a~1"), as.formula("b~1")),
       random = list(plotID = list(a ~ 1)),
       control = nlmeControl(tol = 1e-6, maxIter = 100))
}
