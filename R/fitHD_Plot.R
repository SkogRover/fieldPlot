#' Fit a height-diameter model with plot-Level random effect
#'
#' Fits a height-diameter model that accounts for the plot.
#'
#' @param trees Data frame with columns plotID, h (height in m), and dbh (diameter at breast height in cm).
#' @return A fitted mixed-effects nonlinear model.
#' #' @references
#' Sharma, R. P.and Breidenbach, J. (2015). Modeling height-diameter relationships for Norway spruce, Scots pine, and downy birch using Norwegian national forest inventory data. Forest Science and Technology, 11(1), 44-53.
#' @examples
#' predictMissingHeights(trees$d, trees$h, trees$sp, trees$plotID)
#' @export
fitHD_Plot <- function(d, h, sp, plotID) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The 'nlme' package is required for this function. Please install it using install.packages('nlme').")
  }
  require(nlme)

  data <- data.frame(
    d = as.numeric(d),
    h = as.numeric(h),
    sp = as.factor(sp),
    plotID = as.factor(plotID)
  )

  data1 <- data[!is.na(data$h * data$d), ]

  start <- c(a = 1.5, b = 0.34)

  nlme::nlme(h ~ 1.3 + (d / (a + b * d))^3,
             data = data1,
             start = start,
             fixed = list(as.formula("a~1"), as.formula("b~1")),
             random = list(plotID = list(a ~ 1)),
             control = nlmeControl(tol = 1e-6, maxIter = 100))

}


