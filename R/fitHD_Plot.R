#' Fit a height-diameter model with plot-Level random effect
#'
#' Fits a height-diameter model that accounts for the plot.
#'
#' @param d Numeric vector representing the diameter at breast height (dbh) of trees (in cm).
#' @param h Numeric vector representing the height of trees (in m).#' @return A fitted mixed-effects nonlinear model.
#' @param sp Integer vector representing species codes for each tree (e.g., 1, 2, 3, etc.).
#' @param plotID Factor or character vector indicating plot IDs for each tree.
#' @param timeout Numeric value specifying the maximum time (in seconds) allowed for fitting a mixed effects model for height. If the maximum time is exceeded, a simplified model will be used.
#' #' @references
#' Sharma, R. P.and Breidenbach, J. (2015). Modeling height-diameter relationships for Norway spruce, Scots pine, and downy birch using Norwegian national forest inventory data. Forest Science and Technology, 11(1), 44-53.
#' @examples
#' predictMissingHeights(trees$d, trees$h, trees$sp, trees$plotID)
#' @export
fitHD_Plot <- function(d, h, sp, plotID, timeout = 5) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The 'nlme' package is required for this function. Please install it using install.packages('nlme').")
  }
  require(nlme)

  # Prepare data
  data <- data.frame(
    d = as.numeric(d),
    h = as.numeric(h),
    sp = as.factor(sp),
    plotID = as.factor(plotID)
  )

  data1 <- data[!is.na(data$h * data$d), ]
  start <- c(a = 1.5, b = 0.34)

  # Start timer
  start_time <- Sys.time()

  # Try to fit the model
  result <- try({
    while (as.numeric(Sys.time() - start_time, units = "secs") < timeout) {
      # Attempt to fit the model
      return(nlme::nlme(
        h ~ 1.3 + (d / (a + b * d))^3,
        data = data1,
        start = start,
        fixed = list(as.formula("a~1"), as.formula("b~1")),
        random = list(plotID = list(a ~ 1)),
        control = nlmeControl(tol = 1e-6, maxIter = 10000)
      ))
    }
    stop("Timeout reached during model fitting.")
  }, silent = TRUE)

  # Handle errors or timeouts
  if (inherits(result, "try-error")) {
    message("fitHD_Plot failed or timed out. Using simplified fitHD model.")
    return(fitHD(d, h))  # Fallback to simplified model
  }

  return(result)
}


