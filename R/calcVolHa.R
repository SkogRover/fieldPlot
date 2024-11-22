#' Calculate volume per ha
#'
#' Computes the total volume of trees in each plot.
#'
#' @param vol Numeric vector of tree volumes in m3.
#' @param plotID Factor or character vector identifying plots.
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with `plotID` and total volume (m3 ha-1).
#' @examples
#' calcVolHa(trees$vol, trees$plotID, plotArea = 400)
#' @export
calcVolHa <- function(vol, plotID, plotArea = 250) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for this function. Please install it using install.packages('dplyr').")
  }
  require(dplyr)
  data.frame(plotID = plotID, vol = vol) %>%
    group_by(plotID) %>%
    summarise(VolHa = sum(vol) / plotArea * 10000)
}
