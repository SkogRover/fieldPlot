#' Calculate basal area per ha
#'
#' Computes the basal area in each plot, scaled to per ha values.
#'
#' @param ba Numeric vector of basal areas of trees in m2.
#' @param plotID Factor or character vector identifying plots.
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with `plotID` and total basal area (in m2 ha-1).
#' @examples
#' calcBaHa(trees$ba, trees$plotID, plotArea = 250)
#' @export
calcBaHa <- function(ba, plotID, plotArea) {
  data.frame(plotID = plotID, ba = ba) %>%
    group_by(plotID) %>%
    summarise(BaHa = sum(ba) / plotArea * 10000)
}
