#' Calculate number of trees per ha
#'
#' Computes the density of trees on each plot, expressed as the number of trees per hectare.
#'
#' @param plotID Factor or character vector identifying plots.
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with `plotID` and tree density (N per ha).
#' @examples
#' calcNHa(trees$plotID, plotArea = 400)
#' @export
calcNHa <- function(plotID, plotArea = 250) {
  data.frame(plotID = plotID) %>%
    group_by(plotID) %>%
    summarise(NHa = round(n() / plotArea * 10000))
}
