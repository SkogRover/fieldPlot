#' Calculate plot summaries
#'
#' Computes plot-level summaries for tree density (N), Lorey's height (`Hlor`), dominant height (`Hdom`), volume per hectare, and basal area per hectare.
#'
#' @param d Numeric vector of diameters at breast height in cm.
#' @param h Numeric vector of tree heights in m.
#' @param ba Numeric vector of basal areas of trees in m2.
#' @param vol Numeric vector of tree volumes in m3.
#' @param plotID Factor or character vector identifying plots.
#' @param plotArea Numeric value representing the plot area in m2.
#'
#' @seealso calcNHa, calcHlor, calcVolHa, calcBaHa
#'
#' @return A data frame with plot-level summaries including the following columns:
#' - `plotID`: Identifier for each plot.
#' - `NHa`: Tree density per ha.
#' - `Hlor`: Lorey's height.
#' - `VolHa`: Volume per ha.
#' - `BaHa`: Basal area per ha.
#'
#' @examples
#' calcPlotSummaries(trees$d, trees$h, trees$ba, trees$vol, trees$plotID, plotArea = 400)
#' @export
calcPlotSummaries <- function(d, h, ba, vol, plotID, is_measured, plotArea) {
  data_list <- list(
    calcNHa(plotID, plotArea),
    calcVolHa(vol, plotID, plotArea),
    calcBaHa(ba, plotID, plotArea),
    calcHlor(h, ba, plotID),
    calcHdom(d, h, is_measured, plotID, plotArea)
  )
  Reduce(function(x, y) merge(x, y, by = "plotID"), data_list)
}
