#' Calculate plot summaries
#'
#' This function computes summaries for each plot of number of trees per ha, Lorey's height (Hlor), dominant height, volume per hectare, and basal area per hectare.
#'
#' @param trees Data frame containing plotID, height of each tree ("h" in m) basal area ("ba" in m2), and volume ("vol" in m3).
#'
#' @seealso calcNHa, calcHlor, calcHdom, calcVolHa, calcBaHa
#'
#' @return A data frame with plot-level summaries including the following columns:
#' - `plotID`: Identifier for each plot.
#' - `N`: Tree density per ha
#' - `Hlor`: Lorey's height.
#' - `Hdom`: Dominant height.
#' - `Vol`: Volume per ha
#' - `Ba`: Basal area per ha
#'
#' @examples
#' result <- calcPlotSummaries(trees)
#'
#' @export
calcPlotSummaries <- function(trees){
  data_list <- list(
    calcNHa(trees, plotArea = 400),
    calcHlor(trees),
    calcHdom(trees, plotArea = 400),
    calcVolHa(trees, plotArea = 400),
    calcBaHa(trees, plotArea = 400)
  )
  Reduce(function(x, y) merge(x, y, by = "plotID"), data_list)
}
