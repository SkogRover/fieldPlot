#' Calculate volume per ha
#'
#' Computes the total volume of trees in each plot.
#'
#' @param trees Data frame with columns plotID and vol (volume of each tree in m3).
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with total volume (m3 ha-1) for each plotID.
#' @examples
#' calcVolHa(trees, plotArea = 400)
#' @export
calcVolHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(VolHa = sum(vol)/plotArea*10000)
}
