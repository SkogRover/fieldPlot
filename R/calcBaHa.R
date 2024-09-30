#' Calculate basal area per ha
#'
#' Computes the basal area in each plot, scaled to per ha values.
#'
#' @param trees Data frame with columns plotID and ba (basal area of each tree in m2).
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with plotID and total basal area (in m2 ha-1).
#' @examples
#' calcBa(trees, plotArea = 400)
#' @export
calcBaHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(BaHa = sum(ba)/plotArea*10000)
}
