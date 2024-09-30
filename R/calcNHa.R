#' Calculate number of trees per ha
#'
#' Computes the density of trees in each plot, expressed as the number of trees per hectare.
#'
#' @param trees Data frame with columns plotID and n (number of trees).
#' @param plotArea Numeric value representing the plot area in m??.
#' @return Data frame with plotID and tree density (N).
#' @examples
#' calcN(trees, plotArea = 250)
#' @export
calcNHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(NHa = round(n()/plotArea*10000))
}
