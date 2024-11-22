#' Calculate Lorey's mean height
#'
#' Computes the mean height of trees in each plot, weighted by basal area.
#'
#' @param h Numeric vector of tree heights in m.
#' @param ba Numeric vector of basal areas of trees in m2.
#' @param plotID Factor or character vector identifying plots.
#' @return Data frame with `plotID` and Lorey's mean height (`Hlor`).
#' @examples
#' calcHlor(trees$h_complete, trees$ba, trees$plotID)
#' @export
calcHlor <- function(h, ba, plotID) {
  data.frame(plotID = plotID, h = h, ba = ba) %>%
    group_by(plotID) %>%
    summarise(Hlor = weighted.mean(h, ba, na.rm = TRUE))
}
