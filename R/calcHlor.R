#' Calculate Lorey's mean height
#'
#' Computes the mean height of the largest trees in each plot, weighted by basal area.
#'
#' @param trees Data frame with columns plotID, h (height), and ba (basal area).
#' @return Data frame with plotID and the mean height of the largest trees (Hlor).
#' @examples
#' CalcHlor(trees)
#' @export
calcHlor <- function(trees){
  trees %>%
    group_by(plotID) %>%
    summarise(Hlor = weighted.mean(h, ba))
}
