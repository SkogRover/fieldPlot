#' Calculate dominant height
#'
#' Computes the mean height of the dominant trees in each plot.
#'
#' @param trees Data frame with columns plotID, dbh (diameter at breast height), and h (height).
#' @param plotArea Numeric value representing the plot area in m??.
#' @return Data frame with plotID and the mean dominant height (Hdom).
#' @examples
#' calcHdom(trees, plotArea = 1000)
#' @export
calcHdom <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    arrange(desc(dbh)) %>%
    slice_head(n = round(plotArea/100)) %>%
    summarise(Hdom = mean(h))
}
