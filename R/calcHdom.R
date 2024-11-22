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
calcHdom <- function(d, h, is_measured, plotID, plotArea){

  trees <- data.frame(
    plotID = plotID,
    d = d,
    h = h,
    is_measured = is_measured
  )

  trees <- trees %>%
    group_by(plotID) %>%
    mutate(domTree = rank(-d, ties.method = "min") <= ceiling(plotArea / 100)) %>%
    ungroup()

  Hdom <- trees %>%
    group_by(plotID) %>%
    summarise(Hdom = case_when(
      sum(domTree & is_measured) == 2 ~ mean(h[domTree & is_measured]),
      sum(domTree & is_measured) == 1 ~ mean(h[domTree]) * sum(h[domTree & is_measured]) / sum(h[domTree]),
      TRUE ~ mean(h[domTree])
    )) %>%
    ungroup()

  return(Hdom)
}
