#' Calculate dominant height
#'
#' Computes the mean height of the dominant trees on each plot.
#'
#' @param d Numeric vector of diameter at breast height, in cm of all trees.
#' @param h Numeric vector of heights in m. Must be complete (no \code{NA} values).
#' @param plotID Factor or numeric vector representing the plot identifiers.
#' @param plotArea Numeric value representing the plot area in m2. Defaults to 250 m2. (Note: currently unused in the function).
#' @return Data frame with plotID and dominant height (Hdom).
#' @examples
#' calcHdom(trees, plotArea = 400)
#' @export
calcHdom <- function(d, h, plotID, plotArea=250){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for this function. Please install it using install.packages('dplyr').")
  }
  require(dplyr)

  return(data.frame(
                    plotID = plotID,
                    d = d,
                    h = h) %>%
           group_by(plotID) %>%
           arrange(desc(d)) %>%
           slice_head(n = 2) %>%
           summarise(Hdom = mean(h)))
}
