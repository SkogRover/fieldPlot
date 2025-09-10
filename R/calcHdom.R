#' Calculate dominant height
#'
#' Computes the mean height of the dominant trees on each plot.
#'
#' @param d Numeric vector of diameter at breast height (cm) for all trees.
#' @param h Numeric vector of heights (m). Must be complete (no \code{NA}).
#' @param plotID Factor or numeric vector with plot identifiers.
#' @param plotArea Numeric scalar, plot area in m??. Default: 250.
#'
#' @return Data frame with \code{plotID} and dominant height (\code{Hdom}).
#'
#' @examples
#' # calcHdom(d = trees$d, h = trees$h_complete, plotID = trees$plotID, plotArea = 400)
#'
#' @export
calcHdom <- function(d, h, plotID, plotArea = 250) {
  # deps
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install 'dplyr'.")
  }
  # input checks
  if (!is.numeric(plotArea) || length(plotArea) != 1L || is.na(plotArea) || plotArea <= 0) {
    stop("'plotArea' must be a positive numeric scalar (m^2).")
  }
  if (any(is.na(h))) stop("'h' must be complete (no NA).")

  # round to nearest 100 m??, ties down (e.g., 250 -> 200)
  round100_ties_down <- function(x) {
    r <- x %% 100
    down <- x - r
    if (r < 50) down else if (r > 50) down + 100 else down
  }
  n_dom <- as.integer(round100_ties_down(plotArea) / 100)
  if (n_dom < 1L) n_dom <- 1L

  # compute Hdom
  dplyr::tibble(plotID = plotID, d = d, h = h) |>
    dplyr::group_by(plotID) |>
    dplyr::arrange(dplyr::desc(d), .by_group = TRUE) |>
    dplyr::slice_head(n = n_dom) |>
    dplyr::summarise(Hdom = mean(h), .groups = "drop")
}
