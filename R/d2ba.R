#' Calculate basal area
#'
#' Computes the basal area of trees given diameters at breast height (dbh).
#'
#' @param d Numeric vector of diameters at breast height in cm.
#' @return Numeric vector of basal areas in m2.
#' @examples
#' d2ba(d = trees$d)
#' @export
d2ba <- function(d) {
  ((d / 100)^2) * (pi / 4)
}
