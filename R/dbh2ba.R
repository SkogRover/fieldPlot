#' Calculate basal brea
#'
#' Computes the basal area of trees given diameters at breast height (dbh).
#'
#' @param dbh Numeric vector of diameter at breast height in cm.
#' @return Numeric vector of basal areas in square centimeters.
#' @examples
#' calcBa(30)
#' @export
dbh2ba <- function(d){
  ((d/100)^2) * (pi/4)
}
