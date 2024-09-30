#' Predict missing heights
#'
#' Predicts missing tree heights based on species-specific height-diameter models.
#'
#' @param trees Data frame with columns plotID, species, h (height), and dbh (diameter at breast height).
#' @return Data frame with predicted heights filled in for missing values.
#' @examples
#' predictMissingHeights(trees)
#' @export
predictMissingHeights <- function(trees){
  H_pred <- NA
  H1 <- unname(predict(
    fitHD_Plot(trees[trees$species==1,]), trees))
  H2 <- unname(predict(
    fitHD_Plot(trees[trees$species==2,]), trees))
  H3 <- unname(predict(
    fitHD_Plot(trees[trees$species==3,]), trees))
  H_pred[ trees$sp == 1 ] <- H1[ trees$sp == 1 ]
  H_pred[ trees$sp == 2 ] <- H2[ trees$sp == 2 ]
  H_pred[ trees$sp == 3 ] <- H3[ trees$sp == 3 ]
  model <- fitHD_Plot(trees) # for all species
  H_pred[ is.na(H_pred)] = unname(predict(model,trees[is.na(H_pred),]))
  trees$h[is.na(trees$h)] <- H_pred[is.na(trees$h)]
  return(trees)
}
