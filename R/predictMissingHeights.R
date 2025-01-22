#' This function predicts missing tree heights using species-specific height-diameter models and returns a vector of complete heights.
#'
#' @param d Numeric vector representing the diameter at breast height (dbh) of trees.
#' @param h Numeric vector representing the measured heights of trees (NA for missing heights).
#' @param sp Integer vector representing species codes for each tree (e.g., 1, 2, 3, etc.).
#' @param plotID Factor or character vector indicating plot IDs for each tree.
#'
#' @return A numeric vector (\code{h_complete}) of tree heights, including measured heights and predicted values for missing heights.
#'
#' @details
#' The function fits species-specific height-diameter models to predict missing tree heights.
#' Measured heights are retained, and missing values are replaced with model predictions.
#' For any missing predictions due to unavailable species-specific models, a generalized model is used.
#'
#' @examples
#' predictMissingHeights(trees$d,trees$h,trees$sp,trees$plotID)
#' @export
predictMissingHeights <- function(d, h, sp, plotID){
  H_pred <- rep(NA, length(d))
  H1 <- unname(predict(fitHD_Plot(d[sp == 1], h[sp == 1], sp[sp ==
                                                               1], plotID[sp == 1]), data.frame(d = d, h = h, sp = sp,
                                                                                                plotID = plotID)))
  H2 <- unname(predict(fitHD_Plot(d[sp == 2], h[sp == 2], sp[sp ==
                                                               2], plotID[sp == 2]), data.frame(d = d, h = h, sp = sp,
                                                                                                plotID = plotID)))
  H3 <- unname(predict(fitHD_Plot(d[sp == 3], h[sp == 3], sp[sp ==
                                                               3], plotID[sp == 3]), data.frame(d = d, h = h, sp = sp,
                                                                                                plotID = plotID)))
  H_pred[sp == 1] <- H1[sp == 1]
  H_pred[sp == 2] <- H2[sp == 2]
  H_pred[sp == 3] <- H3[sp == 3]
  H_pred[is.na(H_pred)] <- unname(predict(fitHD_Plot(d, h, sp, plotID), data.frame(d = d,
                                                                                   h = h, sp = sp, plotID = plotID)[is.na(H_pred), ]))
  H_pred[is.na(H_pred)] <- unname(predict(fitHD(d, h), data.frame(d = d,
                                                                  h = h)[is.na(H_pred), ]))
  h_complete <- h
  h_complete[is.na(h)] <- H_pred[is.na(h)]
  return(h_complete)
}
