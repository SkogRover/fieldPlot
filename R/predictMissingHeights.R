#' This function predicts missing tree heights using species-specific height-diameter models and returns a complete height dataset.
#' Additionally, it identifies the trees with measured heights used as sample trees for model fitting.
#'
#' @param d Numeric vector representing the diameter at breast height (dbh) of trees.
#' @param h Numeric vector representing the measured heights of trees (NA for missing heights).
#' @param sp Integer vector representing species codes for each tree (e.g., 1, 2, 3, etc.).
#' @param plotID Factor or character vector indicating plot IDs for each tree.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{h_complete}: Numeric vector of tree heights, including measured heights and predicted values for missing heights.
#'   \item \code{is_measured}: Logical vector indicating which trees had measured heights (TRUE for measured, FALSE for predicted).
#' }
#'
#' @details
#' The function fits species-specific height-diameter models to predict missing tree heights.
#' Measured heights are retained, and missing values are replaced with model predictions.
#' For any missing predictions due to unavailable species-specific models, a generalized model is used.
#'
#' @examples
#' predictMissingHeights(trees)
#' @export
predictMissingHeights <- function(d, h, sp, plotID){
  H_pred <- rep(NA, length(d))
  is_measured <- !is.na(h)

  H1 <- unname(predict(
    fitHD_Plot(d[sp == 1], h[sp == 1], sp[sp == 1], plotID[sp == 1]),
    data.frame(d = d, h = h, sp = sp, plotID = plotID)
  ))

  H2 <- unname(predict(
    fitHD_Plot(d[sp == 2], h[sp == 2], sp[sp == 2], plotID[sp == 2]),
    data.frame(d = d, h = h, sp = sp, plotID = plotID)

  ))
  H3 <- unname(predict(
    fitHD_Plot(d[sp == 3], h[sp == 3], sp[sp == 3], plotID[sp == 3]),
    data.frame(d = d, h = h, sp = sp, plotID = plotID)

  ))

  H_pred[sp == 1] <- H1[sp == 1]
  H_pred[sp == 2] <- H2[sp == 2]
  H_pred[sp == 3] <- H3[sp == 3]

  model <- fitHD_Plot(d, h, sp, plotID)
  H_pred[is.na(H_pred)] <- unname(predict(model, data.frame(d = d, h = h, sp = sp, plotID = plotID)[is.na(H_pred), ]))

  h_complete <- h
  h_complete[is.na(h)] <- H_pred[is.na(h)]

  return(list(h_complete = h_complete, is_measured = is_measured))
}
