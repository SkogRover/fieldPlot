#' Predict missing heights
#'
#' Predicts missing tree heights based on species-specific height-diameter models.
#'
#' @param trees Data frame with columns plotID, species, h (height), and dbh (diameter at breast height).
#' @return Data frame with predicted heights filled in for missing values.
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
