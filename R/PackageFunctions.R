

#' Calculate Basal Area (BA)
#'
#' Computes the basal area of trees given diameters at breast height (dbh).
#'
#' @param dbh Numeric vector of diameter at breast height in cm.
#' @return Numeric vector of basal areas in square centimeters.
#' @examples
#' calcBa(30)
#' @export
dbh2ba <- function(dbh){
  ((dbh/100)^2) * (pi/4)
}

#' Calculate Tree Density
#'
#' Computes the density of trees in each plot, expressed as the number of trees per hectare.
#'
#' @param trees Data frame with columns plotID and n (number of trees).
#' @param plotArea Numeric value representing the plot area in m??.
#' @return Data frame with plotID and tree density (N).
#' @examples
#' calcN(trees, plotArea = 250)
#' @export
calcNHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(NHa = round(n()/plotArea*10000))
}

#' Calculate Lorey's mean height
#'
#' Computes the mean height of the largest trees in each plot, weighted by basal area.
#'
#' @param trees Data frame with columns plotID, h (height), and ba (basal area).
#' @return Data frame with plotID and the mean height of the largest trees (Hlor).
#' @examples
#' CalcHlor(trees)
#' @export
calcHlor <- function(trees){
  trees %>%
    group_by(plotID) %>%
    summarise(Hlor = weighted.mean(h, ba))
}

#' Calculate Dominant Height
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

#' Calculate Volume per ha
#'
#' Computes the total volume of trees in each plot.
#'
#' @param trees Data frame with columns plotID and vol (volume of each tree in m3).
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with total volume (m3 ha-1) for each plotID.
#' @examples
#' calcVolHa(trees, plotArea = 400)
#' @export
calcVolHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(VolHa = sum(vol)/plotArea*10000)
}

#' Calculate Basal Area
#'
#' Computes the total basal area of trees in each plot.
#'
#' @param trees Data frame with columns plotID and ba (basal area of each tree in m2).
#' @param plotArea Numeric value representing the plot area in m2.
#' @return Data frame with plotID and total basal area (in m2 ha-1).
#' @examples
#' calcBa(trees, plotArea = 400)
#' @export
calcBaHa <- function(trees, plotArea){
  trees %>%
    group_by(plotID) %>%
    summarise(BaHa = sum(ba)/plotArea*10000)
}

#' Fit Height-Diameter Model
#'
#' Fits a height-diameter model.
#'
#' @param trees Data frame with columns h (height in m) and dbh (diameter at breast height in cm).
#' @return A fitted nonlinear least squares model.
#' @references
#' Naslund, M. (1936). Thinning experiments in pine forest conducted by the forest experiment station. Meddelanden fran Statens Skogsforsoksanstalt, 29, 1-169.
#' @examples
#' fitHD(trees)
#' @export
fitHD <- function(trees) {
  data1 <- trees[!is.na(data[[h]] * trees$dbh), ]
    fit <- nls(h ~ 1.3 + (dbh / (a + b * dbh))^3,
               data = data1,
               start = c(a = 1.5, b = 0.34))
 return(fit)
}

#' Fit Height-Diameter Model by Plot
#'
#' Fits a height-diameter model to each plot, allowing for random effects.
#'
#' @param trees Data frame with columns plotID, h (height in m), and dbh (diameter at breast height in cm).
#' @return A fitted mixed-effects nonlinear model.
#' #' @references
#' Sharma, R. P.and Breidenbach, J. (2015). Modeling height-diameter relationships for Norway spruce, Scots pine, and downy birch using Norwegian national forest inventory data. Forest Science and Technology, 11(1), 44-53.
#' @examples
#' fitHD_Plot(trees)
#' @export
fitHD_Plot <- function(trees) {
  require(nlme)
  data1 <- trees[!is.na(trees$h * trees$dbh), ] # trees with measured height
  start <- c(1.5, 0.34) # start values
  fit <- NULL
  nlme(h ~ 1.3 + (dbh / (a + b * dbh))^3,
                 data = data1,
                 start = start,
                 fixed = list(as.formula("a~1"), as.formula("b~1")),
                 random = list(plotID = list(a ~ 1)),
                 control = nlmeControl(tol = 1e-6, maxIter = 100))
}

#' Predict Missing Heights
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

#' Calculate plot summaries
#'
#' This function computes summaries for each plot of number of trees per ha, Lorey's height (Hlor), dominant height, volume per hectare, and basal area per hectare.
#'
#' @param trees Data frame containing plotID, height of each tree ("h" in m) basal area ("ba" in m2), and volume ("vol" in m3).
#'
#' @seealso calcNHa, calcHlor, calcHdom, calcVolHa, calcBaHa
#'
#' @return A data frame with plot-level summaries including the following columns:
#' - `plotID`: Identifier for each plot.
#' - `N`: Tree density per hectare.
#' - `Hlor`: Lorey's height.
#' - `Hdom`: Dominant height.
#' - `Vol`: Volume per hectare.
#' - `Ba`: Basal area per hectare.
#'
#' @examples
#' result <- calcPlotSummaries(trees)
#'
#' @export
calcPlotSummaries <- function(trees){

  data_list <- list(
    calcNHa(trees, plotArea = 400),
    calcHlor(trees),
    calcHdom(trees, plotArea = 400),
    calcVolHa(trees, plotArea = 400),
    calcBaHa(trees, plotArea = 400)
  )

  Reduce(function(x, y) merge(x, y, by = "plotID"), data_list)

}
