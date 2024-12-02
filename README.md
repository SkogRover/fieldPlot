# fieldPlot

fieldPlot provides functions for handling field data in forest inventories based on sample plots. Functionality includes calculating basal area for trees, predicting volume using Norwegian volume models, predicting missing heights using height-diameters fitted with diameters and heights of sample trees, calculating the number of stems per ha for plots, as well as Lorey's mean height, dominant height, volume per ha and basal area per ha.  


# Example use
```r
# installation
devtools::install_github("https://github.com/SkogRover/fieldPlot")
library(fieldPlot)

# Example data
trees <- readRDS(
              system.file("extdata",
              "exampleData.Rdata",
              package = "fieldPlot")
                )

# predict missing tree heights
trees$h_complete <- predictMissingHeights(trees$d,
                               trees$h,
                               trees$sp,
                               trees$plotID)

# calculate basal area
trees$ba <- d2ba(trees$d)

# predict tree volumes
trees$vol=taperNOR::volume(dbh=trees$d,
                           h_top=trees$h_complete,
                           sp=trees$sp)

# calculate plot summaries
calcPlotSummaries(d =trees$d,
                  h = trees$h_complete,
                  ba = trees$ba,
                  vol =trees$vol,
                  plotID = trees$plotID,
                  plotArea = 400)

```
# Funding details

The fieldPlot package was developed as part of the project SmartForest, funded by the Research council of Norway (project no. 309671). 
