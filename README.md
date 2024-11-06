# fieldPlot

fieldPlot provides functions for handling field data in forest inventories based on sample plots. Functionality includes calculating basal area and volume for trees, predicting missing heights using height-diameters fitted with diameters and heights of sample trees, calculating the number of stems per ha for plots, as well as Lorey's mean height, dominant height, volume per ha and basal area per ha.  


# Example use
```r
# installation
devtools::install_github("[https://github.com/lennartnoordermeer/FieldPlot](https://github.com/SkogRover/fieldPlot)")
library(fieldPlot)

# Example data
trees <- readRDS(
              system.file("extdata",
              "exampleData.Rdata",
              package = "FieldPlot")
                )

# predict missing tree heights
trees <- predictMissingHeights(trees)

# calculate basal area
trees$ba <- dbh2ba(trees$dbh)

# predict tree volumes
trees$vol=taperNOR::volume(dbh=trees$dbh,
                           h_top=trees$h,
                           sp=trees$species)

# calculate plot summaries
calcPlotSummaries(trees)

```
# Funding details

The fieldPlot package was developed as part of the project SmartForest, funded by the Research council of Norway (project no. 309671). 
