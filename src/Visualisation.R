packages <- c("dplyr", "stringr", "tidyr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate",
              "maps", "mapdata", "maptools", "rgdal", "ggmap", "rgeos", "broom", "plyr")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)

#Create a heatmap (CCG based, England only so far). It can be used for any data in our.csv.
#For example to display prevalence in ep_prevalence_ccg.csv
shapefile <- readOGR("data/ccg", "Clinical_Commissioning_Groups_April_2020_FEB_EN")
mapdata <- tidy(shapefile)
gg <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25)
gg <- gg + coord_fixed(1)
print(gg)


