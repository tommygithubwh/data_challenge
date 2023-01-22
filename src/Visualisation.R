source(file = "src/DataCleaning.R")
packages <- c("dplyr", "stringr", "tidyr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate",
              "maps", "mapdata", "maptools", "rgdal", "leaflet", "ggmap", "rgeos", "broom", "plyr", "cartography")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)

source(file = "src/DataCleaning.R")
# Create a heatmap (CCG based, England only so far). It can be used for any data in our.csv.
# Detailed info on cartography here: https://search.r-project.org/CRAN/refmans/cartography/html/choroLayer.html
shapefile <- readOGR("data/ccg", "Clinical_Commissioning_Groups_April_2020_FEB_EN")
shp <- SpatialPolygonsDataFrame(gSimplify(shapefile, tol = 0.01, topologyPreserve = TRUE), data = shapefile@data)
shp@data$ccg20nm <- toupper(shp@data$ccg20nm)

#' Display the map
#' Params:
#' 1. dataset,
#' 2. title of the plot
#' 3. title of the description
#' 4. (optional) palette ("blue.pal", "red.pal" etc.)
display_map <- function(dataset, title, title_text, pal = "green.pal") {
  temp_shape <- SpatialPolygonsDataFrame(shp, data = dataset, match.ID = F)
  choroLayer(spdf = temp_shape,
             df = temp_shape@data,
             method = "arith",
             col = carto.pal(pal1 = pal, n1 = 6),
             border = "grey40",
             var = "x",
             nclass = 6,
             legend.title.txt = title_text)
  title(title)
}

ep_drugs_total_cost <- Ep_Drugs_CCG %>%
  as.data.frame() %>%
  select(CCG_Name, Total_Cost)

ep_drugs_total_presc <- Ep_Drugs_CCG %>%
  as.data.frame() %>%
  select(CCG_Name, Total_Items_Presc)

ep_drugs_total_cost_shape <- shp@data %>%
  left_join(aggregate(ep_drugs_total_cost$Total_Cost, list(ep_drugs_total_cost$CCG_Name), FUN = sum), by = c('ccg20nm' = 'Group.1'), all.x = TRUE) %>%
  mutate(x = coalesce(x, 0))

ep_drugs_total_presc_shape <- shp@data %>%
  left_join(aggregate(ep_drugs_total_presc$Total_Items_Presc, list(ep_drugs_total_presc$CCG_Name), FUN = sum), by = c('ccg20nm' = 'Group.1'), all.x = TRUE) %>%
  mutate(x = coalesce(x, 0))

#' Display maps: uncomment the ones you need
display_map(ep_drugs_total_cost_shape, "Epilepsy drugs (total cost per CCG)", "Cost/CCG")
#display_map(ep_drugs_total_presc_shape, "Epilepsy drugs (total number of prescriptions per CCG)", "Prescriptions/CCG", "blue.pal")