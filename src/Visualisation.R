source(file = "src/DataCleaning.R")
packages <- c("dplyr", "stringr", "tidyr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate","hrbrthemes",
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
#' 1. dataset (shapefile based),
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
display_map(ep_drugs_total_presc_shape, "Epilepsy drugs (total number of prescriptions per CCG)", "Prescriptions/CCG", "blue.pal")

# Time series plots -------------------------------------------------------

## Total prescriptions

Ep_Drugs_Total <- aggregate(Ep_Drugs_CCG$Total_Items_Presc, by =  list(Ep_Drugs_CCG$date), FUN = sum)

Ep_Drugs_Total <- Ep_Drugs_Total %>% 
  mutate(Date = as.Date(Group.1))

typeof(Ep_Drugs_Total$Date)

ggplot(Ep_Drugs_Total, aes(x=Date, y=x)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  ylab("Total Prescriptions in England") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

## Prevalence over time 






