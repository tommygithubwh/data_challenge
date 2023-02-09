source(file = "src/DataCleaning.R")
packages <- c("dplyr", "stringr", "tidyr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate", "hrbrthemes",
              "maps", "mapdata", "maptools", "rgdal", "leaflet", "ggmap", "rgeos", "broom", "plyr", "cartography",
              "plotly", "scales")
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

## Heat map before covid, heatmap after covid

# Time series plots -------------------------------------------------------

## Time series plot of Costs over time (£1 million)
ggplot(Ep_Drugs_Total, aes(x = Date, y = Total_Cost / 1000000)) +
  geom_line(color = "#023020") +
  xlab("") +
  ylab("Total Cost in England (£1 million)") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## Total prescriptions over time in england 
ggplot(Ep_Drugs_Total, aes(x = Date, y = Total_Items_Presc)) +
  geom_line(color = "#69b3a2") +
  xlab("") +
  ylab("Total Prescriptions in England") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## Prevalence over time in england
ggplot(Ep_Prev_Total_Eng, aes(x = Year, y = Value)) +
  geom_line(group = 1, color = "#69b3a2") +
  geom_point(color = "#69b3a2") +
  xlab("") +
  ylab('Epilepsy prevalence in England') +
  ylim(c(0.6, 0.85)) +
  geom_ribbon(aes(x = seq_along(Year), ymin = Lower.CI.95.0.limit, ymax = Upper.CI.95.0.limit),
              alpha = 0.1,
              linetype = "dashed",
              color = "grey")


## Prescriptions per 1000 people and prescription per case over time in england
ggplot(Ep_Drugs_Total, aes(x = Date)) +
  geom_line(aes(y = Presc_Per_Cases, color = "Prescriptions per case")) +
  geom_line(aes(y = Presc_Per_Population, color = "Prescriptions per 1000 population")) +
  xlab("") +
  ylab("") +
  labs(color = "") +
  ggtitle('Volume of epilepsy prescriptions over time')

ggsave('Plots/prevalence_stacked_time.png')


# stacked area chart - drugs per region over time
ggplot(Ep_Drugs_Region_Year, aes(x = Year, y = Total_Presc / 1000000, fill = Region)) +
  ylab('Prescriptions (millions)') +
  geom_area()

ggplot(Ep_Drugs_Region_Year, aes(x = Year, y = Total_Cost / 1000000, fill = Region)) +
  ylab('Prescriptions (£1 millions)') +
  geom_area()

ggplot(Ep_Prev_Region, aes(x = Year, y = Prev, fill = Region)) +
  ylab('Epilepsy prevalence') +
  ylim(c(0, 0.1)) +
  geom_area()

#Plots for change in prescriptions and costs before and after covid
# Top ten CCGs with greatest % change in volume of prescriptions 
ggplot(Covid_Data_presc %>% 
         head(10), 
       aes(x = fct_rev(fct_reorder(CCG_Name, Percent_Vol)), y = Percent_Vol, fill = CCG_Name)) +
  geom_col()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs( x = 'NHS CCG', y = 'Percentage change in prescriptions per year')+
  scale_fill_discrete(name = "NHS CCG")+
  ggtitle('Top Ten NHS CCGs: Increase in prescriptions before and after COVID-19 lockdown')

## bottom ten ccgs with lowest/most negative change in % volume of prescriptions 
ggplot(Covid_Data_presc %>% 
         tail(10), 
       aes(x = fct_rev(fct_reorder(CCG_Name, Percent_Vol)), y = Percent_Vol, fill = CCG_Name)) +
  geom_col()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs( x = 'NHS CCG', y = 'Percentage change in prescriptions per year')+
  scale_fill_discrete(name = "NHS CCG")+
  ggtitle('Bottom Ten NHS CCGs: Increase in prescriptions before and after COVID-19 lockdown')

## Top ten CCGs with the highest percentage change in cost 
ggplot(Covid_Data_Cost %>% 
         head(10), 
       aes(x = fct_rev(fct_reorder(CCG_Name, Percent_Cost)), y = Percent_Cost, fill = CCG_Name)) +
  geom_col()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs( x = 'NHS CCG', y = 'Percentage change in cost per year')+
  scale_fill_discrete(name = "NHS CCG")+
  ggtitle('Top Ten NHS CCGs: change in cost before and after COVID-19 lockdown')

## lowest change in costs CCG
ggplot(Covid_Data_Cost %>% 
         tail(10), 
       aes(x = fct_rev(fct_reorder(CCG_Name, Percent_Cost)), y = Percent_Cost, fill = CCG_Name)) +
  geom_col()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs( x = 'NHS CCG', y = 'Percentage change in cost per year')+
  scale_fill_discrete(name = "NHS CCG")+
  ggtitle('Bottom Ten NHS CCGs: change in cost before and after COVID-19 lockdown')
