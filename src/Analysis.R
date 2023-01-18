packages <- c("tidyverse", "stringr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate",
              "maps", "mapdata", "maptools", "rgdal", "ggmap", "rgeos", "broom", "plyr", "fuzzyjoin",
              "openxlsx")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)

## Reading in the data
Ep_Drugs_ICB <- read.csv('data/Epilepsy_Drugs_By_ICB.csv')
Ep_Prev_CCG <- read.csv('data/Epilepsy_Prevalence_By_CCG.csv')
Ep_Prev_ICB <- read.csv('data/Epilepsy_Prevalence_ICB_2007.csv')
CCG_ICB_Codes <- read.xlsx('data/CCG_IBS_Region.xlsx')

## Need to convert ICBs to CCG
## selecting necessary columns and rows 
CCG_ICB_Codes <- CCG_ICB_Codes[-c(1, 2), 2:6]

## Renaming columns 
colnames(CCG_ICB_Codes) <- c('Region', 'ICB_Code', 'ICB_Name', 'CCG_Code', 'CCG_Name')

## Filling in missing values from xlsx file
for (j in seq_len(ncol(CCG_ICB_Codes))) {
  for (i in seq_len(nrow(CCG_ICB_Codes))) {
    CCG_ICB_Codes[i, j] <- ifelse(is.na(CCG_ICB_Codes[i, j]), CCG_ICB_Codes[i - 1, j], CCG_ICB_Codes[i, j])
  } }

## Now have a data set containing ICB info for all CCGs 

## Going to aggregate the epilepsy drugs data by year
Ep_Drugs_ICB <- Ep_Drugs_ICB %>%
  mutate_at(vars(date), ~as.Date(date)) %>%
  mutate(Year = year(date))

Ep_Drugs_ICB_By_Year <- aggregate(y_items ~ Year + name, data = Ep_Drugs_ICB, FUN = sum) %>%
  left_join(aggregate(y_actual_cost ~ Year + name, data = Ep_Drugs_ICB, FUN = sum), by = c('Year', 'name'))

Ep_Prev_ICB_2 <- Ep_Prev_ICB %>%
  filter(Area.Type != 'England') %>%
  select(Area.Name, Time.period, Time.period.Sortable, Value, Count, Denominator) %>%
  mutate(name = substr(toupper(Area.Name), 1, nchar(Area.Name) - 1), Year = substr(as.character(Time.period.Sortable), 1, 4))

## Joining the datasets
Master_Data <- Ep_Prev_ICB_2 %>%
  select(name, Year, Value, Count, Denominator) %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::rename('Ep_Prop' = 'Value', 'Ep_Count' = 'Count', 'Total_Pats' = 'Denominator') %>%
  left_join(Ep_Drugs_ICB_By_Year, by = c('name', 'Year'),) %>%
  dplyr::rename('Total_Presc' = 'y_items', 'Total_Cost' = 'y_actual_cost') %>%
  format(scientific = FALSE)

