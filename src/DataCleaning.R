# Installing Packages -----------------------------------------------------
packages <- c("tidyverse", "stringr", "ggplot2", "grid", "gridExtra", "ggpubr", "lubridate",
              "maps", "mapdata", "maptools", "rgdal", "ggmap", "rgeos", "broom", "plyr", "openxlsx")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)

# Reading in data ---------------------------------------------------------

## Reading in the data 
Ep_Drugs_ICB <- read.csv('data/Epilepsy_Drugs_By_ICB.csv')
Ep_Prev_CCG <- read.csv('data/Epilepsy_By_CCG.csv')
Ep_Prev_ICB <- read.csv('data/Epilepsy_Prevalence_ICB_2007.csv')
Ep_Drugs_PCN <- read.csv('data/Epilepsy_Drugs_By_PCN.csv')
CCG_ICB_Codes <- read.xlsx('data/CCG_IBS_Region.xlsx')
PCN_CCG_Codes <- read.xlsx('data/B1357-PCN-Adjusted-Populations-V1.3-updated-31-March.xlsx', sheet = 'PCNs')
PCN_CCG_Codes2 <- read.xlsx('data/CCG_TO_PCN.xlsx')

# Data Cleaning PCN/CCGs/ICBs ---------------------------------------------

## PCNs to CCGs 
colnames(PCN_CCG_Codes) <- PCN_CCG_Codes[1,]

## Selecting necessary rows and cols
PCN_CCG_Codes <- PCN_CCG_Codes[-1, c(1, 2, 5, 6)]

## Renaming columns 
colnames(PCN_CCG_Codes) <- c('PCN_Code', 'PCN_Name', 'CCG_Code', 'CCG_Name')

## Need to convert ICBs to CCG
## selecting necessary columns and rows 
CCG_ICB_Codes <- CCG_ICB_Codes[-c(1, 2), 2:6]

## Cleaning the second lookup named PCN_CCG_Codes 2
colnames(PCN_CCG_Codes2) <- c('CCG_Name', 'PCN_Name')

## Need all strings in upper case 
PCN_CCG_Codes2 <- PCN_CCG_Codes2 %>%
  mutate_at(vars(PCN_Name), ~toupper(PCN_Name))

## Renaming columns 
colnames(CCG_ICB_Codes) <- c('Region', 'ICB_Code', 'ICB_Name', 'CCG_Code', 'CCG_Name')

## Filling in missing values from xlsx file
for (j in 1:ncol(CCG_ICB_Codes)) {
  for (i in 1:nrow(CCG_ICB_Codes)) {
    CCG_ICB_Codes[i, j] <- ifelse(is.na(CCG_ICB_Codes[i, j]), CCG_ICB_Codes[i - 1, j], CCG_ICB_Codes[i, j])
  } }

## Now have a data set containing ICB info for all CCGs


# Converting drug PCNs to CCGs --------------------------------------------

## Renamming columns 
colnames(Ep_Drugs_PCN)[2:5] <- c('PCN_Code', 'PCN_Name', 'Total_Items_Presc', 'Total_Cost')

Ep_Drugs_PCN <- Ep_Drugs_PCN[, 1:5]

## Joining the datasets

Ep_Drugs_CCG <- Ep_Drugs_PCN %>%
  left_join(PCN_CCG_Codes, by = c('PCN_Code', 'PCN_Name'), all.x = TRUE)

## Some CCG's remain missing, a second lookup sheet will be used to assign these

Missing_CCGs <- Ep_Drugs_CCG %>%
  filter(is.na(CCG_Code)) %>%
  select(PCN_Code, PCN_Name) %>%
  distinct(PCN_Name, .keep_all = TRUE)

## Using the second lookup for the missing ID's

Missing_CCGs <- Missing_CCGs %>%
  left_join(PCN_CCG_Codes2, by = 'PCN_Name') %>%
  distinct(PCN_Name, .keep_all = TRUE)

## Assumptions made:  PCN Sittingbourne and Sittingbourne West are in the same CCG
## Harness Temple PCN is in the same CCG as Harness South and North PCN 
## Felixstowe PCN is in NHS IPSWICH AND EAST SUFFOLK CCG
## Central southport is in the same CCG as Southport and formby
## Smart acte kings heath is in NHS BIRMINGHAM AND SOLIHULL CCG
## Central Telford is in NHS SHROPSHIRE, TELFORD AND WREKIN CCG
## Connect health alliance does not exist
## NORTH west suffolk PCN is in NHS WEST SUFFOLK CCG

Missing_CCGs[Missing_CCGs$PCN_Name == 'SITTINGBOURNE WEST PCN', 3] <- 'NHS KENT AND MEDWAY CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'HARNESS TEMPLE PCN', 3] <- 'NHS NORTH WEST LONDON CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'FELIXSTOWE PCN', 3] <- 'NHS IPSWICH AND EAST SUFFOLK CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'CENTRAL SOUTHPORT PCN', 3] <- 'NHS SOUTHPORT AND FORMBY CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'SMARTCARE KINGS HEATH PCN', 3] <- 'NHS BIRMINGHAM AND SOLIHULL CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'CENTRAL TELFORD PCN', 3] <- 'NHS SHROPSHIRE, TELFORD AND WREKIN CCG'
Missing_CCGs[Missing_CCGs$PCN_Name == 'NORTH WEST SUFFOLK PCN', 3] <- 'NHS WEST SUFFOLK CCG'

Missing_CCGs <- Missing_CCGs %>%
  filter(!is.na(CCG_Name))

## Merging CCGs from the second look up sheet 

Missing_CCGs <- Missing_CCGs %>%
  left_join(Ep_Drugs_PCN, by = c('PCN_Name', 'PCN_Code'))

## Combing the two datasets so that they can be aggregated by CCG

Missing_CCGs <- Missing_CCGs %>%
  select(date, PCN_Code, PCN_Name, Total_Items_Presc, Total_Cost, CCG_Name)

Ep_Drugs_CCG <- Ep_Drugs_CCG %>%
  select(-CCG_Code) %>%
  filter(!is.na(CCG_Name))

Ep_Drugs_CCG <- Ep_Drugs_CCG %>%
  bind_rows(Missing_CCGs)

## Double checking there are no duplicate values across: date, and PCN name. all good

Ep_Drugs_CCG %>%
  group_by(PCN_Name) %>%
  filter(duplicated(date))

## Now aggregating across PCNs so we have CCG level data 

Ep_Drugs_CCG <- aggregate(Ep_Drugs_CCG[, 4:5], by = list(Ep_Drugs_CCG$date,
                                                         Ep_Drugs_CCG$CCG_Name), FUN = sum)

## Changing the column names 

colnames(Ep_Drugs_CCG) <- c('date', 'CCG_Name', 'Total_Items_Presc', 'Total_Cost')

## Still need to deal with some of the CCG character strings

Ep_Drugs_CCG <- Ep_Drugs_CCG %>%
  mutate(CCG_Name = toupper(CCG_Name))

table(Ep_Drugs_CCG$CCG_Name)

List_Of_CCGS <- Ep_Prev_CCG %>%
  mutate(CCG_Name_Prev = toupper(Area.Name)) %>%
  select(CCG_Name_Prev) %>%
  distinct()

Ep_Drugs_CCG$CCG_Name <- gsub('ICB - ([0-9].*[A-Z]*|[A-Z]*[0-9]*[A-Z]*[0-9]*[A-Z]*)$', 'CCG', as.character(Ep_Drugs_CCG$CCG_Name))


