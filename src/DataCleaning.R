# Installing Packages -----------------------------------------------------
packages <- c("tidyverse", "stringr", "ggplot2", "grid", "gridExtra", "lubridate",
              "maps", "mapdata", "maptools", "rgdal", "ggmap", "rgeos", "broom", "openxlsx")
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
CCG_IMDs <- read.csv('data/CCG_IMDs.csv')

# Data Cleaning PCN/CCGs/ICBs ---------------------------------------------

## PCNs to CCGs 
colnames(PCN_CCG_Codes) <- PCN_CCG_Codes[1,]

## Selecting necessary rows and cols
PCN_CCG_Codes3 <- PCN_CCG_Codes[-1,]
PCN_CCG_Codes <- PCN_CCG_Codes[-1, c(1, 2, 5, 6)]

## Renaming columns 
colnames(PCN_CCG_Codes) <- c('PCN_Code', 'PCN_Name', 'CCG_Code', 'CCG_Name')

## Need to convert ICBs to CCG
## selecting necessary columns and rows 
CCG_ICB_Codes <- CCG_ICB_Codes[-1, 2:6]

## Cleaning the second lookup named PCN_CCG_Codes 2
colnames(PCN_CCG_Codes2) <- c('CCG_Name', 'PCN_Name')

## Need all strings in upper case 
PCN_CCG_Codes2 <- PCN_CCG_Codes2 %>%
  mutate_at(vars(PCN_Name), ~toupper(PCN_Name))

## Renaming columns 
colnames(CCG_ICB_Codes) <- c('Region', 'ICB_Code', 'ICB_Name', 'CCG_Code', 'CCG_Name')

## Filling in missing values from xlsx file
for (j in seq_len(ncol(CCG_ICB_Codes))) {
  for (i in seq_len(nrow(CCG_ICB_Codes))) {
    CCG_ICB_Codes[i, j] <- ifelse(is.na(CCG_ICB_Codes[i, j]), CCG_ICB_Codes[i - 1, j], CCG_ICB_Codes[i, j])
  }
}

## Now have a data set containing ICB info for all CCGs

# Converting drug PCNs to CCGs --------------------------------------------

## Renaming columns
colnames(Ep_Drugs_PCN)[2:5] <- c('PCN_Code', 'PCN_Name', 'Total_Items_Presc', 'Total_Cost')

Ep_Drugs_PCN <- Ep_Drugs_PCN[, 1:5]

## Joining the datasets
Ep_Drugs_CCG <- Ep_Drugs_PCN %>%
  left_join(PCN_CCG_Codes, by = c('PCN_Code', 'PCN_Name'), all.x = TRUE) %>%
  dplyr::mutate(Date = as.Date(date))

## Some CCG's remain missing, a second lookup sheet will be used to assign these
Missing_CCGs <- Ep_Drugs_CCG %>%
  filter(is.na(CCG_Code)) %>%
  select(PCN_Code, PCN_Name) %>%
  distinct(PCN_Name, .keep_all = TRUE)

## Using the second lookup for the missing ID's
Missing_CCGs <- Missing_CCGs %>%
  left_join(PCN_CCG_Codes2, by = 'PCN_Name') %>%
  distinct(PCN_Name, .keep_all = TRUE)

## Assumptions made::  PCN Sittingbourne and Sittingbourne West are in the same CCG
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

## Double checking there are no duplicate values across:: date, and PCN name. all good
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
  dplyr::mutate(CCG_Name = toupper(CCG_Name))

table(Ep_Drugs_CCG$CCG_Name)

List_Of_CCGS <- Ep_Prev_CCG %>%
  dplyr::mutate(CCG_Name_Prev = toupper(Area.Name)) %>%
  select(CCG_Name_Prev) %>%
  distinct()

Ep_Drugs_CCG$CCG_Name <- gsub('ICB - ([0-9].*[A-Z]*|[A-Z]*[0-9]*[A-Z]*[0-9]*[A-Z]*)$', 'CCG', as.character(Ep_Drugs_CCG$CCG_Name))

## Adding Index multiple deprivation and Health Deprivation and Disability Decile to EP_Drugs_CCG
Ep_Drugs_CCG <- Ep_Drugs_CCG %>%
  left_join(CCG_IMDs, by = c('CCG_Name' = 'CCG_Name'))

Ep_Drugs_CCG <- Ep_Drugs_CCG %>%
  dplyr::mutate(Year = year(date), Date = as.Date(date))


## Total prescriptions 
Ep_Drugs_Total <- aggregate(Ep_Drugs_CCG$Total_Items_Presc, by = list(Ep_Drugs_CCG$date), FUN = sum)

Ep_Drugs_Total <- Ep_Drugs_Total %>%
  dplyr::mutate(Date = as.Date(Group.1)) %>%
  dplyr::mutate(Year = year(Date))

Ep_Drugs_Total_Vol <- Ep_Drugs_Total %>% dplyr::rename(Total_Presc = x)
Ep_Drugs_Total_Vol <- Ep_Drugs_Total_Vol[, "Total_Presc"]

# Epilepsy Prevalence -----------------------------------------------------
Ep_Prev_Total_Eng <- Ep_Prev_CCG %>%
  filter(Area.Type == 'England' & Category == '') %>%
  dplyr::mutate(Year = substr(Time.period, 1, 4)) %>%
  distinct(Year, .keep_all = TRUE)

Ep_Drugs_Total <- Ep_Drugs_Total %>%
  inner_join(Ep_Prev_Total_Eng %>%
               select(13:19, Year) %>%
               dplyr::mutate(Year = as.numeric(Year))) %>%
  dplyr::rename(Total_Presc = x)

Ep_Drugs_Total <- Ep_Drugs_Total %>%
  dplyr::mutate(Presc_Per_Cases = Total_Presc / Count,
                Presc_Per_Population = (Total_Presc / Denominator) * 1000)

# Population per CCG  -----------------------------------------------------

## two calculations for popualtion by CCG
PCN_CCG_Codes3 <- PCN_CCG_Codes3 %>%
  dplyr::rename(CCG_Name = `Parent CCG 1 Jan 22`,
                PCN_Population = `PCN adjusted population 1 Jan 22`) %>%
  select(CCG_Name, PCN_Population)

PCN_CCG_Codes3 <- PCN_CCG_Codes3 %>%
  group_by(CCG_Name) %>%
  dplyr::mutate(CCG_Population = sum(as.numeric(PCN_Population))) %>%
  distinct(CCG_Name, .keep_all = TRUE)

## Adding prevalence by CCG 
Ep_Prev_CCG <- Ep_Prev_CCG %>%
  filter(Area.Name != 'England') %>%
  select(Area.Name, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit,
         Count, Denominator, Time.period) %>%
  dplyr::rename(CCG_Name = Area.Name)

PCN_CCG_Codes3 <- PCN_CCG_Codes3 %>%
  dplyr::mutate(Lower.CI.95.0.limit = NA, Upper.CI.95.0.limit = NA, Count = NA, Time.period = 2022, Prev = NA) %>%
  select(-PCN_Population, CCG_Name, Prev, Lower.CI.95.0.limit, Upper.CI.95.0.limit,
         Count, CCG_Population, Time.period)

Ep_Prev_CCG <- Ep_Prev_CCG %>%
  dplyr::rename(CCG_Population = Denominator, Prev = Value) %>%
  select(CCG_Name, CCG_Population, Lower.CI.95.0.limit, Upper.CI.95.0.limit, Count, Time.period, Prev)

Ep_Prev_CCG <- Ep_Prev_CCG %>%
  dplyr::mutate(Time.period = as.numeric(substr(Time.period, 1, 4))) %>%
  bind_rows(PCN_CCG_Codes)


# Drugs per region  -------------------------------------------------------

## Want to look at total items/cost by region
CCG_ICB_Codes <- CCG_ICB_Codes %>%
  mutate_at(vars(Region, ICB_Name, CCG_Name), ~toupper(.))

Ep_Drugs_ICB <- Ep_Drugs_ICB[, -c(2, 6, 7)]

colnames(Ep_Drugs_ICB) <- c('Date', 'ICB_Name', 'Total_Presc', 'Total_Cost')

## Replacing integrated care board with ICB 
Ep_Drugs_ICB$ICB_Name <- gsub('INTEGRATED CARE BOARD', 'ICB', Ep_Drugs_ICB$ICB_Name)

Ep_Drugs_ICB <- Ep_Drugs_ICB %>%
  inner_join(CCG_ICB_Codes %>%
               select(ICB_Name, Region))

Ep_Prev_ICB <- Ep_Prev_ICB %>%
  dplyr::select(Area.Name, Area.Type, Time.period, Value, Count, Denominator) %>%
  filter(Area.Type == 'ICBs') %>%
  dplyr::rename(ICB_Name = Area.Name) %>%
  dplyr::mutate(Year = as.numeric(substr(Time.period, 1, 4)),
                ICB_Name = toupper(ICB_Name))

Ep_Prev_ICB$ICB_Name <- gsub('INTEGRATED CARE BOARD', 'ICB', Ep_Prev_ICB$ICB_Name)

## ICB Drugs per Year 
Ep_Drugs_ICB <- Ep_Drugs_ICB %>%
  mutate(Year = as.numeric(substr(Date, 1, 4)))

Ep_Drugs_ICB_Year <- aggregate(Ep_Drugs_ICB[, 3:4], by = list(Ep_Drugs_ICB$Year,
                                                              Ep_Drugs_ICB$ICB_Name), FUN = sum)

colnames(Ep_Drugs_ICB_Year)[1:2] <- c('Year', 'ICB_Name')

Ep_Prev_ICB <- Ep_Prev_ICB %>%
  dplyr::rename(Prev = Value, Tot_Epi = Count,
                Tot_Pop = Denominator)

Ep_Drugs_ICB_Year <- Ep_Drugs_ICB_Year %>%
  left_join(CCG_ICB_Codes %>%
              select(ICB_Name, Region))

Ep_Prev_ICB$ICB_Name <- substr(Ep_Prev_ICB$ICB_Name, 1, nchar(Ep_Prev_ICB$ICB_Name) - 1)

Ep_Drugs_ICB <- Ep_Drugs_ICB %>%
  left_join(CCG_ICB_Codes %>%
              select(ICB_Name, Region))

Ep_Prev_ICB <- Ep_Prev_ICB %>%
  left_join(CCG_ICB_Codes %>%
              dplyr::select(Region, ICB_Name))

## Prevalence at the ICB level
Ep_Prev_ICB <- Ep_Prev_ICB %>%
  distinct(Year, ICB_Name, .keep_all = TRUE)

## Prevalence at the regional level 
Ep_Prev_Region <- aggregate(Ep_Prev_ICB[, 5:6], by = list(Ep_Prev_ICB$Year,
                                                          Ep_Prev_ICB$Region), FUN = sum)

colnames(Ep_Prev_Region)[1:2] <- c('Year', 'Region')

Ep_Prev_Region <- Ep_Prev_Region %>%
  mutate(Prev = Tot_Epi / Tot_Pop)

## Drugs at the regional level 
Ep_Drugs_Region_Year <- aggregate(Ep_Drugs_ICB_Year[, 3:4], by = list(Ep_Drugs_ICB_Year$Year,
                                                                      Ep_Drugs_ICB_Year$Region),
                                  FUN = sum)

colnames(Ep_Drugs_Region_Year)[1:2] <- c('Year', 'Region') 
