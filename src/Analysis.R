# Installing Packages -----------------------------------------------------
packages <- c("forecast", "sarima", "strucchange", "bayesforecast", "tseries")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)

source(file = "src/DataCleaning.R")

Ep_Drugs_Total_Vol <- Ep_Drugs_Total$Total_Items_Presc

adf.test(Ep_Drugs_Total_Vol) # Dickey Fuller test for stationarity
# p-value = 0.03493, reject null of non-stationarity

Arima_Vol <- auto.arima(Ep_Drugs_Total_Vol) # Returns best ARIMA model
Predictions_Vol <- forecast(Arima_Vol, h = 12) # Predicts the next 12 time points
summary(Arima_Vol)

Ep_Drugs_Total_Vol <- as.numeric(unlist(Ep_Drugs_Total_Vol))
Sarima_Vol <- auto.sarima(Ep_Drugs_Total_Vol, seasonal = TRUE, iter = 2000, chains = 4)
Predictions_Vol_Sarima <- forecast(Sarima_Vol, h = 12) # Predicts next 12 time points

## A SARIMA model includes seasonal effects in forecasts and a SARIMA forecast can be explored in any further expansion of this analysis

# Chow test for volume for covid period
acf(Ep_Drugs_Total$Total_Items_Presc, pl = FALSE) # auto correlation
acf(Ep_Drugs_Total$Total_Items_Presc) # auto correlation plot, some autocorrelation so Chow test results should be treated with some caution
sctest(Ep_Drugs_Total$Total_Items_Presc ~ Ep_Drugs_Total$Date, type = "Chow", point = 30)

# The Chow test attempts to determine if there is a structural break in the data at some point.
# In this case, it is the volume of drugs before and after covid in the UK (point 30 or April 2020)
# F = 2.9248, p-value = 0.06192.  We have insufficient evidence to reject the null of no structural break before and after covid

# Chow test for NICE epilepsy guidelines April 2022
sctest(Ep_Drugs_Total$Total_Items_Presc ~ Ep_Drugs_Total$Date, type = "Chow", point = 54)

# F = 0.23881, p-value = 0.7884.   We have insufficient evidence to reject the null of no structural break before and after NICE
# Guidelines but sample for the period after the guidelines is small

# The Chow Tests should help us answer the second research question:
# "Have patterns in prescribing changed after: (1) the start of the COVID-19 pandemic, or (2) the NICE 2022 guideline for epilepsy?"

##########################################################################
# Descriptive statistics: 2022 prescriptions volume by CCG by population #
##########################################################################

# To join Ep_Prev_CCG and Ep_Drugs_CCG to get CCGs by volume and cost by pop and prevalence

## Have made some assumptions on matching CCGs, used closest on occasions.  Have kept the old column (CCG_Name_Prev) so people can compare

Ep_Prev_CCG <- full_join(Ep_Prev_CCG, CCG_Lookup, by = "CCG_Name") # Adds the new CCG names to the Ep_Prev_CCG table
Ep_Prev_CCG <- subset(Ep_Prev_CCG, select = -c(CCG_Name)) # Drops the old CCG names
Ep_Prev_CCG <- Ep_Prev_CCG %>% rename(CCG_Name = CCG_Name_New) # Renames the column
Ep_Prev_CCG <- subset(Ep_Prev_CCG, Time.period == 2020) # Selects only prev from 2020 (latest)
Ep_Drugs_CCG <- subset(Ep_Drugs_CCG, Year == 2022) # Selects only those with 2022 by volume or cost
Ep_Prev_Drugs_2022 <- inner_join(Ep_Prev_CCG, Ep_Drugs_CCG, by = "CCG_Name")
Ep_Prev_Drugs_2022 <- unique(Ep_Prev_Drugs_2022) # Removes duplicates.
Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>%
  group_by(CCG_Name, Time.period, Year) %>%
  mutate(vol = sum(Total_Items_Presc)) # Creating a new column
# with total items prescribed by CCG for 2022

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>%
  mutate(Total_items_by_pop = vol / CCG_Population) # Creates a new column with total 2022 prescriptions by CCG population

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022[order(Ep_Prev_Drugs_2022$Total_items_by_pop, decreasing = TRUE),]

########################################################################
# Descriptive statistics: 2022 prescriptions cost by CCG by population #
########################################################################

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>%
  group_by(CCG_Name) %>%
  mutate(cost = sum(Total_Cost)) # Creating a new column with total items cost by CCG for 2022

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>%
  mutate(Total_cost_by_pop = cost / CCG_Population) # Creates a new column with total 2022 cost by CCG population

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022[order(Ep_Prev_Drugs_2022$Total_cost_by_pop, decreasing = TRUE),]

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>%
  mutate(Cost_per_prescription = cost / vol) # Creates a new column with cost per prescription

#########################################################
# Descriptive statistics: 2022 cost and spend data only #
#########################################################

Ep_Prev_Drugs_2022_unique <- subset(Ep_Prev_Drugs_2022, select = -c(Date, date, PCN_Code, CCG_Code, PCN_Name
  , Total_Items_Presc, Total_Cost, LSOA))

Ep_Prev_Drugs_2022_unique <- unique(Ep_Prev_Drugs_2022_unique) # creates new df with one row per CCG for 2022
