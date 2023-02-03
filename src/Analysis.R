# Installing Packages -----------------------------------------------------
packages <- c("forecast", "sarima", "strucchange", "bayesforecast")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)


#############################################################
# Draft script for ARIMA forecast on anti-epileptic volume # 
#############################################################


Ep_Drugs_Total_Temp <- read_csv('data/Ep_Drugs_Vol_Cost_up_to_Oct_2022_temp.csv')
# Reading in data from temp csv which has total drugs prescribing up to 2022 (current Ep_drugs_Total is only up to 2021)

## ARIMA for drugs volume

Ep_Drugs_Total_Vol <- Ep_Drugs_Total_Temp[, "Total_Presc"] # selects the column (volume) to be forecast

Arima_Vol <- auto.arima(Ep_Drugs_Total_Vol) # Returns best ARIMA model 

summary(Arima_Vol)

Predictions_Vol <- forecast(Arima_Vol, h=12) # Predicts the next 12 time points

plot(Predictions_Vol, main ="ARIMA forecast of anti-epileptic prescriptions", xlab="Time (months)", 
     ylab="Total anti-epileptic prescriptions") # basic graph with CIs

##Reading in the data (from Data.Cleaning.R)

#############################################################
# Draft script for SARIMA forecast on anti-epileptic volume # 
#############################################################

Ep_Drugs_Total_Vol <- as.numeric(unlist(Ep_Drugs_Total_Vol))

Sarima_Vol <-auto.sarima(Ep_Drugs_Total_Vol,seasonal = TRUE, iter = 2000,chains = 4)

Predictions_Vol_Sarima <- forecast(Sarima_Vol,h=12) # Predicts next 24 time points

plot(Sarima_Vol) # plots the Sarima model
       
plot(Predictions_Vol_Sarima) # plots the forecast

# We need to tune the SARIMA model for best fit.  An ARIMA forecast will suffice for now  
## as need to look up how to implement in R so will come back to this if have time


#######################################################
# Draft script for Chow Test on anti-epileptic volume # 
#######################################################


# Chow test for covid period

ggplot(Ep_Drugs_Total, aes(x = Date, y = Total_Presc)) +
  geom_line()


sctest(Ep_Drugs_Total$Total_Presc ~ Ep_Drugs_Total$Date, type = "Chow", point = 30)
# The Chow test attempts to determine if there is a structural break in the data at some point.
# In this case, it is the volume of drugs before and after covid in the UK (point 30 or April 2020)
# F = 2.9248, p-value = 0.06192.  We have insufficient evidence to reject the null of no structural break before and after covid


# Chow test for NICE epilepsy guidelines April 2022

# sctest(Ep_Drugs_Total$Total_Presc ~ Ep_Drugs_Total$Date, type = "Chow", point = 54)

# Doesn't currently work as new Ep_Drugs_Total only goes up to Oct 2021 (previous went up to Dec 2022)

# F = 0.23881, p-value = 0.7884.   We have insufficient evidence to reject the null of no structural break before and after NICE
# Guidelines but sample for the period after the guidelines is small

# The Chow Tests should help us answer the second research question: 
# "Have patterns in prescribing changed after: (1) the start of the COVID-19 pandemic, or (2) the NICE 2022 guideline for epilepsy?"



##########################################################################
# Descriptive statistics: 2022 prescriptions volume by CCG by population #
##########################################################################


# 

# To join Ep_Prev_CCG and Ep_Drugs_CCG to get CCGs by volume and cost by pop and prevalence

## Reading in the data from temp Excel file which has the matching CCGs for Ep_Prev_CCG and Ep_Drugs_CCG  
## When have time can do the match in R
### Have made some assumptions on matching CCGs, used closest on occasions.  Have kept the old column (CCG_Name_Prev) so people can compare
 
Ep_Prev_CCG_match <- read.csv('data/Ep_Prev_CCG_match.csv')

Ep_Prev_Drugs <- inner_join(Ep_Prev_CCG_match, Ep_Drugs_CCG , by = "CCG_Name") 

Ep_Prev_Drugs_2022 <- subset(Ep_Prev_Drugs, Time.period == 2020) # Selects only prev from 2020 (latest)

Ep_Prev_Drugs_2022 <- subset(Ep_Prev_Drugs_2022, Year == 2022) # Selects only those with 2022 by volume or cost

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>% group_by(CCG_Name) %>% mutate(vol= sum(Total_Items_Presc)) # Creating a new column 
# with total items prescribed by CCG for 2022

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022  %>%
  mutate(Total_items_by_pop = vol / CCG_Population) # Creates a new column with total 2022 prescriptions by CCG population


Ep_Prev_Drugs_2022 = Ep_Prev_Drugs_2022[order(Ep_Prev_Drugs_2022$Total_items_by_pop, decreasing = TRUE), ]



# NHS Blackpool has the highest prescription per population for 2022


########################################################################
# Descriptive statistics: 2022 prescriptions cost by CCG by population #
########################################################################

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022 %>% group_by(CCG_Name) %>% mutate(cost= sum(Total_Cost)) # Creating a new column 
# with total items cost by CCG for 2022

Ep_Prev_Drugs_2022 <- Ep_Prev_Drugs_2022  %>%
  mutate(Total_cost_by_pop = cost / CCG_Population) # Creates a new column with total 2022 cost by CCG population

Ep_Prev_Drugs_2022 = Ep_Prev_Drugs_2022[order(Ep_Prev_Drugs_2022$Total_cost_by_pop, decreasing = TRUE), ]

View(Ep_Prev_Drugs_2022)








