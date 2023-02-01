# Installing Packages -----------------------------------------------------
packages <- c("forecast", "sarima", "strucchange", "bayesforecast")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)
rm(packages)


#############################################################
# Draft script for ARIMA forecast on anti-epileptic volume # 
#############################################################


##Reading in the data (from Data.Cleaning.R)

### ARIMA for drugs volume

Ep_Drugs_Total_Vol <- Ep_Drugs_Total[, "Total_Presc"] # selects the column (volume) to be forecast

Arima_Vol <- auto.arima(Ep_Drugs_Total_Vol) # Returns best ARIMA model 

summary(Arima_Vol)

Predictions_Vol <- forecast(Arima_vol, h=24) # Predicts the next 24 time points

plot(Predictions_Vol) # basic graph with CIs


#############################################################
# Draft script for SARIMA forecast on anti-epileptic volume # 
#############################################################

Sarima_Vol <-auto.sarima(Ep_Drugs_Total_Vol,seasonal = TRUE, iter = 2000,chains = 4)

Predictions_Vol_Sarima <- forecast(Sarima_Vol,h=24) # Predicts next 24 time points

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



#######################################################
# Descriptive statistics                              #
#######################################################

# To join Ep_Prev_CCG and Ep_Drugs_CCG to get CCGs by volume and cost by pop and prevalence











