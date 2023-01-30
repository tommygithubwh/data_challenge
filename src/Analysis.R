
#############################################################
# Draft script for ARIMA forecast on anti-epileptic volume # 
#############################################################

install.packages("forecast")
install.packages("sarima")

library(ggplot2)
library(forecast)
library(sarima)

# Reading in data ---------------------------------------------------------

## Reading in the data (from Data.Cleaning.R)

Ep_Drugs_Total = subset(Ep_Drugs_Total, select = -c(Group.1,Year) ) # drops the columns not required

Ep_Drugs_Total_time <- Ep_Drugs_Total[, "x"] # selects the column (volume) to be forecast

Arima <- auto.arima(Ep_Drugs_Total_time) # Returns best ARIMA model 

summary(Arima)

predictions <- forecast(Arima, h=12) # Predicts the next 12 time points

plot(predictions) # basic graph with CIs


#############################################################
# Draft script for Chow Test on anti-epileptic volume # 
#############################################################

install.packages("strucchange")

library(strucchange)

ggplot(Ep_Drugs_Total, aes(x = Date, y = x)) +
  geom_line()

# Chow test for covid period

sctest(Ep_Drugs_Total$x ~ Ep_Drugs_Total$Date, type = "Chow", point = 30)
# The Chow test attempts to determine if there is a structural break in the data at some point.
# In this case, it is the volume of drugs before and after covid in the UK (point 30 or April 2020)
# F = 2.9248, p-value = 0.06192.  We have insufficient evidence to reject the null of a structural break before and after covid


# Chow test for NICE epilepsy guidelines April 2022
sctest(Ep_Drugs_Total$x ~ Ep_Drugs_Total$Date, type = "Chow", point = 54)

# F = 0.23881, p-value = 0.7884.   We have insufficient evidence to reject the null of a structural break before and after NICE
# Guidelines but sample for the period after the guidelines is small







