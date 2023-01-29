
#############################################################
# Draft script for SARIMA forecast on anti-epileptic volume # 
#############################################################

install.packages("forecast")

library(forecast)

# Reading in data ---------------------------------------------------------

## Reading in the data (from Data.Cleaning.R)

Ep_Drugs_Total = subset(Ep_Drugs_Total, select = -c(Group.1,Year) ) # drops the columns not required

Ep_Drugs_Total_time <- Ep_Drugs_Total[, "x"] # selects the column (volume) to be forecast

Sarima <- auto.arima(Ep_Drugs_Total_time)

summary(Sarima)

predictions <- forecast(Sarima, h=12) # Predicts the next 12 time points

plot(predictions) # basic graph with CIs
