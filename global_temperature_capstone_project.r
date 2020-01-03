## USE FORECAST LIBRARY.

install.packages("forecast")

# Load Forecase Library
library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/sneha/Documents/Coursework/Capstone/Processed_GlobalData_R_Script")

# Create data frame.
temperature.data <- read.csv("GlobalTemperatures_LandAverageTemp_Cleaned.csv")

# See the first 6 records of the file.
head(temperature.data)

# See the last 6 records of the file.
tail(temperature.data)

# Use ts() function to create time series dataset (Data from 1753 to 2013)
temperature.ts <- ts(temperature.data$LandAverageTemperature, 
                   start = c(1753, 1), end = c(2015, 12), freq = 12)

# Data Summary showing mon, max, median, mean, etc.
summary(temperature.ts)

# Plotting Entire Dataset
plot(temperature.ts, 
     xlab = "Years", ylab = "Temperature", 
     ylim = c(-5, 20), main = "Global Land Temperature", col = "orange")

# Use stl() function to plot time series components of the original data
# The plot includes original data, trend, seasonal, and remainder (level and noise component)
tempavg.stl <- stl(temperature.ts, s.window = "periodic")
autoplot(tempavg.stl, main = "Global Land Temperature Time Series Component")

# Blown - out graph since seasonality is not clear in the graph before
temperature_zoom.ts <- ts(temperature.data$LandAverageTemperature, 
                     start = c(1980, 1), end = c(2015, 12), freq = 12)
tempavg_zoom.stl <- stl(temperature_zoom.ts, s.window = "periodic")
autoplot(tempavg_zoom.stl, main = "Global Land Temperature Time Series Component (Zoom)")


# Use acf() function to identify autocorrelation ad plot autocorrelation for differnt lags
seasonal_months=12
autocor <- acf(temperature.ts, lag.max=seasonal_months, xaxt="n", xlab="Lag (months)",main = "Autocorrelation for Global Temperature Dataset")
axis(1, at=0:seasonal_months/12, labels=0:seasonal_months)

# Display autocorrelatiion coefficients for varous lags
Lag <- round(autocor$lag * 12) 
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

### Data displays yearly seasonality since ACF is 0.967 for lag = 12 ###

### Testing Predictability: Proving data is not random walk type

temperature_predictability.ar1<- Arima(temperature.ts, order = c(1,0,0))
summary(temperature_predictability.ar1)

#Series: temperature.ts 
#ARIMA(1,0,0) with non-zero mean 
#Coefficients:
#    ar1    mean
#0.8496  8.3792
#s.e.  0.0094  0.2729
#sigma^2 estimated as 5.343:  log likelihood=-7122.21
#AIC=14250.43   AICc=14250.44   BIC=14268.6
#Training set error measures:
#    ME     RMSE      MAE       MPE     MAPE     MASE
#Training set 0.0002029032 2.310769 2.007738 -15.70471 45.90645 3.055633
#ACF1
#Training set 0.6943524

# Splitting data into training and validation datasets
# 1753 to 1975 - Training Dataset
# 1976 to 2015 - Validation Dataset
nValid <- 480
nTrain <- length(temperature.ts) - nValid
train_temperature.ts <- window(temperature.ts, start = c(1753, 1), end = c(1753, nTrain))
valid_temperature.ts <- window(temperature.ts, start = c(1753, nTrain + 1), 
                             end = c(1753, nTrain + nValid))

# Create Holt-Winter's model (HW).
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.

hw_temperature.ZZZ <- ets(train_temperature.ts, model = "ZZZ") 
hw_temperature.ZZZ

# Model Chosen:
#ETS(A,N,A) 
#Call:
#    ets(y = train_temperature.ts, model = "ZZZ") 
#Smoothing parameters:
#    alpha = 0.0781 
#    gamma = 0.0299 
#Initial states:
#    l = 8.6291 
#    s = -5.5719 -2.6609 0.003 3.4122 5.6322 6.2056
#         5.3362 3.0262 0.0971 -3.6414 -5.3559 -6.4825
#sigma:  0.8373
#     AIC     AICc      BIC 
#20184.97 20185.15 20273.35 

diff.temperature.ts <- diff(temperature.ts, lag = 1)
diff.temperature.ts

Acf(diff.temperature.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Temperature Data")

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw_temperature.ZZZ.pred <- forecast(hw_temperature.ZZZ, h = nValid, level = 0)
hw_temperature.ZZZ.pred

# Get accuracy metrics for HW's model
round(accuracy(hw_temperature.ZZZ.pred, valid_temperature.ts), 3)

#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.835 0.539 -1.796 18.105 0.758 0.256        NA
#Test set     0.546 0.712 0.599  7.780  8.617 0.842 0.679     0.398

# Plot HW predictions for original dataset
plot(hw_temperature.ZZZ.pred, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Holt-Winter's Model for Temperature with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(hw_temperature.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid_temperature.ts, col = "black", lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1753.75, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "HW's model for training (A, N, A)",
                               "HW's forecast for validation (A, N, A)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#------------------Regression Model with Linear Trend & Seasonality-------------------
# Use tslm() function to create linear trend and seasonal model.
train.lintrend.season <- tslm(train_temperature.ts ~ trend + season)
summary(train.lintrend.season)

#Call:
#    tslm(formula = train_temperature.ts ~ trend + season)
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-6.5330 -0.3922 -0.0222  0.3810  5.8607 
#Coefficients:
#    Estimate Std. Error t value Pr(>|t|)    
#    (Intercept) 1.780e+00  6.658e-02  26.731   <2e-16 ***
#    trend       2.358e-04  2.227e-05  10.589   <2e-16 ***
#    season2     7.152e-01  8.427e-02   8.487   <2e-16 ***
#    season3     2.672e+00  8.427e-02  31.703   <2e-16 ***
#    season4     6.014e+00  8.427e-02  71.366   <2e-16 ***
#    season5     8.935e+00  8.427e-02 106.023   <2e-16 ***
#    season6     1.114e+01  8.427e-02 132.179   <2e-16 ***
#    season7     1.212e+01  8.427e-02 143.794   <2e-16 ***
#    season8     1.155e+01  8.427e-02 137.105   <2e-16 ***
#    season9     9.488e+00  8.427e-02 112.581   <2e-16 ***
#    season10    6.631e+00  8.427e-02  78.681   <2e-16 ***
#    season11    3.481e+00  8.427e-02  41.302   <2e-16 ***
#   season12    8.971e-01  8.427e-02  10.645   <2e-16 ***
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.8899 on 2663 degrees of freedom
#Multiple R-squared:  0.9594,	Adjusted R-squared:  0.9592 
#F-statistic:  5244 on 12 and 2663 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lintrend.season.pred <- forecast(train.lintrend.season, h = nValid, level = 0)
train.lintrend.season.pred

# Get accuracy metrics for Linear Trend and Seasonality model
round(accuracy(train.lintrend.season.pred, valid_temperature.ts),3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.888 0.590 -3.796 19.775 0.829 0.385        NA
#Test set     0.595 0.763 0.642  9.273  9.876 0.902 0.649     0.432

# Plot Linear Trend and Seasonality predictions for original dataset
plot(train.lintrend.season.pred, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Model with Linear Trend and Seasonality", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(train.lintrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid_temperature.ts, col = "black", lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1753, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "Lin. Trend + Seasonality (Training)",
                           "Lin. Trend + Seasonality (Validation)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train_temperature.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)

#Call:
#    tslm(formula = train_temperature.ts ~ trend + I(trend^2) + season)
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-6.8269 -0.3666 -0.0102  0.3748  5.7375 
#Coefficients:
#    Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)  2.127e+00  7.574e-02  28.075  < 2e-16 ***
#    trend       -5.414e-04  8.775e-05  -6.169  7.9e-10 ***
#    I(trend^2)   2.903e-07  3.174e-08   9.147  < 2e-16 ***
#    season2      7.152e-01  8.300e-02   8.618  < 2e-16 ***
#    season3      2.672e+00  8.300e-02  32.191  < 2e-16 ***
#    season4      6.014e+00  8.300e-02  72.465  < 2e-16 ***
#    season5      8.935e+00  8.300e-02 107.656  < 2e-16 ***
#    season6      1.114e+01  8.300e-02 134.215  < 2e-16 ***
#    season7      1.212e+01  8.300e-02 146.008  < 2e-16 ***
#    season8      1.155e+01  8.300e-02 139.217  < 2e-16 ***
#    season9      9.488e+00  8.300e-02 114.315  < 2e-16 ***
#    season10     6.631e+00  8.300e-02  79.893  < 2e-16 ***
#    season11     3.481e+00  8.300e-02  41.938  < 2e-16 ***
#    season12     8.971e-01  8.300e-02  10.809  < 2e-16 ***
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.8764 on 2662 degrees of freedom
#Multiple R-squared:  0.9606,	Adjusted R-squared:  0.9604 
#F-statistic:  4997 on 13 and 2662 DF,  p-value: < 2.2e-16


# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred
# Get accuracy metrics for Quadratic Trend and Seasonality model
round(accuracy(train.trend.season.pred, valid_temperature.ts),3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.874 0.578 -3.364 19.807 0.812 0.366        NA
#Test set     0.039 0.416 0.330  1.452  4.912 0.464 0.538     0.236

# Plot Linear Trend and Seasonality predictions for original dataset
plot(train.trend.season.pred, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Regression Model with Quadratic Trend & Seasonality For Temperature", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(train.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid_temperature.ts, col = "black", lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1.7553, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1750,-7, legend = c("Temperature", "Quad. Trend + Seasonality (Training)",
                           "Quad. Trend + Seasonality Forecast (Validation)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-------------------ACF on residuals data-----------------
# Use acf() to identify auto correlation for training and validation datasets 
# and plot autocorrelation for multiple lags

Acf(train_temperature.ts, lag.max = 12, main = "Autocorrelation for Temperature Training Dataset")
Acf(valid_temperature.ts, lag.max = 12, main = "Autocorrelation for Temperature Validation Dataset")


# Use acf() function to identify autocorrelation for model residuals (training and validation)
# and plot autocorrelation for different lags

#Acf(train.trend.season$residuals, lag.max = 12, 
#    main = "Autocorrelation for Temperature Training Residuals")
Acf(train.trend.season.pred$residuals, lag.max = 12, 
     main = "Autocorrelation for Temperature Predicted Residuals (Training)")
Acf(valid_temperature.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Temperature Predicted Residuals (Validation)")


####### Two Level Model Using Linear Trend With Seasonality and AR(1) residuals model ######
linres.ar1 <- Arima(train.lintrend.season$residuals, order = c(1,0,0))
summary(linres.ar1)
linres.ar1.pred <- forecast(linres.ar1, h = nValid, level = 0)
linres.ar1.pred
valid.lintwo.level.pred <- train.lintrend.season.pred$mean + linres.ar1.pred$mean

# Get accuracy metrics for Linear Trend With Seasonality and AR(1) residuals model
round(accuracy(valid.lintwo.level.pred, valid_temperature.ts), 3)
#            ME  RMSE   MAE   MPE  MAPE ACF1 Theil's U
#Test set 0.594 0.763 0.642 9.259 9.871 0.65     0.432

valid_lin.df <- data.frame(valid_temperature.ts,train.lintrend.season.pred$mean,linres.ar1.pred$mean,valid.lintwo.level.pred)
names(valid_lin.df) <- c("Temperature","Reg.Forecast","AR(1)Forecast","Combined.Forecast")
valid_lin.df

# Plot Linear Trend and Seasonality predictions for original dataset
plot(train.lintrend.season.pred$fitted + linres.ar1.pred$fitted, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Combined Two-level Model (Lin) for Temperature", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(valid.lintwo.level.pred, col = "blue", lwd = 2, lty =4)
lines(valid_temperature.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1753.75, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "Two Level Model (Lin.)(Training)",
                           "Two Level Model (Lin.)(Validation)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


####### Two Level Model Using Quadratic Trend With Seasonality and AR(1) residuals model ######
# User Arima() Function to create AR(1) model for training residuals
# The Arima modl of order = c(1,0,0) gives AR(1) model

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)

# Use forecast() to make prediction of residuals in validation dataset
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean
# Get accuracy metrics for Quadratic Trend With Seasonality and AR(1) residuals model
round(accuracy(valid.two.level.pred, valid_temperature.ts), 3)
#            ME  RMSE  MAE   MPE  MAPE  ACF1 Theil's U
#Test set 0.039 0.416 0.33 1.451 4.912 0.538     0.236

valid_quad.df <- data.frame(valid_temperature.ts,train.trend.season.pred$mean,res.ar1.pred$mean,valid.two.level.pred)
names(valid_quad.df) <- c("Temperature","Reg.Forecast","AR(1)Forecast","Combined.Forecast")
valid_quad.df

# Plot Two Level Model Using Quadratic Trend With Seasonality and AR(1) residuals model for original data
plot(train.trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Combined Two-level Model (Quad) for Temperature", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(valid.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(valid_temperature.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1753.75, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "Two Level Model (Quad.)(Training)",
                           "Two Level Model (Quad.)(Validation)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-------------------AUTO ARIMA-------------------
# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
train_temperature.auto.arima <- auto.arima(train_temperature.ts)

#Series: train_temperature.ts 
#ARIMA(1,0,2)(1,1,0)[12] 
#Coefficients:
#    ar1     ma1     ma2     sar1
#0.2427  0.0627  0.0644  -0.4759
#s.e.  0.1084  0.1083  0.0361   0.0172
#sigma^2 estimated as 0.8909:  log likelihood=-3625.81
#AIC=7261.63   AICc=7261.65   BIC=7291.07
#Training set error measures:
#                      ME     RMSE      MAE       MPE     MAPE      MASE
#Training set 0.001242714 0.941065 0.599267 -3.318967 18.91144 0.8420006
#                     ACF1
#Training set 0.0008882909

summary(train_temperature.auto.arima)

#Series: train_temperature.ts 
#ARIMA(1,0,2)(1,1,0)[12] 
#Coefficients:
#    ar1     ma1     ma2     sar1
#0.2427  0.0627  0.0644  -0.4759
#s.e.  0.1084  0.1083  0.0361   0.0172
#sigma^2 estimated as 0.8909:  log likelihood=-3625.81
#AIC=7261.63   AICc=7261.65   BIC=7291.07
#Training set error measures:
#                      ME     RMSE      MAE       MPE     MAPE      MASE
#Training set 0.001242714 0.941065 0.599267 -3.318967 18.91144 0.8420006
#                     ACF1
#Training set 0.0008882909

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train_temperature.auto.arima.pred <- forecast(train_temperature.auto.arima, h = nValid, level = 0)
summary(train_temperature.auto.arima.pred)

# Get accuracy metrics for Auto ARIMA Model
round(accuracy(train_temperature.auto.arima.pred, valid_temperature.ts), 3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.001 0.941 0.599 -3.319 18.911 0.842 0.001        NA
#Test set     0.545 0.715 0.595  7.606  8.448 0.836 0.632     0.374

# Plot Auto ARIMA for original data
plot(train_temperature.auto.arima.pred, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2015), 
     main = "Auto ARIMA Model For Temperature [(0,0,2)(1,1,0)[12] with drift]", flty = 2) 
axis(1, at = seq(1753, 2015, 1), labels = format(seq(1753, 2015, 1)))
lines(train_temperature.auto.arima$fitted, col = "blue", lwd = 2)
lines(valid_temperature.ts, col = "black", lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1976, 1976), c(-10, 25))
lines(c(2013, 2013), c(-10, 25))
text(1900, 28, "Training")
text(1995, 28, "Validation")

arrows(1753.75, 25, 1975.25, 25, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1976.25, 25, 2012.75, 25, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "Auto ARIMA model (Training)",
                           "Auto ARIMA Model Forecast (Validation)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

####### Comparison for Accuracy Metrics ##############
#HW:
round(accuracy(hw_temperature.ZZZ.pred, valid_temperature.ts), 3)
# Quadratic Trend + Seasonality
round(accuracy(train.trend.season.pred, valid_temperature.ts),3)
# Linear Trend and Seasonality
round(accuracy(train.lintrend.season.pred, valid_temperature.ts),3)
# Two Level - Linear
round(accuracy(valid.lintwo.level.pred, valid_temperature.ts), 3)
# Two Level - Quad
round(accuracy(valid.two.level.pred, valid_temperature.ts), 3)
# Auto ARIMA
round(accuracy(train_temperature.auto.arima.pred, valid_temperature.ts), 3)

################################################################

> #HW:
#    > round(accuracy(hw_temperature.ZZZ.pred, valid_temperature.ts), 3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.835 0.539 -1.796 18.105 0.758 0.256        NA
#Test set     0.546 0.712 0.599  7.780  8.617 0.842 0.679     0.398
#> # Quadratic Trend + Seasonality
#> round(accuracy(train.trend.season.pred, valid_temperature.ts),3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.874 0.578 -3.364 19.807 0.812 0.366        NA
#Test set     0.039 0.416 0.330  1.452  4.912 0.464 0.538     0.236
#> # Linear Trend and Seasonality
 #   > round(accuracy(train.lintrend.season.pred, valid_temperature.ts),3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.000 0.888 0.590 -3.796 19.775 0.829 0.385        NA
#Test set     0.595 0.763 0.642  9.273  9.876 0.902 0.649     0.432
#> # Two Level - Linear
#> round(accuracy(valid.lintwo.level.pred, valid_temperature.ts), 3)
#            ME  RMSE   MAE   MPE  MAPE ACF1 Theil's U
#Test set 0.594 0.763 0.642 9.259 9.871 0.65     0.432
#> # Two Level - Quad
#    > round(accuracy(valid.two.level.pred, valid_temperature.ts), 3)
#            ME  RMSE  MAE   MPE  MAPE  ACF1 Theil's U
#Test set 0.039 0.416 0.33 1.451 4.912 0.538     0.236
#> # Auto ARIMA
#> round(accuracy(train_temperature.auto.arima.pred, valid_temperature.ts), 3)
#                ME  RMSE   MAE    MPE   MAPE  MASE  ACF1 Theil's U
#Training set 0.001 0.941 0.599 -3.319 18.911 0.842 0.001        NA
#Test set     0.545 0.715 0.595  7.606  8.448 0.836 0.632     0.374

################################################################

# Quadratic Trend With Seasonality is the most appropriate model
# MAPE = 4.912%
# RMSE = 0.416

#####################################################################
##################---------ENTIRE DATA SET--------###################
#####################################################################

# Quadratic Trend with Seasonality

#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
quadtrend.season <- tslm(temperature.ts ~ trend + I(trend^2) + season)
summary(quadtrend.season)

#Call:
#    tslm(formula = temperature.ts ~ trend + I(trend^2) + season)
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-6.8750 -0.3404  0.0014  0.3462  5.7347 
#Coefficients:
#    Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)  2.194e+00  6.541e-02  33.545   <2e-16 ***
#    trend       -6.124e-04  6.425e-05  -9.532   <2e-16 ***
#    I(trend^2)   3.172e-07  1.971e-08  16.095   <2e-16 ***
#    season2      6.895e-01  7.167e-02   9.620   <2e-16 ***
#    season3      2.668e+00  7.167e-02  37.230   <2e-16 ***
#    season4      5.970e+00  7.167e-02  83.298   <2e-16 ***
#    season5      8.871e+00  7.167e-02 123.777   <2e-16 ***
#    season6      1.106e+01  7.167e-02 154.352   <2e-16 ***
#    season7      1.202e+01  7.167e-02 167.723   <2e-16 ***
#    season8      1.147e+01  7.167e-02 160.071   <2e-16 ***
#    season9      9.446e+00  7.167e-02 131.804   <2e-16 ***
#    season10     6.616e+00  7.167e-02  92.314   <2e-16 ***
#    season11     3.454e+00  7.167e-02  48.188   <2e-16 ***
#    season12     9.103e-01  7.167e-02  12.702   <2e-16 ***
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.8219 on 3142 degrees of freedom
#Multiple R-squared:  0.9649,	Adjusted R-squared:  0.9648 
#F-statistic:  6652 on 13 and 3142 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
quadtrend.season.pred <- forecast(quadtrend.season, h = 1200, level = 0)
quadtrend.season.pred

# Plot Linear Trend and Seasonality predictions for original dataset
plot(quadtrend.season.pred, 
     xlab = "Time", ylab = "Temperature", ylim = c(-30, 30), bty = "l",
     xaxt = "n", xlim = c(1753, 2120), 
     main = "Model with Quad Trend and Seasonality", flty = 2) 
axis(1, at = seq(1753, 2120, 1), labels = format(seq(1753, 2120, 1)))
lines(quadtrend.season.pred$fitted, col = "blue", lwd = 2)


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.

lines(c(2015, 2015), c(-10, 25))
lines(c(2115, 2115), c(-10, 25))
text(1900, 27.5, "Original Data")
text(2060, 27.5, "Future")

arrows(2015.25, 22, 2114.75, 22, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(1753.25, 22, 2014.75, 22, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(1800,-7, legend = c("Temperature", "Quad. Trend + Seasonality (Orig.)",
                           "Quad. Trend + Seasonality (Future)"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#### END OF SCRIPT ####