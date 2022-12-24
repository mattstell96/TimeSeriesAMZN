##############################
#### INDEPENDENT RESEARCH ####
##############################

#######################
### MATTEO STELLUTI ###
#######################

##############
### PHASE I ##
##############

#### LOAD LIBRARIES ####

library(xts)
library(fpp2)
library(forecast)
library(vars)

#### LOAD DATA AND TRANSFORM ####

df = read.csv(YOUR FILE PATH HERE)

df$Date = NULL

#Transform the df into an xts
qrtrs = seq(as.Date('2000-01-01'),length=88,by='quarters')

xts_df = xts(df,order.by=qrtrs)


#### VISUALIZE ####

#1. Plot AMZN

amz = xts_df[,2]

plot(amz, 
     ylim = c(0,4000),
     xlim = c(as.Date('2000-01-01'),as.Date('2021-10-01')),
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN - Quarterly (Avg)' ,
     xlab = 'Date',
     ylab = 'Adj. Close Price (USD)',
     gpars=list(xaxt="n")
)
lines(amz, col = 'orange', lwd=2)
axis(1, at = seq(2000, 2021, by = 2))

#Notes
#1. Exponential Trend
#2. No visible seasonality
#3. Unstable variance


#1.2 Visualize the T-S-R components (STL Decomposition)

stl = mstl(amz, s.window='periodic',robust=TRUE)

autoplot(stl)

#Notes
#1. Confirmed exponential trend
#2. Confirmed no seasonality (missing plot)
#3. Confirmed unstable variance (in the Remainder)

###############
### PHASE II ##
###############

#### PRELIMINARY ####

#1. Box-Cox Power Transf. To Stabilize Variance

amz2 = BoxCox(amz, lambda='auto')

plot(amz2, 
     xlim = c(as.Date('2000-01-01'),as.Date('2021-10-01')),
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN (Box-Cox Transformed)- Quarterly (Avg)' ,
     xlab = 'Date',
     ylab = 'Adj. Close Price (USD)',
     col='orange',
     lwd =2,
     gpars=list(xaxt="n")
)
lines(amz2, col = 'orange', lwd=2)
axis(1, at = seq(2000, 2021, by = 2))

#Notes
#1. Clearly normalized the variance
#2. The trend became linear

#Check with decomposition
stl2 = mstl(amz2, s.window='periodic',robust=TRUE)
autoplot(stl2)


#2. ACF and PACF

acf(amz2, lag = 24) #The order of the MA
pacf(amz2, lag = 24) #The order of the AR is 1

#Notes
#Decaying ACF and sign. lag in PACF suggests an AR(1) model
#Also the ACF resembles that of a random walk

#3. TEST Unit Root: Augmented Dickey Fuller Test

library(fUnitRoots)

adfTest(amz2, type = 'c') #The transf. series has a linear trend

#YES: the series has a unit root

#remove unit root
d_amz2 = diff(amz2)

#Run test again
adfTest(d_amz2, type = 'c')

#NO: the series does not have a 2nd unit root (0.01 < 0.05)


#### ARIMA ####

#The preliminary phase suggests that the series has the dynamics of
#AR(1)
#With unit root

amz2_arima = auto.arima(amz2, approximation = TRUE, stepwise = FALSE)
amz2_arima

#The presence of an AR(1) component is confirmed
#BUT there are 2 unit roots!
#BUT there is also an MA(1) component!

#5. Diagnostics: check residuals

checkresiduals(amz2_arima)


################
### PHASE III ##
################

#FORECASTING

#1. Predict
pred_arima = forecast(amz2_arima, h = 4)

#2. Check performance metrics
summary(pred_arima) #Find here the error metrics

#3. Plot the prediction
plot(pred_arima,
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN Price Forecast - Quarterly (Avg)' ,
     ylab = 'Adj. Close Price (USD)',
     col='orange',
     lwd=2,
)

#4. Plot the actual series
plot(xts_test[,2],
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN Price Actual - Quarterly (Avg)' ,
     xlab = 'Date',
     ylab = 'Adj. Close Price (USD)',
     col='orange',
     lwd=2,
)

################
### PHASE IV ###
################

#########################################
### Multivariate Time Series Analysis ###
#########################################

### New Time Series: Overview ###

#A. WUI

wui = xts_df[,6]

plot(wui,
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'World Uncertainty Index - Quarterly' )
lines(wui, col = 'black', lwd=2)

stl_wui = mstl(wui, s.window='periodic',robust=TRUE)
autoplot(stl_wui)

wui_arima = auto.arima(wui, approximation = TRUE, stepwise = FALSE)
wui_arima

#B. IXIC (NASDAQ)

ixic = xts_df[,1]

plot(ixic,
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'NASDAQ Composite Index - Quarterly (Avg)' )
lines(ixic, col = 'blue', lwd=2)

stl_ixic = mstl(ixic, s.window='periodic',robust=TRUE)
autoplot(stl_ixic)

ixic_arima = auto.arima(ixic, approximation = TRUE, stepwise = FALSE)
ixic_arima


### Pre-Processing: Stationarity ###
#In order to apply VectorAutogression series must be stationary

#AMZN transformation: ARIMA(1,2,1)

#Step1. Log

amz_log = log(amz)

#Step 2. 1st Differencing

amz_diff = diff(amz_log)

#Step 3. Visualize 

autoplot(amz)
autoplot(amz_log)
autoplot(amz_diff)

#Step 4. AdfTest --> Success: Stationary

adfTest(amz_diff)


#IXIC transformation: ARIMA(4,2,0)

#Step1. Log

ixic_log = log(ixic)

#Step2. 1st Differencing

ixic_diff = diff(ixic_log)

#Step3. Visualize

autoplot(ixic)
autoplot(ixic_log)
autoplot(ixic_diff)

#Step4. AdfTest --> Success: Stationary

adfTest(ixic_diff)


#WUI transformation: ARIMA(0,1,4)

#Step1. First Differencing

wui_diff = diff(wui)

#Step2. Visualize

autoplot(wui)
autoplot(wui_diff)

#Step 3. AdfTest --> Success

adfTest(wui_diff)


### I. Vector AutoRegression ###

#Transformation caused the 1st value to be null, need to remove for VAR

amz_diff = amz_diff[-1,]
ixic_diff = ixic_diff[-1,]
wui_diff = wui_diff[-1,]

##Model A. VAR

#Step1. Build the TS with the stationary series

ts_diff = cbind(amz_diff,ixic_diff,wui_diff)

#Step2. VAR

var1 = VAR(ts_diff, type = "const", lag.max = 8, season = NULL, exogen = NULL, ic="AIC")

summary(var1)


##Model B. EVAR

#Step 1. Create a ts with the endogenous vars (AMZ and IXIC)
amz_ixic = cbind(amz_diff,ixic_diff)

#Step 2. Run the VAR

var2 = VAR(amz_ixic, type = "const", lag.max = 8, season = NULL, exogen = wui_diff, ic="AIC")

summary(var2)

### II. Granger Causality ###
#Q: is the uncertainty that dictates the movement in the stock mkt?
#Q is it the stock market that can predict the uncertainty?

#Step1. Need to create the var models

amz_wui = cbind(amz_diff,wui_diff)
var_amz_wui = VAR(amz_wui, type = "const",lag.max = 8, season = NULL, exogen = NULL, ic="AIC")

ixic_wui = cbind(ixic_diff,wui_diff)
var_ixic_wui = VAR(ixic_wui, type = "const",lag.max = 8, season = NULL, exogen = NULL, ic="AIC")

amz_ixic = cbind(amz_diff,ixic_diff)
var_amz_ixic = VAR(amz_ixic, type = "const",lag.max = 8, season = NULL, exogen = NULL, ic="AIC")

#Step 2. Granger Causality Test

#A. AMZ and WUI
grangerA_amzCwui = causality(var_amz_wui, cause = 'AMZN')
grangerA_wuiCamz = causality(var_amz_wui, cause = 'WUI')

grangerA_amzCwui
grangerA_wuiCamz

#B. IXIC and WUI

grangerA_ixicCwui = causality(var_ixic_wui, cause = 'IXIC')
grangerA_wuiCixic = causality(var_ixic_wui, cause = 'WUI')

grangerA_ixicCwui
grangerA_wuiCixic

#C. AMZ and IXIC

grangerA_amzCixic = causality(var_amz_ixic, cause = 'AMZN')
grangerA_ixicCamz = causality(var_amz_ixic, cause = 'IXIC')

grangerA_amzCixic
grangerA_ixicCamz




### III. Impulse Response Function to interpret VAR ###
#Interpreting
#Response is significant when the 0 line is outside the CI
#The decaying is normal because the TS in the VAR models are stationary

#Model A. VAR1
#AMZN Response
irf1_amz = irf(var1,impulse='WUI', response = 'AMZN', n.ahead = 10, boot = TRUE,run=500,ci=.95)

plot(irf1,
     ylab = 'AMZN Returns',
     main = 'Amazon.com Stock Returns Response to WUI Shock'
)

#IXIC Response
irf1_ixic = irf(var1,impulse='WUI', response = 'IXIC', n.ahead = 10, boot = TRUE,run=500,ci=.95)  

plot(irf1_ixic,
     ylab = 'IXIC Returns',
     main = 'NASDAQ Returns Response to WUI Shock'
)

#Model B. VAR2

#AMZN Response
#irf2_amzn = irf(var2,impulse='IXIC', response = 'AMZN', n.ahead = 10, boot = TRUE,run=500,ci=.95)

#plot(irf2_amzn,
#     ylab = 'AMZN Returns',
#     main = 'Amazon.com Stock Returns Response to IXIC Shock (with WUI Exogenous)'
#)


### IV. Variance decomposition ###
#How much of the response is caused by the shock? And how much by the ts itself?

vd1 = fevd(var1, n.ahead = 8) #Important to focus on the 1st model
plot(vd1)

vd2 = fevd(var2, n.ahead = 8) #Important to focus on the 1st model
plot(vd12)


### V. Variance decomposition ###

var_for1 = predict(var1, n.ahead=4, ci = 0.95)
fanchart(var_for1)

var_for2 = predict(var2, n.ahead=4, ci = 0.95)
fanchart(var_for2)







