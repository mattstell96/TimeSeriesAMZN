<h1> Art Auction Prices: Dataset Generation With Scraping</h1>

<p align="center">

<h2>Description</h2>

The aim of this research is to provide an answer to some questions that investors in Amazon.com (AMZN) – and relatable companies – might find relevant in such turbulent times. To begin with, is the outlook positive for AMZN in the financial markets? Then, what is the relationship between Amazon.com and its tech peers in the NASDAQ? Also, is there a relationship between the performance of AMZN and geopolitical risk? Lastly, following up to this question, can investors anticipate the performance of Amazon.com stocks looking at the level of geopolitical instability, or is the vice versa true, whereby the performance of Amazon.com stock can anticipate geopolitical instability? <br/>

💡 This project was carried out for my CPT Independent Research with Professor M. Majbouri at Babson College<br/>

<br />

<h2>R Libraries and Utilities</h2>

 - <b>xts</b>
 - <b>fpp2</b>
 - <b>forecast</b>
 - <b>vars</b>
 
<br />

<h2>Environments Used </h2>

- <b>macOS Monterey</b>

<br />

<h2>Project walk-through:</h2>

<br />
 
<h3> Preliminary </h3>

**Step 1. Load Libraries** <br/>

```r
library(xts)
library(fpp2)
library(forecast)
library(vars)
```

<br />

**Step 2. Load & Transform Datasets into a Time Series Object (xts)** <br/>

Data was downloaded from [Yahoo!Finance](https://finance.yahoo.com/quote/AMZN/history?p=AMZN) and lightly pre-processed in Excel.

```r
df = read.csv(FILE PATH HERE)

df$Date = NULL

#Transform the df into an xts
qrtrs = seq(as.Date('2000-01-01'),length=88,by='quarters')

xts_df = xts(df,order.by=qrtrs)

```

<br />

<h3> Exploration & Decomposition </h3>

**Step 3. Plot AMZN and Visualize the TSR Components** <br/>

(NOTE: *TSR* stands for Time, Season, Remainder)

First, let's visualize the AMZN trend from 2000 till 2022 (only partially available). <br/>

```r
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
```

OUTPUT: <br/>
<img src="https://i.imgur.com/5uk7Bfm.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

Second, let's visualize TSR decomposition. <br/>

```r
stl = mstl(amz, s.window='periodic',robust=TRUE)
autoplot(stl)
```

OUTPUT: <br/>
<img src="https://i.imgur.com/ER9zKYC.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

The time series decomposition reveals that:
+ There is an exponential trend
+ There is no seasonality (the "S" plot is missing)
+ The Remainder (R) reveals the presence of unstable variance

<br />

<h3> Time Series Modeling </h3>

**Step 5. Box-Cox Power Transformation (To Stabilized Variance)** <br/>

```r
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
```

OUTPUT: <br/>
<img src="https://i.imgur.com/FI5ecNW.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

***Findings***: The series appears to have a more stable variance now, and one could tell that the Box-Cox transformation has also transformed the trend component from exponential to linear.

<br />

**Step 6. ACF and PACF Plots To Investigate the AMZN Time Series** <br/>

```r
acf(amz2, lag = 24) #The order of the MA
pacf(amz2, lag = 24) #The order of the AR is 1
```

OUTPUT: <br/>
<img src="https://i.imgur.com/ExZn6bw.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

***Findings:***
+ Decaying ACF and sign. lag in PACF suggests an AR(1) model
+ Also the ACF resembles that of a random walk

**Step 6.2. Double-Check Previous Findings With the Augmented Dickey-Fuller Test** <br/>

```r
library(fUnitRoots)
adfTest(amz2, type = 'c') #The transf. series has a linear trend
```

***Findings:*** The test's p-value is 0.99; we accept the alternative hypothesis that the AMZN time series (Box-Cox transformed).

**Step 6.3. Unit Root Removal** <br/>

```p
d_amz2 = diff(amz2)
```

We also check again for a potential 2nd unit root with the same test as before:

```p
adfTest(d_amz2, type = 'c')
```

***Findings:*** There is no 2nd unit root; we have successfuly removed the unit root from the series.

<br />

<h3> Application of ARIMA</h3>

**Step 7. ARIMA Modeling** <br/>

```r
amz2_arima = auto.arima(amz2, approximation = TRUE, stepwise = FALSE)
amz2_arima
```

***Findings:*** The presence of a unit root is confirmed, given AR(1); also, according to this algorithm there is even a 2nd unit root and a moving average component.

We can now check the ARIMA's: if the residuals behave like *white noise* then the model is accurate for this AMZN time series.

```r
checkresiduals(amz2_arima)
```

OUTPUT: <br/>
<img src="https://i.imgur.com/kt6LPcs.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

***Findings:*** ARIMA is accurate, as the residuals have the same properties of white noise:
+ Constant mean equal to zero
+ Variance is constant
+ There is no autocorrelation

<br />

<h3> Forecasting </h3>

**Step 8. ARIMA Modeling** <br/>

```r
#1. Predict
pred_arima = forecast(amz2_arima, h = 4)

#2. Plot the prediction
plot(pred_arima,
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN Price Forecast - Quarterly (Avg)' ,
     ylab = 'Adj. Close Price (USD)',
     col='orange',
     lwd=2,
)

#3. Plot the actual series
plot(xts_test[,2],
     grid.col = NA,
     yaxis.right = FALSE,
     main = 'AMZN Price Actual - Quarterly (Avg)' ,
     xlab = 'Date',
     ylab = 'Adj. Close Price (USD)',
     col='orange',
     lwd=2,
)
```

OUTPUT: <br/>
<img src="https://i.imgur.com/2dkh2my.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

***Findings:*** According to the ARIMA-based forecast the quarterly average value of AMZN is predicted to grow till the end of 2022.

<br />

<h3> Multivariate Time Series Analysis: Studying AMZN Combined with the WUI and NASDAQ </h3>

NOTE: "WUI" stands for "World Uncertainty Indicator", which is an indicator measuring global geopolitical risk.

**Step 9. Exploration of WUI and NASDAQ** <br/>

WUI:<br/>

```r
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
```

OUTPUT:<br/>
<img src="https://i.imgur.com/dAb65Yu.png" height="80%" width="80%" alt="AMZN trend"/> <br/>

NASDAQ:<br/>

```r
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
```

OUTPUT:<br/>
<img src="https://i.imgur.com/fILgEdQ.png" height="80%" width="80%" alt="AMZN trend"/> 

<br />

**Step 10.1. Transforming AMZN, WUI, and NASDAQ (Need to Be Stationary)** <br/>

```r
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
```

**Step 10.2. Multivariate Time Series Analysis: Vector Autoregression** <br/>

```r
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

##PREDICT
var_for1 = predict(var1, n.ahead=4, ci = 0.95)
fanchart(var_for1)

var_for2 = predict(var2, n.ahead=4, ci = 0.95)
fanchart(var_for2)
```

OUTPUT:<br/>
<img src="https://i.imgur.com/QhNSFVU.png" height="80%" width="80%" alt="AMZN trend"/> 

<br />

**Step 11. Multivariate Time Series Analysis: Granger Causality** <br/>

The causality function in R simultaneously implements two versions of the Test. The traditional Granger Causality Test attempts to understand whether knowing the past behavior of a time series improves the understanding of the future behavior of another time series. The Instantaneous Granger Causality Test, instead, strives to understand whether knowing the future behavior of a time series is useful to realize the future behavior of another time series. <br/>


```r
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
```

OUTPUT:<br/>
<img src="https://i.imgur.com/aIpweOE.png" height="80%" width="80%" alt="AMZN trend"/> 

<br/ >

**Step 12. Multivariate Time Series Analysis: The Impulse-Response Function** <br/>

Called impulse-response function (IRF) gauges the reaction of a time series to the shock of another time series. The function is even more powerful when paired with the variance decomposition function (FEVD), which explains what portion of the reaction is caused by the time series itself or by the shock.<br/>

```r
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

##Variance decomposition ##
#How much of the response is caused by the shock? And how much by the ts itself?

vd1 = fevd(var1, n.ahead = 8) #Important to focus on the 1st model
plot(vd1)

vd2 = fevd(var2, n.ahead = 8) #Important to focus on the 1st model
plot(vd12)
```

OUTPUT 1:<br/>
<img src="https://i.imgur.com/8CbS5PZ.png" height="80%" width="80%" alt="AMZN trend"/> 

***Findings:*** It is evident that Amazon’s stocks are more sensitive to geopolitical shocks compared to the NASDAQ. This is perfectly logical from a financial standpoint as the numerous different companies englobed in the NASDAQ composite index make the average quarterly returns on the IXIC relatively more insensitive to any kind of exogenous shock, while generating persistent positive returns only in the long run.<br/>

OUTPUT 2:<br/>
<img src="https://i.imgur.com/hBQFU8Q.png" height="80%" width="80%" alt="AMZN trend"/> 

***Findings:*** It is remarkable that most of the response in the returns on AMZN are caused by the behavior of AMZN returns themselves, although there is still a minor share of the response that is attributed to the shock in the WUI. What is more, while most of the response of the returns on the IXIC are caused by the IXIC itself, about 40% of the response of the quarterly average returns on the IXIC to a shock in the WUI are also caused by the behavior of the AMZN stock.<br/>

<br />

</p>



<!--
 ```diff
- text in red
+ text in green
! text in orange
# text in gray
@@ text in purple (and bold)@@
```
--!>
