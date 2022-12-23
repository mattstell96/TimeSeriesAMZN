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
