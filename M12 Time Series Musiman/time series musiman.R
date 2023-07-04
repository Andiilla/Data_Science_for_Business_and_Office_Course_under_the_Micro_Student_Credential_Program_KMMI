library(tseries)
library(forecast)
library(lmtest)
library(normtest)
library(nortest)
setwd("C:/Users/adminstrator/Downloads/ANDI ILLA ERVIANI NENSI/KMMI DS02/M12 Time Series Musiman")
data <- read.csv("data.csv", sep = ";")
data
head(data)
tail(data)
Penumpang <- data$Zt
Bulan <- data$t
ts.plot(Penumpang)
adf.test(Penumpang)
BoxCox.lambda(Penumpang)
logP <- log(Penumpang)
logP
ts.plot(logP)
acf(logP, lag.max = 56)
par(mfrow=c(1,2))
acf(logP, lag.max = 70)
pacf(logP, lag.max = 70)


differencing1 <- diff(logP, differences = 1)
differencing1
ts.plot(differencing1)
differencing2 <- diff(differencing1, differences = 1)
differencing2
ts.plot(differencing2)
differencing3 <- diff(differencing2, differences = 1)
differencing3
ts.plot(differencing3)
differencing4 <- diff(differencing3, differences = 1)
differencing4
ts.plot(differencing4)

acf(differencing1, lag.max=70)
adf.test(differencing1)
diffnonmus_mus12 <- diff(differencing1, lag = 12)
diffnonmus_mus12
ts.plot(diffnonmus_mus12)
adf.test(diffnonmus_mus12)


par(mfrow=c(1,2))
acf(diffnonmus_mus12, lag.max = 70)
pacf(diffnonmus_mus12, lag.max = 70)


fit3 <- arima(logP, order = c(0,1,1), seasonal = list(order=c(0,1,1),period=12),method = "ML")
fit3
coeftest(fit3)
Box.test(fit3$residuals, type="Ljung")
shapiro.test(fit3$residuals)

sf.test(fit3$residuals)
forecasting<-forecast(logP, model=fit3, h=12)
forecasting
plot(forecasting, main = "Plot Hasil Peramalam")

