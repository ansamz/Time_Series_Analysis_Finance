library(PerformanceAnalytics)
library(forecast)
library(rugarch)
library(vars)
library(tseries)
library(quantmod)
library(tinytex)
library(zoo)
library(ggpl)

MSFT <- getSymbols.yahoo("MSFT", from="2022-01-01", to="2023-12-01", periodicity = "daily", auto.assign=FALSE)[,6]

head(MSFT)

write.zoo(MSFT, "MSFT.csv", sep = ",", row.names = TRUE)
