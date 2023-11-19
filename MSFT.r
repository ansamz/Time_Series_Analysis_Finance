install.packages("quantmod")

library(quantmod)

MSFT <- getSymbols.yahoo("MSFT", from="2022-01-01", to="2023-11-18", periodicity = "daily", auto.assign=FALSE)[,6]
MSFT
