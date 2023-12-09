# This Script was used to get the Price data on MSFT, so that we can enrich
# the data with News data and their sentiment scores via python script

library(quantmod)
MSFT <- getSymbols.yahoo("MSFT", from="2022-01-01", to="2023-12-01", periodicity = "daily", auto.assign=FALSE)[,6]
write.zoo(MSFT, "MSFT.csv", sep = ",", row.names = TRUE)
