library(quantmod)
library(FinancialInstrument)
library(devtools)
library(quantstrat)
library(PerformanceAnalytics)

ticker <- c('ITUB4.SA')

getSymbols(ticker, from = '2015-1-1', to = Sys.Date())
itub4 <- ITUB4.SA

itub4$ret <- na.omit(ROC(itub4$ITUB4.SA.Close))

itub4_macd <- MACD(x = itub4$ITUB4.SA.Close, nSlow = 17, nFast = 72, nSig = 34)
itub4_ifr <- RSI(price = itub4$ITUB4.SA.Close, n = 9)
itub4_vol <- volatility(OHLC = itub4$ITUB4.SA.Close, calc = 'close', n = 50)
itub4_obv <- OBV(price = itub4$ITUB4.SA.Close, volume = itub4$ITUB4.SA.Volume)
itub4_cci <- CCI(HLC = itub4$ITUB4.SA.Close, n = 14)


traderule <- ifelse(Lag(itub4_macd$macd > itub4_macd$signal), 1, -1)
traderule2 <- ifelse(itub4_ifr > 60, -1, 1)
traderule3 <- ifelse(itub4_obv > mean(itub4_obv), -1, 1)
traderule4 <- ifelse(itub4_cci$cci > 50, -1, 1)
traderule5 <- ifelse(itub4_cci$cci < -50, -1, 1)


perf <- na.omit(merge(traderule * traderule2 *  itub4$ret, itub4$ret))
colnames(perf) <- c('Backtest', 'Buy&Hold')

table.DownsideRisk(perf)

charts.PerformanceSummary(perf)





























