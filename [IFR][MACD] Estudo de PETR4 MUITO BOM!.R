library(quantmod)
library(FinancialInstrument)
library(devtools)
library(quantstrat)
library(PerformanceAnalytics)

ticker <- c('PETR4.SA')

getSymbols(ticker, from = '2015-1-1', to = Sys.Date())
petr4 <- PETR4.SA

petr4$retorno <- na.omit(ROC(Cl(petr4$PETR4.SA.Close)))

petr4_macd <- MACD(x = petr4$PETR4.SA.Close, nSlow = 17, nFast = 72, nSig = 34)
petr4_ifr <- RSI(price = petr4$PETR4.SA.Close, n = 9)


traderule <- ifelse(Lag(petr4_macd$macd > petr4_macd$signal), 1, -1)
traderule2 <- ifelse(petr4_ifr > 60, -1, 1)

perf <- na.omit(merge(traderule *traderule2 * petr4$retorno, petr4$retorno))
colnames(perf) <- c('Backtest', 'Buy&Hold')

table.DownsideRisk(perf)

charts.PerformanceSummary(perf)





























