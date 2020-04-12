require(quantmod)
require(PerformanceAnalytics)

getSymbols('PETR4.SA', from = '2015-1-1', to = Sys.Date())

price <- PETR4.SA$PETR4.SA.Close
price <- na.omit(price)
retorno <- ROC(Cl(PETR4.SA$PETR4.SA.Close))
retorno <- na.omit(retorno)

macd <- MACD(x = price, nSlow = 17, nFast = 72, nSig = 34)
ifr <- RSI(price = price, n = 9)
ifr <- na.omit(ifr)
ema9 <- EMA(price, n = 9)
sma20 <- SMA(price, n=20)
rel_ema_sma <- Lag(ema9 / sma20)



traderule1 <- ifelse(Lag(macd$macd > macd$signal), 1, 0)
traderule2 <- ifelse(ifr > mean(ifr), 1, 0)
traderule3 <- ifelse(price > sma20, 1, 0)
traderule4 <- ifelse(price > ema9, 1, 0)

perf <- na.omit(merge(traderule1 * traderule2 * traderule3 * traderule4 *  retorno, retorno))
colnames(perf) <- c('Backtest', 'Buy&Hold')

table.DownsideRisk(perf)

charts.PerformanceSummary(perf)



