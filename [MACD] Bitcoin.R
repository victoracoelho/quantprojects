library(quantmod)
library(TTR)
library(PerformanceAnalytics)


tickers <- c('BTCUSD=X')

getSymbols(tickers, from = '2013-01-01', to = '2019-10-25')

btc.prices <- `BTCUSD=X`

#gráfico com os dados
chartSeries(`BTCUSD=X`)
addMACD(fast = 17, slow = 72, signal = 34, histogram = T)

#trabalhando com indicador técnico
BTC_macd <- MACD(BTC$`BTCUSD=X.Close`, nFast = 17, nSlow = 72, nSig = 34,percent = F)

#regra de trade
traderule <- Lag(ifelse(BTC_macd$macd < BTC_macd$signal, -1, 1))

#retornos do fechamento multiplicado pela regra de trade
retornosBTC <- ROC(BTC$`BTCUSD=X.Close`) * traderule
retornos_2019 <- retornosBTC['2019-01-01/2019-10-15']

#performance da carteira
BTC_perf <- exp(cumsum(retornos_2019$`BTCUSD=X.Close`))-1
plot(BTC_perf)









