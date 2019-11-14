library(tseries)
library(rugarch)
library(quantmod)
library(FinTS)


ticker <- c("PETR4.SA")

petr4 <- getSymbols(ticker, from = '2018-1-1', to = '2019-10-30')
petr4 <- PETR4.SA

petr4$vol <- volatility(petr4$PETR4.SA.Close, calc = 'close', n = 5)
plot.ts(petr4$vol)

ArchTest(petr4$vol)
garch(na.omit(petr4$vol, grad = 'numerical', trace = F))

petr4.garch <- ugarchspec(variance.model = list(garchOrder = c(1,1)), 
                          mean.model = list(armaOrder = c(0,0)))

petr4.garch.fit <- ugarchfit(petr4.garch, data = na.omit(petr4$vol))

petr4.garch.forecast <- ugarchforecast(petr4.garch.fit, n.ahead = 20)
plot(petr4.garch.forecast)
