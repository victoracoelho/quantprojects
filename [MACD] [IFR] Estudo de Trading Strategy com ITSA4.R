library(quantmod)
library(tseries)
library(forecast)

ticker <- c('ITSA4.SA')

getSymbols(ticker, from = '2016-1-1', to = Sys.Date())


retorno <- dailyReturn(ITSA4.SA$ITSA4.SA.Close, type = 'log' )
retorno_acumulado <- retorno['2016-06-07/2019-11-05']


itsa_macd <- MACD(ITSA4.SA$ITSA4.SA.Close, nFast = 21, nSlow = 72, nSig = 34)
ifr <- RSI(price = ITSA4.SA$ITSA4.SA.Close, n = 9)
bandas <- BBands(HLC = ITSA4.SA$ITSA4.SA.Close)


traderule <- Lag(ifelse(itsa_macd$macd < itsa_macd$signal,-1, 1))
traderule2 <- ifelse(ifr > 60, -1, 1)
traderule3 <- ifelse(ifr < 30, 1, -1)
traderule4 <- ifelse(((bandas$up - bandas$dn) / bandas$mavg) > 0.20, -1, 1)

retorno7itsa <- ROC(ITSA4.SA$ITSA4.SA.Close) * traderule2 * traderule3 * traderule4

retorno7new <- retorno7itsa['2016-06-07/2019-11-05']
modeloperf <- exp(cumsum(retorno7new))-1
retorno_buyhold <- exp(cumsum(retorno_acumulado))-1

plot(modeloperf, col = 'green')
lines(retorno_buyhold, col = 'red')

#### PREVISÃO DO MODELO ####
modelo <- arima(retorno7new, order = c(1,1,14), include.mean = F)


resultado <- data.frame(cbind((retorno_acumulado), fitted(modelo)))
names(resultado) <- c("real", 'previsto')

performance <- sign(resultado$real) == sign(resultado$previsto)
table(performance)/dim(resultado)[1]

retorno.buyhold <- cumsum(resultado$real)*100
retorno_arima_train <- ifelse(sign(resultado$real)==sign(resultado$previsto),abs(resultado$real),-abs(resultado$real))
retorno_arima_train <- cumsum(retorno_arima_train)*100

plot(retorno.buyhold, type = 'l', col = 'red')
lines(retorno_arima_train, type = 'l', col = 'green')

previsao <- predict(resultado$previsto, 8)

################################# RASCUNHOS ################################################

fechamento <- ITSA4.SA$ITSA4.SA.Close['2015-06-08/2019-10-17']

modelolinear <- lm(retorno_acumulado ~ retorno7new)
summary(modelolinear)

modelo <- data.frame(retorno_acumulado, retorno7new)




