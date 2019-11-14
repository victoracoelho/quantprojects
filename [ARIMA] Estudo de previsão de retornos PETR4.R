library(quantmod)
library(TSPred)
library(Rwave)
library(tseries)
library(forecast)


ticker <- c('PETR4.SA')
getSymbols(ticker, from = '2019-1-2', to = Sys.Date())
petr.treino <- PETR4.SA
petr.teste <- PETR4.SA

petr.treino <- na.omit(petr.treino)
petr.teste <- na.omit(petr.teste)

petr.teste.ret <- dailyReturn(petr.teste$PETR4.SA.Close, type = 'log')
petr.treino.ret <- dailyReturn(petr.treino$PETR4.SA.Close, type = 'log')
dim(petr.treino.ret)

adf.test(petr.treino.ret)
plot(petr.treino.ret)

petr_auto <- auto.arima(petr.treino.ret)
petr_arima <- arima(petr.treino.ret, order = c(1,1, 14), include.mean = F)


resultado.treino <- data.frame(cbind(as.ts(petr.treino.ret), fitted(petr_arima), fitted(petr_auto)))
names(resultado.treino) <- c("real", 'previsto', 'auto.previsto')
performance.treino <- sign(resultado.treino$real) == sign(resultado.treino$previsto)
table(performance.treino)/dim(resultado.treino)[1]

retorno.buyhold <- cumsum(resultado.treino$real)*100
plot(retorno.buyhold, type = 'l', col = 'red')

retorno_arima_train <- ifelse(sign(resultado.treino$real)==sign(resultado.treino$previsto),abs(resultado.treino$real),-abs(resultado.treino$real))
retorno_arima_train <- cumsum(retorno_arima_train)*100

retorno_auto_arima_train <- ifelse(sign(resultado.treino$real)==sign(resultado.treino$auto.previsto),
                                   abs(resultado.treino$real),-abs(resultado.treino$real))
retorno_auto_arima_train <- cumsum(retorno_auto_arima_train)*100

plot(retorno_arima_train, type = 'l', col = 'red', lwd = 2, ylim = c(-100, 400))
lines(retorno_auto_arima_train, type = 'l', col = 'blue', lwd = 2)
lines(retorno.buyhold, type = 'l', col = 'green')

petr.previsao <- forecast(petr_arima, 211)
PETR4.SA_previsao_auto <- forecast(petr_auto,210)

aux <- dim(resultado.treino)[1]
resultado <- data.frame(cbind(as.ts(petr.teste.ret), fitted(petr.previsao)))
names(resultado) <- c("Real","Previsto")
performance <- sign(resultado$Real)==sign(resultado$Previsto)
table(performance)/dim(resultado)[1]

retorno_BH_acumulado <- cumsum(resultado$Real)*100

retorno_arima <- ifelse(sign(resultado$Real)==sign(resultado$Previsto),abs(resultado$Real),-abs(resultado$Real))
retorno_arima <- cumsum(retorno_arima)*100

plot(retorno_arima, type = 'l', col = 'green')
lines(retorno_BH_acumulado, type = 'l', col = 'red')

previsao <- predict(retorno_arima, 9)
plot(previsao)









