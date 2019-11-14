# CAPTAR RETORNO DA AÇÃO DESLOCADO COM O BATCHGETSYMBOLs
require(BatchGetSymbols)
require(xlsx)

ticker <- c('BTCUSD=x')

btc <- BatchGetSymbols(tickers = ticker, first.date = '2013-01-01', 
                       last.date = Sys.Date(), type.return = 'log', freq.data = 'daily')
btc.df <- bind_rows(btc$df.tickers)
btc.df <- data.frame(btc.df)


glimpse(btc)


com_macd <- MACD(x = btc.prices$`BTCUSD=X.Close`, nFast = 21, nSlow = 72, nSig = 34)
com_ifr <- RSI(price = btc.prices$`BTCUSD=X.Close`, n = 9)

traderule1 <- Lag(ifelse(com_macd$macd < com_macd$signal, -1, 1))
traderule2 <- ifelse(com_ifr > 60, 1, -1)
traderule3 <- ifelse(com_ifr < 30, -1, 1)

carteira <- ROC(btc.prices$`BTCUSD=X.Close`) * traderule1 * traderule2 * traderule3
carteira_performance <- carteira['2013-05-28/2019-10-25']
carteira_acumulada <- exp(cumsum(carteira_performance))-1
btc_buyhold <- exp(cumsum(dailyReturn(btc.prices$`BTCUSD=X.Close`, type = 'log')))
btc_buyhold <- btc_buyhold['2013-05-28/2019-10-25']

par(mfrow=c(1,2))
plot(carteira_acumulada)
plot(btc_buyhold)


btc_retorno <- dailyReturn(btc.prices$`BTCUSD=X.Close`, type = 'log')
btc_retorno <- btc_retorno['2013-05-28/2019-10-25']
retorno_deslocado <- btc.df$ret.closing.prices
retorno_deslocado <- data.frame(retorno_deslocado)
data <- btc.df$ref.date
retorno_deslocado <- data.frame(data, retorno_deslocado)
retorno_deslocado <- retorno_deslocado$retorno_deslocado[106:1779]


btc_pre.modelo <- data.frame(carteira_performance, btc_retorno)

btc_modelo <- lm(btc_pre.modelo$daily.returns ~ btc_pre.modelo$BTCUSD.X.Close)
summary(btc_modelo)
mean(btc_pre.modelo$daily.returns)



retorno_deslocado <- btc.df$ret.closing.prices
retorno_deslocado <- data.frame(retorno_deslocado)


write.table(btc_pre.modelo, file = 'btc_premodelo.xls', sep = '\t')

btc_pre.modelo$regressao <- -0.00014 + (btc_pre.modelo$daily.returns * 0.21298)
btc_pre.modelo$trade <- ifelse(btc_pre.modelo$regressao > btc_pre.modelo$daily.returns, 1, 0)
mean(btc_pre.modelo$trade)
























