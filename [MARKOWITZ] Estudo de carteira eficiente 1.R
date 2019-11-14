library(quadprog)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(IntroCompFinR)



tickers <- c('GRND3.SA', 'SGPS3.SA', 'POMO4.SA', 'MEAL3.SA', 'OIBR3.SA', 'VVAR3.SA')

prices <- NULL
for (ticker in tickers){
  prices <- cbind(prices, getSymbols(ticker, from = '2019-1-1', periodicity = 'daily', auto.assign = F)[,4])
}

portf <- na.omit(portf)


grnd_ret <- dailyReturn(portf$GRND3.SA.Close, type = 'log')
sgps_ret <- dailyReturn(portf$SGPS3.SA.Close, type = 'log')
pomo_ret <- dailyReturn(portf$POMO4.SA.Close, type = 'log')
meal_ret <- dailyReturn(portf$MEAL3.SA.Close, type = 'log')
oibr_ret <- dailyReturn(portf$OIBR3.SA.Close, type = 'log')
vvar_ret <- dailyReturn(portf$VVAR3.SA.Close, type = 'log')

retornos <- cbind(grnd_ret, sgps_ret, pomo_ret, meal_ret, oibr_ret, vvar_ret)
names(retornos) <- c('grnd', 'sgps', 'pomo', 'meal', 'oibr', 'vvar')

ret_medio <- rbind(mean(retornos$grnd), mean(retornos$sgps), mean(retornos$pomo), 
                   mean(retornos$meal), mean(retornos$oibr), mean(retornos$vvar))
row.names(ret_medio) <- c('grnd', 'sgps', 'pomo', 'meal', 'oibr', 'vvar')

matriz_cov <- cov(retornos)

rownames(matriz_cov) <- c('grnd', 'sgps', 'pomo', 'meal', 'oibr', 'vvar')
colnames(matriz_cov) <- c('grnd', 'sgps', 'pomo', 'meal', 'oibr', 'vvar')

selic <- 0.05
short_selling <- F

###### MODELANDO CARTEIRA EFICIENTE ######
carteira_ef <- tangency.portfolio(er = ret_medio, cov.mat = matriz_cov, risk.free = 0.0001, 
                                  shorts = short_selling)
carteira_ef

risco_min <- globalMin.portfolio(er = ret_medio, cov.mat = matriz_cov, shorts = short_selling)
risco_min

fronteira <- efficient.frontier(er = ret_medio, cov.mat = matriz_cov, nport = 100, shorts = short_selling)

plot(fronteira, col = 'blue')
tangente <- (carteira_ef$er - 0.00001) / carteira_ef$sd
abline(a = 0.00001, b = tangente, col = 'green')











