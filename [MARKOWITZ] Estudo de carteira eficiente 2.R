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

retornos <- na.omit(ROC(prices))

portf <- portfolio.spec(colnames(retornos))

portf <- add.constraint(portf, type = 'weight_sum', min_sum = 0.99, max_sum = 1.00)
portf <- add.constraint(portf, type = 'transaction_cost', ptc = 0.001)
portf <- add.constraint(portf, type = 'box', min = .10, max = .40)
portf <- add.objective(portf, type = 'return', name = 'mean')
portf <- add.objective(portf, type = 'risk', name = 'StdDev', target = 0.005)

rp <- random_portfolios(portf, 1000, 'sample')
rebal <- optimize.portfolio.rebalancing(R = retornos, portfolio = portf, optimize_method = 'random',
                                        rp = rp, rebalance_on = 'months', training_period = 2,
                                        rolling_window = 10)
rebal


pesos_iguais <- rep(1 / ncol(retornos), ncol(retornos))
benchmark <- Return.portfolio(retornos, weights = pesos_iguais)
colnames(benchmark) <- c('benchmark')

chart.Weights(rebal, main = 'Rebalanceamento de Portfólio')

rebal_new <- extractWeights(rebal)
ret_new <- Return.portfolio(retornos, weights = rebal_new)

perf <- cbind(ret_new, benchmark)

charts.PerformanceSummary(perf)

















