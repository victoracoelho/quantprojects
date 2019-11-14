library(quantmod)

petr4 <- getSymbols("PETR4.SA", from = "2012-1-1", auto.assign = F)[,6]
petr4 <- na.omit(petr4)

u <- dailyReturn(petr4, type = 'log')

stdev <- sd(u)
t <- 1:10
mu <- mean(u)

n_sim <- 1000
total_sim <- matrix(0, nrow = 10, ncol = n_sim)
p_inicial <- 29.65

for(j in 1:n_sim){
  aux <- 2
  p <- p_inicial
  preco <- c(p)
  dist <- rnorm(9, 0, 1)
  for(i in dist){
    P = p + p * (mu/9 + stdev/sqrt(9)*i)
    preco[aux] <- P
    p = P
    aux = aux + 1
  }
  total_sim[,j] <- preco
}

matplot(total_sim, ylab = 'Preço', xlab = 'Tempo', type = 'l', col = 1:n_sim)

round(quantile(total_sim, probs = c(0.05, .25, .50, .75, 0.95)), 5)
