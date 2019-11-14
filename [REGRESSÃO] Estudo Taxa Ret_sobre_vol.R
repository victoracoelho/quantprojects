library(ggplot2)

ticker <- c("PETR4.SA")

getSymbols(ticker, from = '2012-1-1', to = Sys.Date())

petr4 <- PETR4.SA


petr4$return <- na.omit(dailyReturn(petr4$PETR4.SA.Close, type = 'arithmetic'))
petr4['return'] <- na.omit(petr4$return)

petr4_return <- na.omit(petr4$return)

petr4_price <- na.omit(petr4$PETR4.SA.Close)
petr4_vol <- volatility(OHLC = petr4_price, n = 5)

petr4_taxa <- petr4_return/Lag(petr4_vol)
petr4new <- petr4_taxa[100:600,]
petr4_taxa_acc <- exp(cumsum(Lag(petr4_taxa)))
petr4_ret_acc <- exp(cumprod(petr4_return))

plot(petr4_ret_acc)

df <- data.frame(petr4_return, petr4_taxa)

regressao <- lm(petr4_return ~ petr4_taxa)
plot(petr4_return ~ petr4_taxa)

petr4_price_new <- petr4_price[1000:1936,]
par(mfrow=c(1,1))
chart_Series(petr4_price_new)
chart_Series(petr4_taxa_acc)
graf <- ggplot(df, aes(df$return, df$return.1)) + geom_jitter(aes(x = df$return, 
                                                                  colour = 'retorno')) + geom_point(aes(x = df$return.1,
                                                                                                                 colour = 'taxa'))
graf


df <- na.omit(df)
write.table(df, file = 'taxa_retorno.xls', sep = "\t")
