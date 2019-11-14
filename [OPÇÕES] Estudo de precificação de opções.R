library(fOptions)


itubl358 <- GBSCharacteristics(TypeFlag = 'p', S = 35.45, X = 35.71, 
                               Time = 30/360, r = .05, sigma = .2534, b = 0)


GBSVolatility(price = itubl358$premium, TypeFlag = 'p', S = 35.45, X = 35.71, Time = 30/360, r = .05, b = 0)