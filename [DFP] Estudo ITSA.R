library(GetDFPData)
library(tidyverse)

gdfpd.search.company('itaúsa')

itsa <- "ITAÚSA - INVESTIMENTOS ITAÚ S.A."

itsa_data <- gdfpd.GetDFPData(name.companies = itsa, first.date = '2014-01-01', 
                 last.date = '2019-10-24', selected.data = 'DFP', max.levels = 5)


glimpse(itsa_data)

itsa_fc <- bind_rows(itsa_data$fr.cashflow.consolidated)
itsa_lucro <- data.frame(itsa_fc$ref.date ,itsa_fc$acc.desc, itsa_fc$acc.value)
















































