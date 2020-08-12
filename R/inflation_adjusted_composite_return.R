source("R/inflation_rate.R")

# sp500: 1926/01/30-2019/12/31, monthly
sp <- read.csv("./datasets/sp500 returns.csv")
sp.return <- data.frame(Year = as.integer(substr(sp$caldt[-1],1,4)),
                        Month = as.integer(substr(sp$caldt[-1],5,6)),
                        Return = sp$sprtrn[-1])

# inflation rate
# CPI_rate: 1913-2020
# CPIC_rate: 1957-2020
# PPI_rate: 1913-2020

Inf_Adj <- function(return, inf){
  ratename <- colnames(inf)[3]
  measure <- str_remove(colnames(inf)[3], '_Rate')
  colnames(inf)[3] <- 'rate'
  total <- return %>%
    left_join(inf, by = c('Year', 'Month'))
  total <- total %>% 
    mutate(adj_Return = (1+Return)/(1+rate)-1)
  colnames(total)[4:5] <- c(ratename, paste0(measure, '_adjReturn'))
  return(total)
}

CPI_SP <- Inf_Adj(sp.return, CPI_rate)
CPIC_SP <- Inf_Adj(sp.return, CPIC_rate)
PPI_SP <- Inf_Adj(sp.return, PPI_rate)

adjusted_total <- CPI_SP %>%
  left_join(CPIC_SP, by = c('Year', 'Month', 'Return')) %>%
  left_join(PPI_SP, by = c('Year', 'Month', 'Return'))

# write.csv(adjusted_total, file="./datasets/Inflation_Adjusted_Composite_Return.csv", row.names = F)  

# Compare with Robert Shiller'e method: https://www.multpl.com/inflation-adjusted-s-p-500/table/by-month
