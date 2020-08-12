source("R/inflation_rate.R")

# sp500: 1926/01/30-2019/12/31, monthly
sp <- read.csv("./datasets/sp500 returns.csv")
sp <- sp[-1,1:3]
colnames(sp) <- c("Date", "GrossReturn", "PriceReturn")
sp.return <- data.frame(Year = as.integer(substr(sp$Date,1,4)),
                        Month = as.integer(substr(sp$Date,5,6)),
                        GrossReturn = sp$GrossReturn,
                        PriceReturn = sp$PriceReturn)

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
    mutate(adjusted_GR = (1+GrossReturn)/(1+rate)-1) %>% 
    mutate(adjusted_PR = (1+PriceReturn)/(1+rate)-1)
  colnames(total)[5:7] <- c(ratename, paste0(measure, '_adjusted_GR'),paste0(measure, '_adjusted_PR'))
  return(total)
}

CPI_SP <- Inf_Adj(sp.return, CPI_rate)
CPIC_SP <- Inf_Adj(sp.return, CPIC_rate)
PPI_SP <- Inf_Adj(sp.return, PPI_rate)

adjusted_total <- CPI_SP %>%
  left_join(CPIC_SP, by = c('Year', 'Month', 'GrossReturn', 'PriceReturn')) %>%
  left_join(PPI_SP, by = c('Year', 'Month', 'GrossReturn', 'PriceReturn'))

# write.csv(adjusted_total, file="./datasets/Inflation_Adjusted_Return.csv", row.names = F)  
  