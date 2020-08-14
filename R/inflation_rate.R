library(dplyr)
library(lubridate)
library(stringr)

# CPI-U: https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0
# 1913 Jan - 2020 Jun, monthly
CPI_index <- read.csv("./datasets/inflation-CPI.csv")
CPI_index <- data.frame(Year = CPI_index$Year,
                        Month = as.integer(str_remove(CPI_index$Period,'M')),
                        CPI = CPI_index$Value)

# Core CPI: https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0L1E
# 1957 Jan - 2020 Jun, monthly
CPIC_index <- read.csv("./datasets/inflation-CPIC.csv")
CPIC_index <- data.frame(Year = CPIC_index$Year,
                        Month = as.integer(str_remove(CPIC_index$Period,'M')),
                        CPIC = CPIC_index$Value)

# PPI: https://fred.stlouisfed.org/series/M0448BUSM336NNBR
# 1913 Jan - 2020 Jun, monthly
PPI_index <- read.csv("./datasets/inflation-PPI.csv")
PPI_index <- data.frame(Year = year(as.Date(PPI_index$DATE)),
                        Month = month(as.Date(PPI_index$DATE)),
                        PPI = PPI_index[,2])

# PCE: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2#reqid=19&step=2&isuri=1&1921=survey
# 1959 Jan - 2020 Jun, monthly
PCE_index <- read.csv("./datasets/inflation-PCE.csv")
PCE_index <- data.frame(Year = year(as.Date(PCE_index$DATE)),
                        Month = month(as.Date(PCE_index$DATE)),
                        PCE = PCE_index[,2])

# calculate monthly inflation rate
## input data.frame should be (Year, Month, Index)
Inf_RatefromIndex <- function(inf){
  measure <- colnames(inf)[3]
  colnames(inf)[3] <- 'Index'
  n <- dim(inf)[1]
  InfRate <- cbind(inf[-1,], Index_pre = inf$Index[-n])
  InfRate <- InfRate %>% mutate(rate = (Index - Index_pre)/Index_pre)
  InfRate <- InfRate[,c(1,2,5)]
  colnames(InfRate)[3] <- paste0(measure, '_Rate')
  return(InfRate)
}

CPI_rate <- Inf_RatefromIndex(CPI_index)
CPIC_rate <- Inf_RatefromIndex(CPIC_index)
PPI_rate <- Inf_RatefromIndex(PPI_index)
PCE_rate <- Inf_RatefromIndex(PCE_index)
InfRate_total <- CPI_rate %>%
  left_join(CPIC_rate, by = c('Year', 'Month')) %>%
  left_join(PPI_rate, by = c('Year', 'Month')) %>%
  left_join(PCE_rate, by = c('Year', 'Month'))

# write.csv(InfRate_total, file="./datasets/Inflation_Rate.csv", row.names = F)
