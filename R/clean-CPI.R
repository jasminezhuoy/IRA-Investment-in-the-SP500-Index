library(readxl)
CPI <- read_xlsx("datasets/inflation-CPI.xlsx", skip = 11)
CPI_org <- data.frame()
for (i in CPI$Year){
  CPI_sub <- CPI[CPI$Year==i,]
  CPI_org_sub <- data.frame(Year = CPI$Year,
                            Month = names(CPI_sub)[2:13],
                            CPI = t(CPI_sub)[2:13])
  CPI_org <- rbind(CPI_org, CPI_org_sub)
}
write.csv(CPI_org, file = "datasets/Inflation-CPI_clean.csv", row.names = F)
